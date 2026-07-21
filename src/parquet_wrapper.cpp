#if __cplusplus < 202002L
#error "parquet-fortran requires C++20 (Arrow/Parquet headers use std::span unconditionally, regardless of Arrow version). " \
	"Set FPM_CXXFLAGS to include -std=c++20 (see README.md) before running fpm build/test."
#endif

#include <arrow/api.h>
#include <arrow/array/array_decimal.h>
#include <arrow/array/concatenate.h>
#include <arrow/array/util.h>
#include <arrow/compute/api.h>
#include <arrow/compute/initialize.h>
#include <arrow/io/api.h>
#include <arrow/util/bit_util.h>
#include <arrow/util/byte_size.h>
#include <arrow/util/compression.h>
#include <arrow/util/decimal.h>
#include <arrow/util/float16.h>
#include <arrow/util/thread_pool.h>
#include <parquet/arrow/reader.h>
#include <parquet/arrow/schema.h>
#include <parquet/arrow/writer.h>
#include <parquet/parquet_version.h>

// Arrow's vendored copy of Howard Hinnant's date library (date.h only -- deliberately not
// datetime.h, whose tz.h part would drag in the timezone database): used solely by the
// test-only parquet_debug_civil_from_days/parquet_debug_days_from_civil hooks below to
// cross-validate src/parquet_temporal.f90's own pure-Fortran implementation of the same
// civil<->days algorithm against Arrow's.
#include <arrow/vendored/datetime/visibility.h>
#include <arrow/vendored/datetime/date.h>

#include <algorithm>
#include <atomic>
#include <cctype>
#include <cerrno>
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <ctime>
#include <cstring>
#include <memory>
#include <limits>
#include <mutex>
#include <sstream>
#include <stdexcept>
#include <string>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <vector>

// Neither ParquetReaderHandle nor ParquetWriterHandle's own data structures
// (column_cache, the fields/arrays/*_metadata vectors, defined below) are
// synchronized -- concurrent calls into the *same* handle from more than one
// thread race on them (e.g. two threads' unordered_map::emplace on
// column_cache). Each library-facing entry point obtains its handle through
// as_reader_handle/as_handle below, which return this guard instead of a raw
// pointer: it atomically claims `busy` for the duration of the call (RAII)
// and immediately aborts the process if another thread is already inside a
// call on the same handle, rather than silently racing. This intentionally
// does NOT forbid handing a reader/writer off between threads sequentially
// (only true overlap is rejected), and it does not make it safe/meaningful to
// call into one reader/writer from many threads at once for speed -- see the
// README's Thread safety section: each thread must still use its own
// independent instance for that. Declared outside the extern "C" block below
// because templates cannot be given C language linkage.
//
// Deliberately calls std::abort() here instead of throwing: this guard is
// meant to be hit from worker threads inside a caller's own !$omp/#pragma omp
// parallel region (that's the whole misuse case it exists to catch), and the
// OpenMP specification does not guarantee well-defined behavior for a C++
// exception that escapes a parallel region uncaught -- different compiler/
// OpenMP-runtime combinations are free to handle that differently. Aborting
// directly sidesteps that entirely: it is well-defined from any thread,
// inside or outside any parallel construct, on every platform.
template <typename Handle>
class ConcurrencyGuard
{
public:
	ConcurrencyGuard(Handle *handle, const char *what) : handle_(handle)
	{
		bool expected = false;
		if (!handle_->busy.compare_exchange_strong(expected, true))
		{ // GCOVR_EXCL_START -- same std::abort() gcov-loss mechanism as report_fatal_error's own GCOVR_EXCL comment
			std::fprintf(stderr,
				"parquet-fortran: concurrent access to a single %s detected: each thread must use "
				"its own independent parquet_reader/parquet_writer instance (see the README's Thread "
				"safety section) -- do not call into the same one from more than one thread at a time. "
				"Aborting.\n", what);
			std::fflush(stderr);
			std::abort();
		}
		// GCOVR_EXCL_STOP
	}

	~ConcurrencyGuard()
	{
		if (handle_) handle_->busy.store(false, std::memory_order_release);
	}

	ConcurrencyGuard(const ConcurrencyGuard &) = delete;
	ConcurrencyGuard &operator=(const ConcurrencyGuard &) = delete;

	operator Handle *() const { return handle_; }
	Handle *operator->() const { return handle_; }

	// Disarms the guard (its destructor becomes a no-op) and returns the raw
	// pointer, for close_parquet_reader/close_parquet_writer, which delete
	// the underlying handle themselves -- without this, the guard's
	// destructor would touch already-freed memory afterwards.
	Handle *release()
	{
		auto *p = handle_;
		handle_ = nullptr;
		return p;
	}

private:
	Handle *handle_;
};

// Used by eval_filter_clause (filter row-matching, further below) -- declared
// here, outside extern "C", since templates cannot have C language linkage
// (same reason read_list_primitive_row/ctype_name live between extern "C"
// blocks rather than inside one).
template <typename T>
static bool compare_op(const T &a, const T &b, const std::string &op)
{
	if (op == ">") return a > b;
	if (op == ">=") return a >= b;
	if (op == "<") return a < b;
	if (op == "<=") return a <= b;
	if (op == "==") return a == b;
	return a != b; // "/="; every other op string is rejected before this is ever called.
}

extern "C"
{
	// MAML parsing (parquet_metadata.f90) repeatedly grows arrays of derived
	// types that themselves have allocatable character components (e.g.
	// parquet_column_type, parquet_metadata_entry) via a "allocate a bigger
	// tmp, whole-array-assign the old contents into it, move_alloc" pattern.
	// That whole-array assignment requires the compiler to generate a deep
	// copy (allocating and copying each element's allocatable components),
	// and testing found this is not reliably safe under genuine concurrent
	// threads in this gfortran version -- even with -frecursive, which fixes
	// the (different, simpler) problem of non-recursive local variables not
	// being safely stack-allocated per thread. A recursive mutex serializes
	// the affected code paths (parquet_parse_maml_lines and friends, see
	// parquet_metadata.f90) so concurrent MAML parsing stays correct, at the
	// cost of not running in true parallel for that specific operation.
	// Recursive because these functions call each other (e.g.
	// parquet_parse_maml_lines calls parquet_metadata_append_entry): a plain
	// std::mutex would deadlock a single thread locking it twice.
	static std::recursive_mutex g_maml_mutex;

	// Acquires g_maml_mutex; see its own comment for why it must be recursive.
	void parquet_maml_lock()
	{
		g_maml_mutex.lock();
	}

	// Releases the mutex acquired by parquet_maml_lock.
	void parquet_maml_unlock()
	{
		g_maml_mutex.unlock();
	}

	// arrow::default_memory_pool() lazily constructs a process-wide singleton
	// on its first call. That first call is not safely reentrant in every
	// Arrow build (observed: "Internal error: cannot create default memory
	// pool" / SIGABRT when two OpenMP test threads both hit it as their very
	// first Arrow call at the same time -- test-drive runs tests within a
	// suite concurrently, see run_testsuite in testdrive.F90). Calling this
	// once, single-threaded, before any concurrent work starts forces the
	// singleton to already exist by the time multiple threads use it.
	//
	// The same class of race applies to Arrow's other lazily-constructed
	// process-wide singletons: the global CPU thread pool
	// (arrow::internal::GetCpuThreadPool(), used by SetCpuThreadPoolCapacity
	// and multi-threaded reads) and each compression codec's first-use
	// initialization (arrow::util::Codec::Create(), which for some codecs
	// touches lazily-initialized library state, e.g. zlib). On machines with
	// very high core counts, test-drive's concurrent-within-a-suite
	// scheduling can start dozens of threads at once, several of which may
	// hit one of these first-call paths simultaneously and abort with
	// "terminate called recursively". Touch all of them here, single
	// threaded, before any concurrent work starts.
	void parquet_warmup_memory_pool()
	{
		arrow::default_memory_pool();
		arrow::internal::GetCpuThreadPool();

		for (auto codec_type : {arrow::Compression::SNAPPY, arrow::Compression::GZIP,
			arrow::Compression::ZSTD, arrow::Compression::BROTLI, arrow::Compression::LZ4_FRAME})
		{
			if (!arrow::util::Codec::IsAvailable(codec_type)) continue;
			auto result = arrow::util::Codec::Create(codec_type);
			(void)result;
		}
	}

	struct ColumnMetadata
	{
		std::string name;
		std::string unit;
		std::string description;
		std::string ucd;
		std::string data_type;
		int64_t array_size;
		int64_t col_size;
	};

	struct TableMetadataEntry
	{
		std::string key;
		std::string value;
		std::string description;
	};

	struct ParquetWriterHandle
	{
		std::shared_ptr<arrow::io::FileOutputStream> outfile;
		std::vector<std::shared_ptr<arrow::Field>> fields;
		std::vector<std::shared_ptr<arrow::Array>> arrays;
		std::vector<ColumnMetadata> column_metadata;
		std::vector<TableMetadataEntry> table_metadata;
		arrow::Compression::type compression_codec = arrow::Compression::SNAPPY;
		int compression_level = arrow::util::kUseDefaultCompressionLevel;
		int64_t chunk_size = -1; // <= 0 means "not set by the caller": auto-sized at close time from the final row count.
		bool use_threads = true; // per-writer opt-out of Arrow's internal thread pool; see parquet_open_writer(..., use_threads=).
		std::atomic<bool> busy{false}; // guards against two threads calling into the same writer at once; see ConcurrencyGuard.

		// --- Streaming row-group API state (parquet_new_row_group/parquet_write_column_chunk/
		// parquet_finish_row_group) -- unused (left at these defaults) by a writer that only
		// ever uses parquet_write_column; see resolve_chunk_size/close_parquet_writer for how
		// the two paths coexist. ---
		int64_t resolved_chunk_size = -1; // Cached by resolve_chunk_size on first need; -1 = not yet resolved.
		// The lower-level, row-group-oriented FileWriter (distinct from the WriteTable-based
		// batch path) -- lazily opened at the *first* parquet_finish_row_group call, once every
		// column appearing in that (first) row group is known, since Parquet's file-level schema
		// must be fixed before any row group can be written. Null until then; once non-null, the
		// batch (WriteTable) close path is no longer used -- see close_parquet_writer.
		std::unique_ptr<parquet::arrow::FileWriter> row_group_writer;
		bool in_row_group = false; // true between parquet_new_row_group and its matching parquet_finish_row_group.
		int64_t current_row_group_nrows = 0; // The `nrows` given to the currently-open parquet_new_row_group call.
		// Running total of rows covered by every *finished* row group so far -- also the slice
		// offset into any whole (parquet_write_column-populated) column for the next row group.
		int64_t streamed_rows_total = 0;
		// One entry per column already touched by parquet_write_column_chunk for the
		// currently-open row group (built fresh, as a small array covering just this row
		// group's rows, by append_typed_column_chunk_impl -- never a slice of a larger array).
		// Cleared by parquet_new_row_group, consumed and cleared again by
		// parquet_finish_row_group.
		std::unordered_map<int, std::shared_ptr<arrow::Array>> pending_chunk_arrays;
	};

	// One column's read-time QC declaration, parsed on the Fortran side
	// (parquet_parse_qc_maml, parquet_metadata.f90) from a qc-maml's fields:
	// entries -- min_raw/max_raw are deliberately raw text, parsed against
	// this column's *actual* Arrow type only when a check actually runs
	// (run_qc_range_check), the same "don't trust a declared data_type"
	// convention parquet_reader_set_filter already uses for filter values.
	struct QcRule
	{
		bool has_min = false;
		std::string min_op; // ">", ">=", "<", or "<="
		std::string min_raw;
		bool has_max = false;
		std::string max_op;
		std::string max_raw;
		bool null_values_allowed = false; // true only if the maml's qc: miss: was Null/NA (case-insensitive)
	};

	// Deliberately does NOT hold a materialized arrow::Table: opening a file
	// only parses the (small) footer/schema via FileReaderBuilder, so no
	// column's data is ever read from disk until that specific column is
	// actually requested (see get_single_chunk_array). column_cache holds
	// each column already read this way, keyed by its schema field index, so
	// asking for the same column twice (e.g. parquet_get_col_size followed
	// by parquet_read_column) doesn't re-read it from disk.
	struct ParquetReaderHandle
	{
		std::unique_ptr<parquet::arrow::FileReader> reader;
		std::shared_ptr<arrow::Schema> schema;
		// Bridges arrow::Schema field paths to Parquet's own flat leaf-column indices -- built
		// once at open time (create_parquet_reader). Needed because ReadRowGroup/ReadTable's
		// `column_indices` parameter is indexed against the flat physical leaf schema (unlike
		// ReadColumn's `i`, which is a top-level arrow::Schema field index and can read a whole
		// struct in one call) -- these coincide only when every top-level field contributes
		// exactly one leaf (true for every file before struct columns existed, which is why nothing
		// needed this before). See resolve_single_leaf_index/collect_leaf_indices, below.
		parquet::arrow::SchemaManifest manifest;
		std::string filename;
		int64_t nrows = 0; // effective row count: equal to total_nrows until a filter narrows it (see parquet_reader_set_filter).
		int64_t total_nrows = 0; // the file's true, unfiltered row count -- kept for parquet_reader_print_stat's "of N total".
		std::unordered_map<int, std::shared_ptr<arrow::Array>> column_cache;
		// Flat key-value table metadata (parquet_add_table_metadata's own
		// key/value pairs, as written into the Arrow schema's KeyValueMetadata
		// by build_file_metadata -- NOT re-parsed from the embedded VOTable
		// XML). Populated once by create_parquet_reader, so parquet_get_metadata
		// (parquet_metadata.f90) never re-reads the file: every call just
		// scans this in-memory copy.
		std::vector<std::pair<std::string, std::string>> table_metadata_cache;
		// Set once by parquet_reader_set_filter: a plain (never-null) boolean
		// mask, one entry per row of the *unfiltered* file, true for rows that
		// pass every filter clause. get_single_chunk_array and
		// parquet_reader_prefetch_columns both apply this (via arrow::compute::Filter)
		// to every column right after decoding it, so every column ever handed
		// back to Fortran -- and every column_cache entry -- reflects only the
		// matching rows, transparently, once a filter is set.
		std::shared_ptr<arrow::BooleanArray> filter_mask;
		// Per-column filter clauses retained solely for parquet_reader_print_stat's
		// "filter" column: each entry is one clause's operator+value with the
		// column name stripped (e.g. ">=0.0"), in the order set_filter saw them.
		std::unordered_map<int, std::vector<std::string>> filter_clauses;
		// Access bookkeeping for parquet_reader_print_stat only: was_prefetched
		// is set for every column index named in a parquet_reader_prefetch_columns
		// call (whether or not it actually triggered a read that time -- see
		// parquet_reader_prefetch_columns); was_read is set by mark_read/
		// mark_read_string, called from every typed parquet_read_* entry point,
		// so it reflects an actual Fortran-side read call, not just caching.
		// output_type_used/output_str_len_used record the most recent Fortran
		// output type (and, for strings, output buffer length) used to read
		// that column, for the same reporting purpose.
		std::unordered_set<int> was_prefetched;
		std::unordered_set<int> was_read;
		std::unordered_map<int, std::string> output_type_used;
		std::unordered_map<int, int64_t> output_str_len_used;
		// Read-time QC (see parquet_reader_set_qc, called from parquet_open_reader
		// when a qc-maml is supplied and qc is enabled): qc_rules holds one
		// entry per column the maml declared bounds/miss: for AND that also
		// exists in this file (columns named in the maml but absent from the
		// file are silently ignored -- see parquet_reader_set_qc). qc_enabled
		// is false whenever no qc-maml was supplied, or qc was explicitly
		// turned off; every check below is a no-op in that case. qc_soft
		// picks the failure mode when a violation is found: soft (true) prints
		// a WARNING and continues; hard (false, the default) aborts the
		// process via report_fatal_error -- the same class of clean,
		// stderr-diagnosed abort as the read-side Null/type-mismatch checks.
		// qc_null_warned/qc_range_warned record which columns have already
		// printed their (at most one each) Null-presence/range-violation
		// WARNING in soft mode, so repeated reads of the same column during
		// the reader's lifetime don't spam the same warning again (moot in
		// hard mode, which aborts on the first violation) -- see run_qc_checks.
		// All three are keyed by the exact column path string (a plain name, or
		// a dotted struct-leaf path) rather than a physical column index: two
		// different struct leaves can share one physical top-level index, and
		// an int-keyed map would let a second leaf's rule silently clobber the
		// first's (see run_qc_checks's own comment).
		bool qc_enabled = false;
		bool qc_soft = false;
		std::unordered_map<std::string, QcRule> qc_rules;
		std::unordered_set<std::string> qc_null_warned;
		std::unordered_set<std::string> qc_range_warned;
		// Row-group-chunked read support (parquet_read_column_chunk / parquet_get_num_row_groups /
		// parquet_get_chunk_size(reader,...) -- see get_row_group_chunk_array). num_row_groups is
		// the file's row-group count, read once from the footer at open time
		// (create_parquet_reader) -- unlike nrows/total_nrows this is never affected by filtering,
		// since a filtered reader disallows chunk reads entirely (get_row_group_chunk_array).
		// chunk_read_row_groups tracks, per column schema-field-index, which 1-based row groups
		// have been read via the chunk API -- used only by parquet_reader_check_complete, called
		// from parquet_close_reader(check_complete=.true.).
		int64_t num_row_groups = 0;
		std::unordered_map<int, std::unordered_set<int64_t>> chunk_read_row_groups;
		// Pins the most recent array handed out (by pointer, not value) by
		// parquet_read_string_column_chunk_buffers, whose row-group-scoped array (from
		// get_row_group_chunk_array) is otherwise never retained anywhere -- unlike a whole-column
		// read's array, which column_cache already keeps alive for the reader's whole lifetime.
		// Fortran (parquet_read.f90) always consumes the returned buffer pointers immediately
		// (append_buffers), before any other call on this same reader, so a single slot -- next
		// overwritten by the next such call -- is sufficient; it does not need per-column tracking.
		std::shared_ptr<arrow::Array> last_chunk_buffers_array;
		std::atomic<bool> busy{false}; // guards against two threads calling into the same reader at once; see ConcurrencyGuard.
	};

	// Wraps a raw writer handle in a ConcurrencyGuard for the duration of one extern "C" call.
	static ConcurrencyGuard<ParquetWriterHandle> as_handle(void *handle)
	{
		return ConcurrencyGuard<ParquetWriterHandle>(static_cast<ParquetWriterHandle *>(handle), "parquet_writer");
	}

	// Wraps a raw reader handle in a ConcurrencyGuard for the duration of one extern "C" call.
	static ConcurrencyGuard<ParquetReaderHandle> as_reader_handle(void *handle)
	{
		return ConcurrencyGuard<ParquetReaderHandle>(static_cast<ParquetReaderHandle *>(handle), "parquet_reader");
	}

	// Prints a diagnostic and aborts, the same way ConcurrencyGuard does. Used
	// at specific extern "C" entry points (file-open, and reading a column
	// whose physical Parquet type this library doesn't support) to turn a
	// C++ exception into a clean, well-defined process abort with a message
	// on stderr, instead of an uncaught exception reaching std::terminate()
	// with compiler/runtime-dependent output. Most other throw sites in this
	// file are deliberately left as plain, uncaught std::runtime_errors --
	// see the README's Error handling section: this project only guarantees
	// a clean error at these specific, documented boundaries.
	//
	// GCOVR_EXCL'd (this function's body, plus every one of its ~112 call sites elsewhere in
	// this file -- see .gitlab-ci.yml's --exclude-lines-by-pattern for the single-line ones):
	// std::abort() skips the atexit-registered gcov-flush handler a normal process exit relies
	// on, so any process that reaches this function loses that whole run's coverage data --
	// unobservable by gcov no matter how well-tested, not merely hard to trigger.
	[[noreturn]] static void report_fatal_error(const char *context, const std::string &message) // GCOVR_EXCL_START
	{
		std::fprintf(stderr, "parquet-fortran: %s: %s\n", context, message.c_str());
		std::fflush(stderr);
		std::abort();
	}
	// GCOVR_EXCL_STOP

	// Returns the schema field index of `name`, or throws if it isn't a column.
	static int64_t get_column_index(const ParquetReaderHandle *reader_handle, const char *name)
	{
		auto idx = reader_handle->schema->GetFieldIndex(name);
		if (idx < 0)
		{ // GCOVR_EXCL_START -- dead: parquet_read.f90's check_column_exists (built on the
		  // non-throwing parquet_reader_has_column/struct_path_exists probe) gates every read
		  // entry point before this can be reached.
			throw std::runtime_error(std::string("Column not found: ") + name);
		}
		// GCOVR_EXCL_STOP
		return static_cast<int64_t>(idx);
	} // GCOVR_EXCL_LINE -- gcov attribution artifact: this closing brace shows uncovered even though the covered `return` above proves the body ran.

	// Describes how a (possibly dotted) column name resolves against the schema: either an exact
	// top-level field match (child_path empty), or a walk through nested STRUCT fields down to a
	// leaf. Schema-only -- never reads any column data, so this is cheap enough to call from
	// existence checks (parquet_reader_has_column) as well as before an actual read.
	struct StructPathInfo
	{
		std::string top_level_name;
		std::vector<std::string> child_path; // empty => `top_level_name` is the whole story
		std::shared_ptr<arrow::Field> leaf_field; // the resolved leaf's schema-level field
	};

	// Resolves `name` against `schema`, walking a dotted path through nested STRUCT fields if it
	// isn't itself a literal top-level field name (exact match always wins, so an existing column
	// literally named with a "." in it is unaffected). Every non-terminal path segment must be a
	// STRUCT field; the terminal segment must resolve to something other than STRUCT/LIST/
	// LARGE_LIST/MAP -- a FIXED_SIZE_LIST or scalar leaf is fine. Struct-of-struct nesting to any
	// depth is supported, but a path stopping at an intermediate struct, or passing through/
	// landing on a MAP or (variable-length) LIST, is not (see CLAUDE.md's nested-struct-field
	// design notes). Throws std::runtime_error, same as get_column_index, on any failure --
	// callers that need a non-throwing probe should use struct_path_exists instead.
	static StructPathInfo resolve_struct_path(const std::shared_ptr<arrow::Schema> &schema, const std::string &name)
	{
		auto direct_idx = schema->GetFieldIndex(name);
		if (direct_idx >= 0)
		{
			return StructPathInfo{name, {}, schema->field(direct_idx)};
		}

		std::vector<std::string> segments;
		size_t start = 0;
		while (true)
		{
			auto dot = name.find('.', start);
			segments.push_back(name.substr(start, dot == std::string::npos ? std::string::npos : dot - start));
			if (dot == std::string::npos) break;
			start = dot + 1;
		}
		// The four throws below are all dead, same proof as get_column_index's own: parquet_read.f90's
		// check_column_exists (built on the non-throwing parquet_reader_has_column/struct_path_exists
		// probe, which mirrors this function's own walk) gates every read entry point on the exact
		// same (possibly dotted) `name` before this can ever be reached.
		if (segments.size() < 2)
		{ // GCOVR_EXCL_START -- dead, see comment above.
			throw std::runtime_error(std::string("Column not found: ") + name);
		}
		// GCOVR_EXCL_STOP

		auto top_idx = schema->GetFieldIndex(segments[0]);
		if (top_idx < 0)
		{ // GCOVR_EXCL_START -- dead, see comment above.
			throw std::runtime_error(std::string("Column not found: ") + name);
		}
		// GCOVR_EXCL_STOP

		std::shared_ptr<arrow::Field> field = schema->field(top_idx);
		std::string walked_so_far = segments[0];
		for (size_t i = 1; i < segments.size(); ++i)
		{
			if (field->type()->id() != arrow::Type::STRUCT)
			{ // GCOVR_EXCL_START -- dead, see comment above.
				throw std::runtime_error(std::string("Column not found: ") + name + " (path segment '" + walked_so_far +
					"' is not a struct, found type: " + field->type()->ToString() + ")");
			}
			// GCOVR_EXCL_STOP
			auto struct_type = std::static_pointer_cast<arrow::StructType>(field->type());
			auto child_field = struct_type->GetFieldByName(segments[i]);
			if (!child_field)
			{ // GCOVR_EXCL_START -- dead, see comment above.
				throw std::runtime_error(std::string("Column not found: ") + name + " (no field '" + segments[i] +
					"' under '" + walked_so_far + "')");
			}
			// GCOVR_EXCL_STOP
			field = child_field;
			walked_so_far += "." + segments[i];
		}

		auto leaf_id = field->type()->id();
		if (leaf_id == arrow::Type::STRUCT || leaf_id == arrow::Type::LIST ||
			leaf_id == arrow::Type::LARGE_LIST || leaf_id == arrow::Type::MAP)
		{ // GCOVR_EXCL_START -- dead, see comment above.
			throw std::runtime_error(std::string("Column not found: ") + name +
				" (resolves to a " + field->type()->ToString() +
				" column; struct paths must resolve to a leaf scalar/vector column, and MAP/LIST are not supported)");
		}
		// GCOVR_EXCL_STOP

		return StructPathInfo{segments[0], std::vector<std::string>(segments.begin() + 1, segments.end()), field};
	}

	// Non-throwing existence probe for a (possibly dotted) column path -- used by
	// parquet_reader_has_column/parquet_reader_set_qc/parquet_reader_set_filter so a caller can
	// validate names up front without an uncaught exception crossing the Fortran/C++ boundary.
	// Deliberately a standalone, exception-free walk mirroring resolve_struct_path's logic --
	// NOT "try { resolve_struct_path(...); return true; } catch (...) { return false; }" -- a
	// throw from resolve_struct_path here was empirically found to escape uncaught even through
	// an enclosing try/catch in the same translation unit (reproduced with
	// test_add_col_qc_roundtrip's intentionally-absent "extra_dummy" qc column, which reaches
	// exactly this call), some mismatch in how this project's mixed gfortran-driven link handles
	// C++ exception unwinding across the static library boundary. Keep this independent of
	// resolve_struct_path rather than reintroducing a try/catch around it.
	static bool struct_path_exists(const std::shared_ptr<arrow::Schema> &schema, const std::string &name)
	{
		if (schema->GetFieldIndex(name) >= 0) return true;

		std::vector<std::string> segments;
		size_t start = 0;
		while (true)
		{
			auto dot = name.find('.', start);
			segments.push_back(name.substr(start, dot == std::string::npos ? std::string::npos : dot - start));
			if (dot == std::string::npos) break;
			start = dot + 1;
		}
		if (segments.size() < 2) return false;

		auto top_idx = schema->GetFieldIndex(segments[0]);
		if (top_idx < 0) return false;

		std::shared_ptr<arrow::Field> field = schema->field(top_idx);
		for (size_t i = 1; i < segments.size(); ++i)
		{
			if (field->type()->id() != arrow::Type::STRUCT) return false;
			auto struct_type = std::static_pointer_cast<arrow::StructType>(field->type());
			auto child_field = struct_type->GetFieldByName(segments[i]);
			if (!child_field) return false;
			field = child_field;
		}

		auto leaf_id = field->type()->id();
		return leaf_id != arrow::Type::STRUCT && leaf_id != arrow::Type::LIST &&
			leaf_id != arrow::Type::LARGE_LIST && leaf_id != arrow::Type::MAP;
	}

	// Walks `child_path` through nested STRUCT fields of `root` (the already-read top-level
	// struct array), combining every hop's own validity bit -- root's own, each intermediate
	// struct's, and the final leaf's -- into one mask: a row is null in the result if the
	// top-level struct was null there, any intermediate struct field was null there, or the leaf
	// itself was null there (see CLAUDE.md's nested-struct-field design notes -- the same
	// "combine independently" principle already used for FixedSizeList's outer/inner nulls,
	// generalized from one level of list-nesting to N levels of struct-nesting). Returns a
	// freshly materialized Array sharing the leaf's own value buffers but with that combined mask
	// as its validity bitmap, so every existing column-consuming function (convert_values_to_*,
	// qc, filter, array row/element mode, print_stat's stat computation) sees an ordinary Array
	// and needs no struct-specific handling of its own. `root`/`child_path` are assumed already
	// validated against the schema by resolve_struct_path -- this does no error-checking itself.
	static std::shared_ptr<arrow::Array> unwrap_struct_path(
		const std::shared_ptr<arrow::Array> &root, const std::vector<std::string> &child_path)
	{
		int64_t n = root->length();
		std::vector<bool> combined_valid(static_cast<size_t>(n));
		for (int64_t i = 0; i < n; ++i) combined_valid[static_cast<size_t>(i)] = root->IsValid(i);

		std::shared_ptr<arrow::Array> current = root;
		for (const auto &segment : child_path)
		{
			auto struct_arr = std::static_pointer_cast<arrow::StructArray>(current);
			current = struct_arr->GetFieldByName(segment);
			for (int64_t i = 0; i < n; ++i)
			{
				if (combined_valid[static_cast<size_t>(i)] && !current->IsValid(i))
				{
					combined_valid[static_cast<size_t>(i)] = false;
				}
			}
		}

		int64_t base_offset = current->data()->offset;
		auto alloc = arrow::AllocateBitmap(base_offset + n);
		if (!alloc.ok())
		{ // GCOVR_EXCL_START -- real allocation-failure backstop, not fixture-triggerable
			throw std::runtime_error(std::string("Failed to allocate combined-validity bitmap: ") + alloc.status().ToString());
		}
		// GCOVR_EXCL_STOP
		auto buffer = alloc.ValueOrDie();
		for (int64_t i = 0; i < n; ++i)
		{
			arrow::bit_util::SetBitTo(buffer->mutable_data(), base_offset + i, combined_valid[static_cast<size_t>(i)]);
		}
		auto new_data = current->data()->Copy();
		new_data->buffers[0] = buffer;
		new_data->null_count = arrow::kUnknownNullCount;
		return arrow::MakeArray(new_data);
	}

	// Appends every leaf Parquet column index found under `field` (depth-first, left to right) --
	// for a plain scalar/FIXED_SIZE_LIST arrow field this is exactly one index; for a struct this
	// recurses through every descendant. A FIXED_SIZE_LIST/LIST field is itself never a leaf in
	// parquet::arrow::SchemaManifest's tree (Parquet's own 2/3-level list encoding always wraps
	// the actual value in at least one synthetic child node, e.g. "spectrum" -> "element") --
	// recursing through `.children` handles that uniformly alongside genuine STRUCT nesting,
	// with no special-casing needed here for list-typed fields.
	static void collect_leaf_indices(const parquet::arrow::SchemaField &field, std::vector<int> &out)
	{
		if (field.is_leaf())
		{
			out.push_back(field.column_index);
			return;
		}
		for (const auto &child : field.children)
		{
			collect_leaf_indices(child, out);
		}
	}

	// Resolves the single Parquet leaf column index reached by walking `child_path` (by field
	// name) down from top-level field `top_level_idx`'s manifest entry, then descending any
	// further single-child wrapper nodes (a FIXED_SIZE_LIST/LIST's own internal encoding) until a
	// leaf is reached. Assumes `child_path` was already validated against the schema by
	// resolve_struct_path -- every name lookup here is expected to succeed. Needed for
	// ReadRowGroup/ReadTable's `column_indices`, which (unlike ReadColumn's plain top-level field
	// index) is indexed against Parquet's flat leaf schema -- see ParquetReaderHandle::manifest's
	// own comment for why this coincides with `top_level_idx` alone only for a childless field.
	static int64_t resolve_single_leaf_index(
		const ParquetReaderHandle *reader_handle, int top_level_idx, const std::vector<std::string> &child_path)
	{
		const parquet::arrow::SchemaField *current = &reader_handle->manifest.schema_fields[top_level_idx];
		for (const auto &segment : child_path)
		{
			const parquet::arrow::SchemaField *next = nullptr;
			for (const auto &child : current->children)
			{
				if (child.field->name() == segment)
				{
					next = &child;
					break;
				}
			}
			current = next;
		}
		while (!current->is_leaf())
		{
			current = &current->children[0];
		}
		return current->column_index;
	}

	// A column chunk read via FileReader::ReadColumn can still be split
	// across several Arrow chunks if the file has multiple row groups (see
	// the `chunk_size` writer option) -- collapse those into one contiguous
	// array here, same as the whole-table CombineChunks this replaced used
	// to do, just scoped to one column instead of the entire file.
	static std::shared_ptr<arrow::Array> combine_column_chunks(const std::shared_ptr<arrow::ChunkedArray> &chunked, const std::string &name)
	{
		if (chunked->num_chunks() == 1)
		{
			return chunked->chunk(0);
		}
		// GCOVR_EXCL_START -- untested compat backstop, confirmed unreachable through this
		// library's current read paths, not just untested: a temporary instrumented build (a
		// counter printed on every call) showed every one of 400+ combine_column_chunks calls
		// across the full test suite -- including a dedicated probe against a 3-row-group
		// (chunk_size=2, 5 rows) file's whole-column read -- always sees exactly 1 chunk. Arrow's
		// FileReader::ReadColumn/ReadTable evidently always coalesces every row group into a
		// single chunk in this Arrow version, regardless of row-group count. Kept rather than
		// deleted: a genuine second-level safeguard against a future Arrow version (or a
		// different reader configuration) not coalescing this way, not dead code to remove --
		// same reasoning as the pattern already used for genuinely file-I/O-triggered backstops
		// elsewhere in this file.
		if (chunked->num_chunks() == 0)
		{
			auto empty = arrow::MakeEmptyArray(chunked->type(), arrow::default_memory_pool());
			if (!empty.ok())
			{
				throw std::runtime_error(std::string("Failed to build empty array for column: ") + name);
			}
			return empty.ValueOrDie();
		}
		auto combined = arrow::Concatenate(chunked->chunks(), arrow::default_memory_pool());
		if (!combined.ok())
		{
			throw std::runtime_error(std::string("Failed to combine chunks for column: ") + name + ": " + combined.status().ToString());
		}
		return combined.ValueOrDie();
		// GCOVR_EXCL_STOP
	} // GCOVR_EXCL_LINE -- gcov attribution artifact under GCC: this closing brace shows uncovered
	// even though the function's other, always-taken return path (chunked->chunk(0) above) proves
	// it demonstrably runs -- same category as this file's other documented closing-brace
	// attribution artifacts.

	// Arrow's compute kernels (MinMax, Filter, ...) live in a separate
	// registry from core arrow/parquet and are only usable once explicitly
	// registered -- done lazily here (only print_stat/filtering need them),
	// once per process via std::call_once, since the underlying registry is
	// process-global and not safe to register into concurrently.
	static void ensure_compute_initialized()
	{
		static std::once_flag compute_init_flag;
		std::call_once(compute_init_flag, []() { auto st = arrow::compute::Initialize(); (void)st; });
	}

	// arrow::compute::Filter (Arrow 24) has no "array_filter" kernel for arrow::Type::STRING_VIEW
	// at all -- confirmed via NotImplementedError ("Function 'array_filter' has no kernel
	// matching input types (string_view, bool)"). Since every read-side call site already treats
	// STRING_VIEW identically to STRING/LARGE_STRING via is_string_like_type/
	// make_string_like_accessor, working around this Arrow gap by casting to arrow::large_utf8()
	// first (rather than teaching every filter call site about STRING_VIEW specifically) is
	// lossless for every consumer -- the one visible side effect is that a STRING_VIEW column's
	// reported parquet_type in parquet_reader_print_stat becomes "large_string" once a filter is
	// active on the reader (parquet_type reflects the column_cache's actual decoded type, not
	// the original file schema's). Returns `array` unchanged for every other type.
	static arrow::Result<std::shared_ptr<arrow::Array>> coerce_for_filter_kernel(const std::shared_ptr<arrow::Array> &array)
	{
		if (array->type_id() != arrow::Type::STRING_VIEW) return array;
		ARROW_ASSIGN_OR_RAISE(auto cast_datum, arrow::compute::Cast(array, arrow::large_utf8()));
		return cast_datum.make_array();
	}

	// Applies a reader's filter_mask (if set -- see parquet_reader_set_filter)
	// to a just-decoded column array, keeping only the rows that pass every
	// filter clause. A no-op (returns `array` unchanged) if no filter is set.
	// Called from every place a column is first decoded from disk
	// (get_single_chunk_array, parquet_reader_prefetch_columns), so every
	// column ever cached or handed back to Fortran reflects only the
	// matching rows once a filter is in effect.
	static std::shared_ptr<arrow::Array> apply_filter_mask(ParquetReaderHandle *reader_handle, const std::shared_ptr<arrow::Array> &array)
	{
		if (!reader_handle->filter_mask) return array;
		ensure_compute_initialized();
		auto coerced = coerce_for_filter_kernel(array);
		if (!coerced.ok())
		{ // GCOVR_EXCL_START -- Cast-kernel Status backstop on already-validated input
			throw std::runtime_error(coerced.status().ToString());
		}
		// GCOVR_EXCL_STOP
		auto filtered = arrow::compute::Filter(coerced.ValueOrDie(), reader_handle->filter_mask);
		if (!filtered.ok())
		{ // GCOVR_EXCL_START -- Filter-kernel Status backstop on already-validated input
			throw std::runtime_error(filtered.status().ToString());
		}
		// GCOVR_EXCL_STOP
		return filtered.ValueOrDie().make_array();
	}

	// Test-only: forces get_single_chunk_array (below) to abort via report_fatal_error the next
	// time it would actually issue a whole-column ReadColumn call (a cache hit is unaffected --
	// see get_single_chunk_array's own comment). Lets
	// test/error_scenarios.f90's scenario_col_size_and_row_mode_avoid_whole_column_read prove,
	// on a tiny fixture, that parquet_reader_get_column_col_size/
	// parquet_reader_get_column_total_elements/read_list_primitive_row (backing
	// parquet_get_col_size/parquet_get_column_total_elements/parquet_read_array_row_mode) never
	// take the whole-column path for a FIXED_SIZE_LIST column -- the actual fix for the "List
	// index overflow" crash these functions used to hit once nrows * col_size exceeded int32 (see
	// CLAUDE.md's "Guarding a hard Arrow int32-only ceiling"). Same process-global/subprocess-
	// isolation reasoning as g_debug_string_offset_limit, above: safe only because the scenario
	// that flips it runs as its own isolated subprocess.
	static bool g_debug_force_whole_column_read_error = false;

	// Test-only: counts every genuine disk ReadColumn call get_single_chunk_array issues (a cache
	// miss) -- never incremented on a cache hit. Lets test/error_scenarios.f90's
	// scenario_nested_struct_shares_cached_read prove that reading two different leaf paths under
	// the same top-level struct column (e.g. "main.id" then "main.inner.age") triggers exactly one
	// real disk read of "main" -- i.e. that struct-path resolution shares get_single_chunk_array's
	// existing column_cache instead of re-reading per leaf path.
	static int64_t g_debug_physical_column_read_count = 0;

	// Reads (and caches) exactly one column's data from disk -- every other
	// column in the file is never touched, regardless of how many columns
	// the file has or how large they are. This is what makes reading a
	// large file with many columns, but only asking for a few of them,
	// cheap: nothing beyond the footer/schema is read until this is called.
	// `name` may be a dotted struct-field path (see resolve_struct_path):
	// the top-level struct column is read/cached exactly as any other column
	// would be (keyed by its own physical index, same as always -- so
	// reading two different leaves under one struct still only reads that
	// struct's data from disk once), then unwrap_struct_path walks down to
	// the requested leaf, so every caller downstream of this function
	// (conversion, qc, filter, array row/element mode, print_stat) keeps
	// seeing an ordinary Array and needs no struct-specific handling.
	static std::shared_ptr<arrow::Array> get_single_chunk_array(ParquetReaderHandle *reader_handle, const char *name)
	{
		auto resolved = resolve_struct_path(reader_handle->schema, name);
		auto idx = get_column_index(reader_handle, resolved.top_level_name.c_str());
		auto cached = reader_handle->column_cache.find(static_cast<int>(idx));
		std::shared_ptr<arrow::Array> array;
		if (cached != reader_handle->column_cache.end())
		{
			array = cached->second;
		}
		else
		{
			if (g_debug_force_whole_column_read_error)
			{
				report_fatal_error("get_single_chunk_array",
					std::string("forced debug error: whole-column read attempted for column: ") + name); // GCOVR_EXCL_LINE
			}

			std::shared_ptr<arrow::ChunkedArray> chunked;
			auto status = reader_handle->reader->ReadColumn(static_cast<int>(idx), &chunked);
			if (!status.ok())
			{ // GCOVR_EXCL_START -- file-I/O backstop, not fixture-triggerable
				throw std::runtime_error(status.ToString());
			}
			// GCOVR_EXCL_STOP

			array = apply_filter_mask(reader_handle, combine_column_chunks(chunked, resolved.top_level_name));
			reader_handle->column_cache.emplace(static_cast<int>(idx), array);
			++g_debug_physical_column_read_count;
		}

		if (!resolved.child_path.empty())
		{
			array = unwrap_struct_path(array, resolved.child_path);
		}
		return array;
	}

	// Flattens a fixed-size-list/list column's array down to its element
	// values, so null-count/min-max/qc checks run over every element in
	// every row, as one column-wide figure -- the same "flatten across the
	// whole vector column" scope get_uniform_list_values gives an actual
	// read, just without the nrows/col_size shape checks (callers here only
	// ever see an already-cached, already-validated array). Non-list arrays
	// are returned unchanged.
	static std::shared_ptr<arrow::Array> flatten_for_stats(const std::shared_ptr<arrow::Array> &array)
	{
		if (array->type_id() == arrow::Type::FIXED_SIZE_LIST)
		{
			return std::static_pointer_cast<arrow::FixedSizeListArray>(array)->values();
		}
		if (array->type_id() == arrow::Type::LIST)
		{
			return std::static_pointer_cast<arrow::ListArray>(array)->values();
		}
		return array;
	}

	// True for any Arrow string representation this library's read paths can decode via
	// make_string_like_accessor's GetView/IsNull interface -- STRING (int32 offsets, the
	// default this library writes), LARGE_STRING (int64 offsets, used only for a column whose
	// own byte payload would overflow STRING's 2^31-1 limit -- see
	// would_overflow_string_offset_limit and parquet_append_string_column/
	// parquet_append_string_array_column), and STRING_VIEW (Arrow's view-array representation:
	// inlined short values plus out-of-line data buffers for longer ones). This library's own
	// writer never produces STRING_VIEW -- it can only arrive from a Parquet file written by
	// another Arrow-based tool whose stored Arrow schema (arrow_writer_builder.store_schema())
	// declared the column as utf8_view(), which this library's reader then reconstructs
	// faithfully rather than coercing to STRING/LARGE_STRING. A file written with the large
	// variant round-trips back as exactly that type on read (same store_schema mechanism) -- so
	// every STRING-only check on the read side must also accept LARGE_STRING and STRING_VIEW
	// indefinitely, not just while writing a new large column. NOTE: is_string_like_type is the
	// *value-accessor* gate (GetView/IsNull, one value at a time) -- the separate buffer-level
	// fast path (extract_string_buffers, offsets+data+validity handed straight to
	// parquet_strings.f90's append_buffers) does NOT support STRING_VIEW, since a view array has
	// no offsets buffer at all; its call sites use is_offset_string_type instead, below.
	static bool is_string_like_type(arrow::Type::type type_id)
	{
		return type_id == arrow::Type::STRING || type_id == arrow::Type::LARGE_STRING ||
			type_id == arrow::Type::STRING_VIEW;
	}

	// Narrower than is_string_like_type: true only for the two offset-based representations
	// (STRING/LARGE_STRING) that extract_string_buffers knows how to decode. STRING_VIEW has no
	// offsets/data buffer pair to expose this way (its values are inlined or held in separate
	// variadic data buffers) -- callers needing the buffer-level fast path check this instead of
	// is_string_like_type, and reject STRING_VIEW with a clear error rather than misreading it.
	static bool is_offset_string_type(arrow::Type::type type_id)
	{
		return type_id == arrow::Type::STRING || type_id == arrow::Type::LARGE_STRING;
	}

	// Uniform (length/IsNull/GetView) accessor over a STRING, LARGE_STRING, or STRING_VIEW
	// array, erasing the otherwise-unrelated-at-compile-time arrow::StringArray/
	// arrow::LargeStringArray/arrow::StringViewArray distinction so read-side call sites need
	// one code path instead of three near-identical ones. `array`'s type_id() must satisfy
	// is_string_like_type -- callers are expected to have already checked/branched on that
	// themselves (so their own error message can name the actual type).
	struct StringLikeAccessor
	{
		std::function<bool(int64_t)> is_null;
		std::function<std::string_view(int64_t)> get_view;
		int64_t length = 0;
	};

	static StringLikeAccessor make_string_like_accessor(const std::shared_ptr<arrow::Array> &array)
	{
		if (array->type_id() == arrow::Type::LARGE_STRING)
		{
			auto arr = std::static_pointer_cast<arrow::LargeStringArray>(array);
			return StringLikeAccessor{
				[arr](int64_t i) { return arr->IsNull(i); },
				[arr](int64_t i) { return arr->GetView(i); },
				arr->length()};
		}
		if (array->type_id() == arrow::Type::STRING_VIEW)
		{
			auto arr = std::static_pointer_cast<arrow::StringViewArray>(array);
			return StringLikeAccessor{
				[arr](int64_t i) { return arr->IsNull(i); },
				[arr](int64_t i) { return arr->GetView(i); },
				arr->length()};
		}
		auto arr = std::static_pointer_cast<arrow::StringArray>(array);
		return StringLikeAccessor{
			[arr](int64_t i) { return arr->IsNull(i); },
			[arr](int64_t i) { return arr->GetView(i); },
			arr->length()};
	}

	// Buffer-level counterpart to make_string_like_accessor: exposes a STRING/LARGE_STRING
	// array's own offsets/data/validity buffers directly, for handing straight to
	// parquet_strings.f90's append_buffers instead of copying one string at a time via GetView.
	// `array`'s type_id() must satisfy is_offset_string_type (STRING/LARGE_STRING only --
	// STRING_VIEW is not supported here, see is_offset_string_type's own comment).
	// offsets_int32_out is set to 1 for a plain STRING array (int32
	// offsets) or 0 for LARGE_STRING (int64 offsets) -- append_buffers accepts both.
	//
	// Relies on `array`'s own offset() being 0: every caller of this reaches `array` via
	// combine_column_chunks (either directly, for a whole-column read, or through
	// get_row_group_chunk_array, for a chunked read), which only ever returns a freshly
	// ReadColumn/ReadRowGroup-decoded chunk or the result of arrow::Concatenate -- never a
	// genuine Slice() -- so offset() is 0 in every reachable case today. This is deliberately not
	// re-verified/rebased here: doing so correctly would need an O(nrows) copy of the offsets
	// buffer (rebasing every entry), which isn't worth paying for a case that cannot currently
	// occur. If that ever changes, parquet_strings.f90's append_buffers has its own precondition
	// guard (offsets(1) must be 0) that aborts loudly instead of silently misplacing bytes --
	// see its doc comment. data_out is still correctly rebased by raw_value_offsets()[0] below (a
	// single pointer add, always correct and free to do regardless); only the offsets values
	// themselves and the validity bitmap rely on the offset()==0 assumption.
	static void extract_string_buffers(const std::shared_ptr<arrow::Array> &array,
		int64_t *nrows_out, int64_t *nchars_out,
		const void **offsets_out, const void **data_out, const void **validity_out,
		int8_t *offsets_int32_out)
	{
		bool is_large = array->type_id() == arrow::Type::LARGE_STRING;
		*offsets_int32_out = is_large ? 0 : 1;
		*nrows_out = array->length();
		if (is_large)
		{
			auto arr = std::static_pointer_cast<arrow::LargeStringArray>(array);
			*nchars_out = arr->total_values_length();
			*offsets_out = arr->raw_value_offsets();
			*data_out = arr->raw_data() + (arr->length() > 0 ? arr->raw_value_offsets()[0] : 0);
		}
		else
		{
			auto arr = std::static_pointer_cast<arrow::StringArray>(array);
			*nchars_out = arr->total_values_length();
			*offsets_out = arr->raw_value_offsets();
			*data_out = arr->raw_data() + (arr->length() > 0 ? arr->raw_value_offsets()[0] : 0);
		}
		*validity_out = array->null_bitmap_data();
	}

	// Arrow's real limit for a plain STRING array: its offsets buffer is int32, capping total
	// value bytes at 2^31-1 for one array/column. parquet_append_string_column/
	// parquet_append_string_array_column check a column's projected byte total against this
	// (or, under test, g_debug_string_offset_limit -- see
	// parquet_debug_set_string_offset_limit) before building it, switching to
	// arrow::large_utf8() instead of arrow::utf8() when it would overflow.
	static constexpr int64_t kArrowInt32OffsetLimit = 2147483647; // 2^31 - 1

	// Test-only override of kArrowInt32OffsetLimit -- see parquet_debug_set_string_offset_limit,
	// further below, for why this is a process-global rather than scoped to one writer (short
	// version: parquet_writer%handle is a private component of the parquet Fortran module, so no
	// test-only Fortran hook can reach a specific writer's handle from outside that module; a
	// global is the only thing reachable, made safe by running the one test that touches it as
	// an isolated subprocess -- see test/error_scenarios.f90's scenario_large_utf8_roundtrip).
	// <= 0 (the default) means "use the real production limit".
	static int64_t g_debug_string_offset_limit = -1;

	// True if `n_values` string entries of up to `item_len` bytes each might overflow `limit`
	// once built as a flat STRING/LARGE_STRING array. Uses item_len (the declared max length)
	// as a safe upper bound on actual (trimmed) value bytes, so this can never *under*-estimate
	// and miss a real overflow. Guards the multiplication itself against overflowing int64
	// rather than computing n_values*item_len directly.
	static bool would_overflow_string_offset_limit(int64_t n_values, int64_t item_len, int64_t limit)
	{
		if (item_len <= 0 || n_values <= 0) return false;
		if (n_values > limit / item_len) return true;
		return n_values * item_len > limit;
	}

	// Arrow's real limit for a vector column's per-row width: arrow::FixedSizeListBuilder and
	// arrow::fixed_size_list() both take `list_size` as a plain int32_t. Unlike the string byte-offset
	// limit above, there is no "large" fixed-size-list variant to auto-upgrade to -- this is a hard
	// Arrow architectural ceiling, not something this library can work around. append_typed_column and
	// parquet_append_string_array_column check col_size against this (or, under test,
	// g_debug_col_size_limit -- see parquet_debug_set_col_size_limit) before ever casting it to
	// int32_t, so an oversized col_size fails cleanly instead of silently wrapping into a garbage
	// list_size and corrupting the written column.
	static constexpr int64_t kArrowInt32ListSizeLimit = 2147483647; // 2^31 - 1

	// Test-only override of kArrowInt32ListSizeLimit -- see parquet_debug_set_col_size_limit, further
	// below, for why this is a process-global (same reasoning as g_debug_string_offset_limit, above).
	// <= 0 (the default) means "use the real production limit".
	static int64_t g_debug_col_size_limit = -1;

	// Aborts (via report_fatal_error) if `col_size` exceeds Arrow's FixedSizeListType limit --
	// see kArrowInt32ListSizeLimit, above. Called before any of the col_size > 1 vector-column
	// write paths cast col_size to int32_t.
	static void check_col_size_fits_arrow_limit(int64_t col_size, const std::string &name, const char *context)
	{
		int64_t limit = g_debug_col_size_limit > 0 ? g_debug_col_size_limit : kArrowInt32ListSizeLimit;
		if (col_size > limit)
		{
			report_fatal_error(context, "column '" + name + "': col_size (" + std::to_string(col_size) +
				") exceeds " + std::to_string(kArrowInt32ListSizeLimit) + // GCOVR_EXCL_LINE
				", the maximum vector-column width Arrow's FixedSizeListType supports"); // GCOVR_EXCL_LINE
		}
	}

	// Arrow/Parquet's real limit on a vector column's flattened element count *per row group*
	// (row_group_rows * col_size), separate from the col_size-alone ceiling above. Parquet's own
	// repetition/definition-level generation for list-typed columns (level_conversion.cc) walks
	// every flattened element of a row group with a plain int32_t counter, so once
	// row_group_rows * col_size exceeds 2^31-1 that counter overflows and Arrow throws
	// IOError("List index overflow") from deep inside parquet::arrow::WriteTable during
	// close_parquet_writer. Known upstream limitation, not fixed by choosing large_utf8/large_list
	// on the write side (see apache/arrow#33188 / ARROW-17983). Unlike the string byte-offset
	// limit, there is no "large" list variant to auto-upgrade to -- this is a hard ceiling, but
	// crucially it is scoped to one row group's element count, not the whole file's: WriteTable's
	// own row-group splitting already keeps an arbitrarily large *total* column within this limit
	// as long as each individual row group stays under it (confirmed empirically -- a
	// multi-billion-element FixedSizeListArray writes successfully split across small-enough row
	// groups). So this is enforced in close_parquet_writer, once the actual row-group size
	// (effective_chunk_size) is known -- see there for how the auto-sized and explicit-chunk_size
	// cases differ.
	static constexpr int64_t kArrowInt32ListElementCountLimit = 2147483647; // 2^31 - 1

	// Test-only override of kArrowInt32ListElementCountLimit -- see
	// parquet_debug_set_list_element_count_limit, further below, for why this is a process-global
	// (same reasoning as g_debug_string_offset_limit, above). <= 0 (the default) means "use the
	// real production limit".
	static int64_t g_debug_list_element_count_limit = -1;

	// Returns the widest col_size among `fields`' FIXED_SIZE_LIST columns (0 if none) -- used by
	// close_parquet_writer to size/validate effective_chunk_size against
	// kArrowInt32ListElementCountLimit.
	static int64_t max_fixed_size_list_col_size(const std::vector<std::shared_ptr<arrow::Field>> &fields)
	{
		int64_t max_col_size = 0;
		for (const auto &field : fields)
		{
			if (field->type()->id() != arrow::Type::FIXED_SIZE_LIST) continue;
			auto col_size = static_cast<int64_t>(std::static_pointer_cast<arrow::FixedSizeListType>(field->type())->list_size());
			if (col_size > max_col_size) max_col_size = col_size;
		}
		return max_col_size;
	}

	// Core of check_explicit_chunk_size_fits_arrow_limit / check_chunk_size_fits_metadata_limit,
	// below -- both need only a (name, col_size) view of one column, whether it comes from an
	// already-built arrow::Field (the WriteTable/batch path and the streaming path once its
	// schema is locked -- see ParquetWriterHandle::fields) or a still-schema-only ColumnMetadata
	// entry (resolving a row-group size before any data exists at all -- see resolve_chunk_size).
	// Aborts (via report_fatal_error) if `chunk_size` -- an *explicit*, caller-chosen row-group
	// size, from parquet_open_writer(..., chunk_size=)/parquet_set_writer_options -- combined
	// with `col_size` would exceed kArrowInt32ListElementCountLimit. Only ever called for an
	// explicit chunk_size: the auto-sized path (chunk_size <= 0) instead silently clamps its own
	// computed value down to whatever is safe, since nothing was explicitly requested to
	// silently deviate from -- see close_parquet_writer/estimate_chunk_size_from_schema. Guards
	// the chunk_size * col_size multiplication itself against overflowing int64 (the same way
	// would_overflow_string_offset_limit does) rather than computing it directly.
	static void check_chunk_size_fits_limit_for_col_size(int64_t chunk_size, const std::string &name,
		int64_t col_size, const char *context, const char *value_label = "chunk_size",
		const char *advice = "pass a smaller chunk_size to parquet_open_writer/parquet_set_writer_options, or "
			"omit it to auto-size safely")
	{
		if (col_size <= 1) return;
		int64_t limit = g_debug_list_element_count_limit > 0 ? g_debug_list_element_count_limit : kArrowInt32ListElementCountLimit;
		bool overflows = chunk_size > limit / col_size || chunk_size * col_size > limit;
		if (overflows)
		{
			report_fatal_error(context, "column '" + name + "': " + value_label + " (" + std::to_string(chunk_size) +
				") * col_size (" + std::to_string(col_size) + ") exceeds " + std::to_string(kArrowInt32ListElementCountLimit) + // GCOVR_EXCL_LINE
				", the maximum per-row-group element count Arrow/Parquet's list-column level generation supports " // GCOVR_EXCL_LINE
				"-- " + advice); // GCOVR_EXCL_LINE
		}
	}

	// Field-based (WriteTable/batch path, and the streaming path once every column's
	// arrow::Field is known) form of check_chunk_size_fits_limit_for_col_size, above -- checks
	// every FIXED_SIZE_LIST column in `fields`.
	static void check_explicit_chunk_size_fits_arrow_limit(int64_t chunk_size,
		const std::vector<std::shared_ptr<arrow::Field>> &fields, const char *context)
	{
		for (const auto &field : fields)
		{
			if (field->type()->id() != arrow::Type::FIXED_SIZE_LIST) continue;
			auto col_size = static_cast<int64_t>(std::static_pointer_cast<arrow::FixedSizeListType>(field->type())->list_size());
			check_chunk_size_fits_limit_for_col_size(chunk_size, field->name(), col_size, context);
		}
	}

	// ColumnMetadata-based (schema-declared, before any data exists) form of
	// check_chunk_size_fits_limit_for_col_size, above -- used by resolve_chunk_size to validate
	// an explicit chunk_size against every column a schema already declares, without needing any
	// column's arrow::Field to exist yet.
	static void check_chunk_size_fits_metadata_limit(int64_t chunk_size,
		const std::vector<ColumnMetadata> &column_metadata, const char *context, const char *value_label = "chunk_size",
		const char *advice = "pass a smaller chunk_size to parquet_open_writer/parquet_set_writer_options, or "
			"omit it to auto-size safely")
	{
		for (const auto &col : column_metadata)
		{
			check_chunk_size_fits_limit_for_col_size(chunk_size, col.name, col.col_size, context, value_label, advice);
		}
	}

	// Row-group-size byte-target policy, shared by close_parquet_writer's actual-table-bytes
	// computation (writers that never use the streaming row-group API) and
	// estimate_chunk_size_from_schema's schema-only estimate, below (writers that do). Targets a
	// row group size in BYTES rather than a flat row count -- a flat row-count cap doesn't know
	// how wide a row is: for a handful of int32 columns, a few hundred thousand rows might be a
	// few MB, while for a table with several vector columns (large col_size) the same row count
	// could be gigabytes -- sized this way, both end up with row groups in the same ballpark of
	// actual bytes, which is what Parquet's own row-group size guidance (roughly 128MB-1GB) is
	// actually about, and what drives per-row-group compression efficiency and decode cost.
	// kMinAutoChunkSizeRows/kMaxAutoChunkSizeRows bound the result so pathological row widths
	// still produce something reasonable: an extremely wide row (e.g. a huge vector column) is
	// floored so a table isn't fragmented into an absurd number of tiny row groups, and an
	// extremely narrow row is capped so a huge table doesn't collapse into one single, enormous
	// row group either. Callers separately apply any further caps afterward (a known num_rows,
	// the int32 vector-column ceiling via max_fixed_size_list_col_size/estimate_chunk_size_from_
	// schema) -- this function only implements the core byte-target arithmetic.
	static constexpr int64_t kTargetRowGroupBytes = 256LL * 1024 * 1024; // ~256 MiB
	static constexpr int64_t kMinAutoChunkSizeRows = 1000;
	// 10,000,000: high enough that the byte target above governs for any realistically-shaped
	// table (the row-count cap only starts to bind below ~27 bytes/row -- e.g. a single narrow
	// column), while still backstopping genuinely pathological cases (a handful of bytes per row
	// at billions of rows) from collapsing into one giant row group spanning the whole file.
	static constexpr int64_t kMaxAutoChunkSizeRows = 10000000;
	// The kMinAutoChunkSizeRows floor exists to avoid fragmenting a table into an excessive
	// number of tiny row groups when rows are moderately wide -- but blindly applying it
	// regardless of row width defeats the whole point of sizing by bytes: if a single row is
	// already close to (or bigger than) kTargetRowGroupBytes (e.g. a vector column with a very
	// large col_size), forcing kMinAutoChunkSizeRows rows into one row group would produce a row
	// group many times the intended size. kMaxFloorOvershootFactor bounds how far the floor is
	// allowed to push things past the target before it's abandoned in favor of a
	// smaller-than-floor (down to 1 row) row group instead -- an under-sized row group is a much
	// smaller problem than one that is unboundedly oversized.
	static constexpr double kMaxFloorOvershootFactor = 4.0;

	static int64_t chunk_size_from_bytes_per_row(double bytes_per_row)
	{
		auto rows_for_target = static_cast<int64_t>(
			static_cast<double>(kTargetRowGroupBytes) / std::max(bytes_per_row, 1.0));

		if (rows_for_target >= kMinAutoChunkSizeRows)
		{
			return std::min<int64_t>(rows_for_target, kMaxAutoChunkSizeRows);
		}
		if (static_cast<double>(kMinAutoChunkSizeRows) * bytes_per_row
			<= static_cast<double>(kTargetRowGroupBytes) * kMaxFloorOvershootFactor)
		{
			// Floor overshoots the target, but only by a bounded, acceptable amount -- apply it
			// as usual.
			return kMinAutoChunkSizeRows;
		}
		// Even the floor would blow far past the target (rows this wide): accept a
		// smaller-than-floor row group (down to 1 row) instead of a wildly oversized one.
		return std::max<int64_t>(rows_for_target, 1);
	}

	// Per-element byte-width estimate for a MAML-declared data_type, used only to estimate a
	// row-group size from schema alone (no actual data yet) -- see
	// estimate_chunk_size_from_schema. A conservative (never-under) estimate is what matters
	// here, not exactness: overestimating bytes-per-row only makes the resulting row groups
	// smaller/more numerous than strictly necessary, never larger than the byte target intends.
	// "string" uses array_size (the MAML-declared max length) as its upper bound, the same
	// convention would_overflow_string_offset_limit already uses for the string byte-offset
	// ceiling.
	static int64_t estimated_bytes_per_element(const std::string &data_type, int64_t array_size)
	{
		if (data_type == "int32" || data_type == "float32") return 4;
		if (data_type == "int64" || data_type == "float64") return 8;
		if (data_type == "boolean") return 1; // Arrow bit-packs booleans; 1 byte/element over-estimates safely.
		if (data_type == "string") return array_size > 0 ? array_size : 1;
		return 8; // Unrecognized data_type shouldn't happen (schema parsing validates this
		          // elsewhere) -- a conservative fallback rather than a hard failure here, since
		          // this is only an estimate, never the source of correctness.
	}

	// Estimates a row-group size (see chunk_size_from_bytes_per_row) purely from
	// `column_metadata`'s declared types/col_size/array_size -- no actual data needed. Used to
	// give parquet_get_chunk_size(writer) a usable answer before any column has been written,
	// and to lock in a starting row-group size the moment the streaming row-group API
	// (parquet_new_row_group) is first used, since by then data may only exist one row group at
	// a time rather than as one fully-built table the way close_parquet_writer's own,
	// more-accurate actual-bytes computation needs -- see resolve_chunk_size. Also applies the
	// int32 vector-column clamp directly (via max_col_size, computed inline below since
	// ColumnMetadata's col_size doesn't need max_fixed_size_list_col_size's arrow::Field
	// unwrapping), since a schema-based estimate must already be safe the moment it's handed
	// out -- unlike close_parquet_writer's own computation, there is no later opportunity to
	// re-clamp before the first row group is built.
	static int64_t estimate_chunk_size_from_schema(const std::vector<ColumnMetadata> &column_metadata)
	{
		double bytes_per_row = 0;
		int64_t max_col_size = 0;
		for (const auto &col : column_metadata)
		{
			auto col_size = col.col_size > 0 ? col.col_size : 1;
			bytes_per_row += static_cast<double>(estimated_bytes_per_element(col.data_type, col.array_size)) *
				static_cast<double>(col_size);
			if (col_size > max_col_size) max_col_size = col_size;
		}

		int64_t effective_chunk_size = bytes_per_row > 0 ? chunk_size_from_bytes_per_row(bytes_per_row) : kMaxAutoChunkSizeRows;
		if (effective_chunk_size < 1) effective_chunk_size = 1;

		if (max_col_size > 1)
		{
			int64_t limit = g_debug_list_element_count_limit > 0 ? g_debug_list_element_count_limit : kArrowInt32ListElementCountLimit;
			effective_chunk_size = std::min(effective_chunk_size, std::max<int64_t>(limit / max_col_size, 1));
		}
		return effective_chunk_size;
	}

	// Resolves and CACHES writer_handle->resolved_chunk_size on first call -- the authoritative
	// row-group size for parquet_get_chunk_size(writer), queried before any data has been
	// written, and the advisory starting point for the streaming row-group API
	// (parquet_new_row_group's own `nrows` argument is what actually determines each row
	// group's size; this is only ever a suggestion the caller's loop can choose to follow).
	// Once cached, always returns the same value -- recomputing later would be pointless, since
	// nothing about this schema-only estimate improves with time the way close_parquet_writer's
	// own actual-table-bytes computation does for a writer that never streams at all (see there
	// -- that path is entirely separate and unaffected by this one).
	static int64_t resolve_chunk_size(ParquetWriterHandle *writer_handle)
	{
		if (writer_handle->resolved_chunk_size > 0) return writer_handle->resolved_chunk_size;

		if (writer_handle->chunk_size > 0)
		{
			check_chunk_size_fits_metadata_limit(writer_handle->chunk_size, writer_handle->column_metadata,
				"parquet_get_chunk_size");
			writer_handle->resolved_chunk_size = writer_handle->chunk_size;
		}
		else
		{
			writer_handle->resolved_chunk_size = estimate_chunk_size_from_schema(writer_handle->column_metadata);
		}
		return writer_handle->resolved_chunk_size;
	}

	// Arrow's real limit for a table's column count: arrow::Schema::num_fields()/GetFieldIndex()
	// both return a plain int32_t internally (static_cast<int>(fields_.size())) -- unlike row
	// count (int64_t throughout) there is no "large" variant for field count at all. Past this
	// many columns, that cast would silently wrap rather than error, corrupting every subsequent
	// field-count/field-index lookup (used pervasively by this file's own append_column,
	// get_column_index, print_stat, ...) instead of failing cleanly. append_column and
	// parquet_add_column_metadata check the column count against this (or, under test,
	// g_debug_column_count_limit -- see parquet_debug_set_column_count_limit) before it can ever
	// grow past it.
	static constexpr int64_t kArrowInt32FieldCountLimit = 2147483647; // 2^31 - 1

	// Test-only override of kArrowInt32FieldCountLimit -- see parquet_debug_set_column_count_limit,
	// further below, for why this is a process-global (same reasoning as g_debug_string_offset_limit,
	// above). <= 0 (the default) means "use the real production limit".
	static int64_t g_debug_column_count_limit = -1;

	// Aborts (via report_fatal_error) if adding one more column would make the table's column
	// count exceed Arrow's Schema field-count limit -- see kArrowInt32FieldCountLimit, above.
	// `current_count` is the column count *before* the one about to be added.
	static void check_column_count_fits_arrow_limit(size_t current_count, const std::string &name, const char *context)
	{
		int64_t limit = g_debug_column_count_limit > 0 ? g_debug_column_count_limit : kArrowInt32FieldCountLimit;
		int64_t new_count = static_cast<int64_t>(current_count) + 1;
		if (new_count > limit)
		{
			report_fatal_error(context, "column '" + name + "': this table would have " + std::to_string(new_count) +
				" columns, exceeding " + std::to_string(kArrowInt32FieldCountLimit) + // GCOVR_EXCL_LINE
				", the maximum column count Arrow's Schema supports"); // GCOVR_EXCL_LINE
		}
	}

	// Formats `v` with 6 significant digits for a parquet_reader_print_stat line.
	static std::string format_stat_double(double v)
	{
		char buf[64];
		std::snprintf(buf, sizeof(buf), "%.6g", v);
		return std::string(buf);
	}

	// Shared by run_qc_range_check below and eval_filter_clause (row-filter
	// value parsing, further below) -- strict (whole-string, no trailing
	// junk) numeric parsing of a raw maml/filter text value.
	static bool parse_int64_strict(const std::string &s, int64_t &out)
	{
		if (s.empty()) return false;
		char *end = nullptr;
		errno = 0;
		long long v = std::strtoll(s.c_str(), &end, 10);
		if (end != s.c_str() + s.size() || errno == ERANGE) return false;
		out = static_cast<int64_t>(v);
		return true;
	}

	// Strict (whole-string, no trailing junk) floating-point parsing of a raw maml/filter text value.
	static bool parse_double_strict(const std::string &s, double &out)
	{
		if (s.empty()) return false;
		char *end = nullptr;
		double v = std::strtod(s.c_str(), &end);
		if (end != s.c_str() + s.size()) return false;
		out = v;
		return true;
	}

	// Outcome of a checked conversion of one source value (real or decimal)
	// into an integer output: kOk on success, kNonIntegral if the source
	// value has a nonzero fractional part, kOverflow if the (integral) value
	// doesn't fit the requested target width. Used by convert_values_to_int32/
	// int64 further below for every source type that can fail this way --
	// unlike the existing, silently-lossy int->real/double->float narrowing
	// elsewhere in this file, a real or decimal value being converted to an
	// integer is never silently truncated: either check failing is a hard
	// error.
	enum class NumericConvertStatus { kOk, kNonIntegral, kOverflow };

	// Checks whether `v` (already widened to double -- see real_family_value_at)
	// is both exactly integral and in range for int32_t/int64_t, filling `out`
	// on success. Two distinctly-named functions rather than one template or
	// one overloaded name: this whole file lives inside `extern "C" { ... }`
	// blocks, which forbids both templates (rejected by every compiler) and
	// overloading (accepted by clang, but rejected by gcc as a "conflicting
	// declaration of C function" -- C linkage doesn't encode parameter types
	// into the symbol name, so two same-named functions genuinely conflict
	// there even though clang doesn't catch it).
	// real_to_int32_checked is genuinely exercised by a passing (non-aborting) float/double->int32
	// round trip. real_to_int64_checked just below is not: it's only ever reached via the
	// extended_real_*_int64 error scenarios, all of which end in std::abort() -- and std::abort()
	// discards that whole process's gcov coverage, including the kOk/kNonIntegral lines that ran
	// before it, not just the abort line itself.
	static NumericConvertStatus real_to_int32_checked(double v, int32_t &out)
	{
		if (!std::isfinite(v) || v != std::trunc(v)) return NumericConvertStatus::kNonIntegral;
		if (v < static_cast<double>(std::numeric_limits<int32_t>::min()) ||
			v > static_cast<double>(std::numeric_limits<int32_t>::max()))
			return NumericConvertStatus::kOverflow; // GCOVR_EXCL_LINE -- the overflow path itself is
			// only reached via extended_real_overflow_int32's abort-ending scenario, unlike the rest
			// of this function (kOk/kNonIntegral), which a passing round trip covers.
		out = static_cast<int32_t>(v);
		return NumericConvertStatus::kOk;
	}
	// GCOVR_EXCL_START -- only reached via an abort-ending scenario, see comment above.
	static NumericConvertStatus real_to_int64_checked(double v, int64_t &out)
	{
		if (!std::isfinite(v) || v != std::trunc(v)) return NumericConvertStatus::kNonIntegral;
		if (v < static_cast<double>(std::numeric_limits<int64_t>::min()) ||
			v > static_cast<double>(std::numeric_limits<int64_t>::max()))
			return NumericConvertStatus::kOverflow;
		out = static_cast<int64_t>(v);
		return NumericConvertStatus::kOk;
	}
	// GCOVR_EXCL_STOP

	// Extracts element `idx` of a FLOAT/HALF_FLOAT/DOUBLE array as a double --
	// shared by convert_values_to_int32/int64 (via real_to_int_checked above),
	// convert_values_to_float32/float64's HALF_FLOAT widening case, and
	// run_qc_range_check/eval_filter_clause below.
	static double real_family_value_at(const std::shared_ptr<arrow::Array> &vals, int64_t idx)
	{
		switch (vals->type_id())
		{
		// GCOVR_EXCL_START -- same reasoning as real_to_int32/64_checked above: only reached via
		// the extended_real_*_int32/64 error scenarios' FLOAT fixture column, which end in an
		// abort (discarding that whole process's gcov coverage). The case label itself is included
		// in this exclusion (not just the body): under GCC, a case label reachable only via an
		// abort-ending scenario shows uncovered in its own right, distinct from Clang's gcov.
		case arrow::Type::FLOAT:
			return static_cast<double>(std::static_pointer_cast<arrow::FloatArray>(vals)->Value(idx));
			// GCOVR_EXCL_STOP
		case arrow::Type::HALF_FLOAT:
		{
			auto arr = std::static_pointer_cast<arrow::HalfFloatArray>(vals);
			return static_cast<double>(arrow::util::Float16::FromBits(arr->Value(idx)).ToFloat());
		}
		default: // arrow::Type::DOUBLE
			return std::static_pointer_cast<arrow::DoubleArray>(vals)->Value(idx);
		}
	}

	// Returns this decimal column's declared scale (digits after the point) --
	// shared by every DECIMAL32/64/128/256 case below. DecimalType is the
	// common base every decimal width's concrete type class derives from, so
	// this one accessor works regardless of which width `vals` actually is.
	static int32_t decimal_scale_of(const std::shared_ptr<arrow::Array> &vals)
	{
		return std::static_pointer_cast<arrow::DecimalType>(vals->type())->scale();
	} // GCOVR_EXCL_LINE -- gcov attribution artifact: this closing brace shows uncovered even though the covered `return` above proves the body ran.

	// Converts element `idx` of a DECIMAL32/64/128/256 array into an exact
	// int64_t via Rescale(scale, 0, ...) -- which fails with
	// DecimalStatus::kRescaleDataLoss exactly when the value has a nonzero
	// fractional part (nonzero digits below the decimal point), giving the
	// same "no silent truncation" behavior real_to_int_checked applies to
	// float/double/half_float sources. Deliberately calls BasicDecimalNN's
	// own Rescale directly (rather than the Decimal128::Rescale/
	// Decimal256::Rescale Result<> wrappers) so kRescaleDataLoss can be told
	// apart from a genuine overflow, instead of collapsing both into one
	// opaque Status.
	static NumericConvertStatus decimal_to_int64_checked(const std::shared_ptr<arrow::Array> &vals, int64_t idx, int64_t &out)
	{
		int32_t scale = decimal_scale_of(vals);
		switch (vals->type_id())
		{
		// GCOVR_EXCL_START -- permanently unreachable, not just hard to trigger via an
		// abort-ending scenario (see decimal_value_at's own identically-reasoned exclusion above,
		// verified there via a temporary type_id() debug print): Parquet C++'s Arrow reader always
		// materializes a decimal column as Decimal128Array/Decimal256Array, never
		// Decimal32Array/Decimal64Array, regardless of the physical Parquet decimal width written
		// -- and this library never constructs one on the write path either (decimals are
		// read-only here). The case label itself is included in this exclusion (not just the
		// body): under GCC, a case label reachable only via a permanently-unreachable path shows
		// uncovered in its own right, distinct from Clang's gcov.
		case arrow::Type::DECIMAL32:
		{
			auto arr = std::static_pointer_cast<arrow::Decimal32Array>(vals);
			arrow::Decimal32 dec(arr->GetValue(idx));
			arrow::BasicDecimal32 rescaled;
			auto status = dec.BasicDecimal32::Rescale(scale, 0, &rescaled);
			if (status == arrow::DecimalStatus::kRescaleDataLoss) return NumericConvertStatus::kNonIntegral;
			if (status != arrow::DecimalStatus::kSuccess) return NumericConvertStatus::kOverflow;
			out = static_cast<int64_t>(rescaled.value());
			return NumericConvertStatus::kOk;
		}
		case arrow::Type::DECIMAL64:
		{
			auto arr = std::static_pointer_cast<arrow::Decimal64Array>(vals);
			arrow::Decimal64 dec(arr->GetValue(idx));
			arrow::BasicDecimal64 rescaled;
			auto status = dec.BasicDecimal64::Rescale(scale, 0, &rescaled);
			if (status == arrow::DecimalStatus::kRescaleDataLoss) return NumericConvertStatus::kNonIntegral;
			if (status != arrow::DecimalStatus::kSuccess) return NumericConvertStatus::kOverflow;
			out = rescaled.value();
			return NumericConvertStatus::kOk;
		}
		// GCOVR_EXCL_STOP
		case arrow::Type::DECIMAL128:
		{
			auto arr = std::static_pointer_cast<arrow::Decimal128Array>(vals);
			arrow::Decimal128 dec(arr->GetValue(idx));
			arrow::BasicDecimal128 rescaled;
			auto status = dec.BasicDecimal128::Rescale(scale, 0, &rescaled);
			if (status == arrow::DecimalStatus::kRescaleDataLoss) return NumericConvertStatus::kNonIntegral;
			if (status != arrow::DecimalStatus::kSuccess) return NumericConvertStatus::kOverflow;
			auto as_int = arrow::Decimal128(rescaled).ToInteger<int64_t>();
			if (!as_int.ok()) return NumericConvertStatus::kOverflow;
			out = as_int.ValueOrDie();
			return NumericConvertStatus::kOk;
		}
		default: // arrow::Type::DECIMAL256
		{
			auto arr = std::static_pointer_cast<arrow::Decimal256Array>(vals);
			arrow::Decimal256 dec(arr->GetValue(idx));
			arrow::BasicDecimal256 rescaled;
			auto status = dec.BasicDecimal256::Rescale(scale, 0, &rescaled);
			if (status == arrow::DecimalStatus::kRescaleDataLoss) return NumericConvertStatus::kNonIntegral;
			if (status != arrow::DecimalStatus::kSuccess) return NumericConvertStatus::kOverflow;
			// Decimal256 has no ToInteger<T>() (unlike 32/64/128 above) -- it
			// fits in int64_t exactly iff every word above the lowest one is
			// just the sign-extension of the low word's own sign bit.
			auto words = rescaled.native_endian_array();
			int64_t low = static_cast<int64_t>(words[0]);
			uint64_t sign_ext = (low < 0) ? ~uint64_t{0} : uint64_t{0};
			for (size_t w = 1; w < words.size(); ++w)
			{
				if (words[w] != sign_ext) return NumericConvertStatus::kOverflow;
			}
			out = low;
			return NumericConvertStatus::kOk;
		}
		}
	}

	// Converts element `idx` of a DECIMAL32/64/128/256 array to a scaled
	// double -- shared by convert_values_to_float32/float64's unchecked
	// (silently lossy, same stance as int64->real32/double->real32 narrowing
	// elsewhere in this file) widening cases, and run_qc_range_check/
	// eval_filter_clause below.
	static double decimal_value_at(const std::shared_ptr<arrow::Array> &vals, int64_t idx)
	{
		int32_t scale = decimal_scale_of(vals);
		switch (vals->type_id())
		{
		// GCOVR_EXCL_START -- permanently unreachable, not just hard to trigger via an
		// abort-ending scenario: verified empirically (temporary type_id() debug print, both
		// DECIMAL32(9,0)/DECIMAL64(18,0)
		// source columns) that Parquet C++'s Arrow reader always materializes a decimal column
		// as Decimal128Array (or Decimal256Array for precision > 38), regardless of the
		// physical Parquet decimal width written -- Decimal32Array/Decimal64Array are never
		// produced on read at all. This library also never constructs a Decimal32Array/
		// Decimal64Array on the write path (decimals are a read-only source type here -- see
		// tools/generate_fixtures.cpp's own generation comment). So these two case arms have no
		// reachable caller through the public API, on any input, successful or aborting.
		case arrow::Type::DECIMAL32:
			return arrow::Decimal32(std::static_pointer_cast<arrow::Decimal32Array>(vals)->GetValue(idx)).ToDouble(scale);
		case arrow::Type::DECIMAL64:
			return arrow::Decimal64(std::static_pointer_cast<arrow::Decimal64Array>(vals)->GetValue(idx)).ToDouble(scale);
		// GCOVR_EXCL_STOP
		case arrow::Type::DECIMAL128:
			return arrow::Decimal128(std::static_pointer_cast<arrow::Decimal128Array>(vals)->GetValue(idx)).ToDouble(scale);
		default: // arrow::Type::DECIMAL256
			return arrow::Decimal256(std::static_pointer_cast<arrow::Decimal256Array>(vals)->GetValue(idx)).ToDouble(scale);
		}
	}

	// True for every Arrow physical type that widens into an int64_t with no
	// possibility of overflow -- INT8/16/32/64 and UINT8/16/32, but not
	// UINT64 (which can exceed INT64_MAX). Shared by convert_values_to_int64
	// further below and by run_qc_range_check/eval_filter_clause (qc/filter
	// always compare the raw physical value, never the maml-declared
	// data_type -- see those functions' own comments).
	// GCOVR_EXCL_START -- gcov attribution artifact under GCC, not dead code: this function is
	// genuinely, constantly called (run_qc_range_check, eval_filter_clause, and every narrow-int
	// array conversion), and every one of INT8/16/32/64/UINT8/16/32 is exercised somewhere in the
	// test suite (e.g. the extended-source-type UINT16/UINT8 fixtures, and the int64 qc range-
	// violation scenario) -- if this function's logic were actually wrong or unreached, those
	// tests would fail on incorrect values, not merely show as uncovered. Confirmed under CI's
	// real GCC/gcovr toolchain that the function's signature/switch-header/first-case-label/
	// return/default lines still show 0 hits, apparently because GCC merges the whole
	// fall-through case-label group into one block and attributes it inconsistently -- the same
	// category of closing-brace/case-label attribution difference documented elsewhere in this
	// file, just affecting more of one function's lines here.
	static bool is_small_integer_family(arrow::Type::type id)
	{
		switch (id)
		{
		case arrow::Type::INT8:
		case arrow::Type::INT16:
		case arrow::Type::INT32:
		case arrow::Type::INT64:
		case arrow::Type::UINT8:
		case arrow::Type::UINT16:
		case arrow::Type::UINT32:
			return true;
		default:
			return false;
		}
	}
	// GCOVR_EXCL_STOP

	// Extracts element `idx` of any is_small_integer_family array as an exact
	// int64_t. Shared by convert_values_to_int32/int64 and
	// run_qc_range_check/eval_filter_clause.
	static int64_t small_integer_value_at(const std::shared_ptr<arrow::Array> &vals, int64_t idx)
	{
		switch (vals->type_id())
		{
		case arrow::Type::INT8:
			return static_cast<int64_t>(std::static_pointer_cast<arrow::Int8Array>(vals)->Value(idx));
		case arrow::Type::INT16:
			return static_cast<int64_t>(std::static_pointer_cast<arrow::Int16Array>(vals)->Value(idx));
		case arrow::Type::UINT8:
			return static_cast<int64_t>(std::static_pointer_cast<arrow::UInt8Array>(vals)->Value(idx));
		case arrow::Type::UINT16:
			return static_cast<int64_t>(std::static_pointer_cast<arrow::UInt16Array>(vals)->Value(idx));
		case arrow::Type::UINT32:
			return static_cast<int64_t>(std::static_pointer_cast<arrow::UInt32Array>(vals)->Value(idx));
		case arrow::Type::INT32:
			return static_cast<int64_t>(std::static_pointer_cast<arrow::Int32Array>(vals)->Value(idx));
		default: // arrow::Type::INT64
			return std::static_pointer_cast<arrow::Int64Array>(vals)->Value(idx);
		}
	}

	// Read-time QC (see the QcRule struct and parquet_reader_set_qc further
	// below): checks `array` (already the filtered version, if a filter is
	// set -- see apply_filter_mask) against `rule`'s declared Null policy.
	// Fires (returns true, filling `out_message`) only if Nulls are found
	// and the maml's qc: miss: did NOT declare Null/NA for this field --
	// independent of whether the caller passed null_value=/is_valid=, and
	// regardless of whether reading would go on to abort for that same
	// reason (see check_or_report_nulls/report_nulls_list_*): this is a
	// diagnostic, not a substitute for that existing strict-by-default
	// behavior, which is completely unchanged by any of this.
	static bool run_qc_null_check(const std::shared_ptr<arrow::Array> &array, const QcRule &rule,
		const std::string &colname, std::string &out_message)
	{
		if (rule.null_values_allowed) return false;
		int64_t nulls = array->null_count();
		if (nulls == 0) return false;
		// Core message only (no "WARNING: " prefix, no "parquet-fortran: "
		// prefix) -- run_qc_checks adds whichever is appropriate for the
		// soft (WARNING to stdout) vs hard (report_fatal_error) mode.
		out_message = "qc violation for column '" + colname + "' (based on incomplete column information): " +
			std::to_string(nulls) + " unexpected Null value(s) found (qc: miss: does not declare Null/NA for this field)";
		return true;
	}

	// Checks `array`'s non-Null elements against `rule`'s declared min:/max:
	// bounds (parsed dynamically here, against whatever `array`'s actual
	// Arrow type turns out to be -- never trusting a maml-declared
	// data_type, the same convention parquet_reader_set_filter's clause
	// evaluation already uses). Always false for a boolean array (min/max
	// isn't meaningful there) or a column with neither bound declared.
	// Fires at most one combined message covering either/both bounds,
	// mirroring the writer's own qc: WARNING wording exactly.
	static bool run_qc_range_check(const std::shared_ptr<arrow::Array> &array, const QcRule &rule,
		const std::string &colname, std::string &out_message)
	{
		if (!(rule.has_min || rule.has_max)) return false;
		if (array->type_id() == arrow::Type::BOOL) return false;

		int64_t n = array->length();
		int64_t n_valid = 0, n_violate = 0;
		bool any_valid = false;
		std::string bounds_desc, data_min_s, data_max_s;

		switch (array->type_id())
		{
		case arrow::Type::INT32:
		case arrow::Type::INT64:
		case arrow::Type::INT8:
		case arrow::Type::INT16:
		case arrow::Type::UINT8:
		case arrow::Type::UINT16:
		case arrow::Type::UINT32:
		{
			int64_t min_bound = 0, max_bound = 0;
			bool have_min = rule.has_min && parse_int64_strict(rule.min_raw, min_bound);
			bool have_max = rule.has_max && parse_int64_strict(rule.max_raw, max_bound);
			if (!have_min && !have_max) return false;
			int64_t data_min = 0, data_max = 0;
			auto scan = [&](int64_t v)
			{
				n_valid++;
				if (!any_valid) { data_min = v; data_max = v; any_valid = true; }
				else { data_min = std::min(data_min, v); data_max = std::max(data_max, v); }
				bool ok = true;
				if (have_min) ok = ok && compare_op<int64_t>(v, min_bound, rule.min_op);
				if (have_max) ok = ok && compare_op<int64_t>(v, max_bound, rule.max_op);
				if (!ok) n_violate++;
			};
			for (int64_t i = 0; i < n; ++i) if (!array->IsNull(i)) scan(small_integer_value_at(array, i));
			if (!any_valid || n_violate == 0) return false;
			if (have_min) bounds_desc = "min " + rule.min_op + " " + std::to_string(min_bound);
			if (have_max)
			{
				if (!bounds_desc.empty()) bounds_desc += ", ";
				bounds_desc += "max " + rule.max_op + " " + std::to_string(max_bound);
			}
			data_min_s = std::to_string(data_min);
			data_max_s = std::to_string(data_max);
			break;
		}
		case arrow::Type::FLOAT:
		case arrow::Type::DOUBLE:
		case arrow::Type::HALF_FLOAT:
		case arrow::Type::UINT64:
		case arrow::Type::DECIMAL32:
		case arrow::Type::DECIMAL64:
		case arrow::Type::DECIMAL128:
		case arrow::Type::DECIMAL256:
		{
			double min_bound = 0, max_bound = 0;
			bool have_min = rule.has_min && parse_double_strict(rule.min_raw, min_bound);
			bool have_max = rule.has_max && parse_double_strict(rule.max_raw, max_bound);
			if (!have_min && !have_max) return false;
			double data_min = 0, data_max = 0;
			auto scan = [&](double v)
			{
				n_valid++;
				if (!any_valid) { data_min = v; data_max = v; any_valid = true; }
				else { data_min = std::min(data_min, v); data_max = std::max(data_max, v); }
				bool ok = true;
				if (have_min) ok = ok && compare_op<double>(v, min_bound, rule.min_op);
				if (have_max) ok = ok && compare_op<double>(v, max_bound, rule.max_op);
				if (!ok) n_violate++;
			};
			auto type_id = array->type_id();
			bool is_decimal = type_id == arrow::Type::DECIMAL32 || type_id == arrow::Type::DECIMAL64 ||
				type_id == arrow::Type::DECIMAL128 || type_id == arrow::Type::DECIMAL256;
			for (int64_t i = 0; i < n; ++i)
			{
				if (array->IsNull(i)) continue;
				if (is_decimal) scan(decimal_value_at(array, i));
				else if (type_id == arrow::Type::UINT64) scan(static_cast<double>(std::static_pointer_cast<arrow::UInt64Array>(array)->Value(i)));
				else scan(real_family_value_at(array, i));
			}
			if (!any_valid || n_violate == 0) return false;
			if (have_min) bounds_desc = "min " + rule.min_op + " " + format_stat_double(min_bound);
			if (have_max)
			{
				if (!bounds_desc.empty()) bounds_desc += ", ";
				bounds_desc += "max " + rule.max_op + " " + format_stat_double(max_bound);
			}
			data_min_s = format_stat_double(data_min);
			data_max_s = format_stat_double(data_max);
			break;
		}
		case arrow::Type::STRING:
		case arrow::Type::LARGE_STRING:
		case arrow::Type::STRING_VIEW:
		{
			auto acc = make_string_like_accessor(array); // GCOVR_EXCL_LINE -- gcov attribution
			// artifact, not a genuine gap: verified directly that this line does run (a passing,
			// non-aborting scenario_qc_range_violation_string_warns exercises exactly this branch
			// end to end, printing the correct string-column qc violation message), yet this one
			// line never shows as hit while every other line in the same branch does. Same
			// category as the closing-brace gcov artifact documented elsewhere in this file, just
			// a different line shape (a variable initialization immediately followed by a lambda
			// definition, rather than a closing brace after a [[noreturn]] call).
			std::string data_min, data_max;
			auto scan = [&](const std::string &v)
			{
				n_valid++;
				if (!any_valid) { data_min = v; data_max = v; any_valid = true; }
				else { data_min = std::min(data_min, v); data_max = std::max(data_max, v); }
				bool ok = true;
				if (rule.has_min) ok = ok && compare_op<std::string>(v, rule.min_raw, rule.min_op);
				if (rule.has_max) ok = ok && compare_op<std::string>(v, rule.max_raw, rule.max_op);
				if (!ok) n_violate++;
			};
			for (int64_t i = 0; i < n; ++i) if (!acc.is_null(i)) scan(std::string(acc.get_view(i)));
			if (!any_valid || n_violate == 0) return false;
			if (rule.has_min) bounds_desc = "min " + rule.min_op + " \"" + rule.min_raw + "\"";
			if (rule.has_max)
			{
				if (!bounds_desc.empty()) bounds_desc += ", ";
				bounds_desc += "max " + rule.max_op + " \"" + rule.max_raw + "\"";
			}
			data_min_s = "\"" + data_min + "\"";
			data_max_s = "\"" + data_max + "\"";
			break;
		}
		// GCOVR_EXCL_START -- untested compat backstop: every physical Arrow type this library's
		// own writer can produce for a column eligible for a qc rule is already an explicit case
		// above (BOOL short-circuits at the top of this function; FIXED_SIZE_LIST/LIST are
		// flattened to their element type by flatten_for_stats before this function is ever
		// called; date/time/timestamp columns cannot carry a qc: rule at all -- rejected at MAML
		// validation time). No known way to construct a fixture whose physical column type
		// reaches here.
		default:
			return false;
		}
		// GCOVR_EXCL_STOP

		// Core message only (no "WARNING: "/"parquet-fortran: " prefix) --
		// run_qc_checks adds whichever suits the soft vs hard mode.
		out_message = "qc violation for column '" + colname + "' (based on incomplete column information): declared " +
			bounds_desc + ", data range [" + data_min_s + ", " + data_max_s + "], " +
			std::to_string(n_violate) + " of " + std::to_string(n_valid) + " valid element(s) out of range";
		return true;
	}

	// Runs both read-time QC checks for column `qc_key` (a plain column name, or a full dotted
	// struct-leaf path) against `array` (whatever was just decoded/cached/unwrapped for it --
	// already the filtered version, if a filter is set), if this reader has qc enabled and this
	// exact qc_key has a rule declared for it. `qc_key` is deliberately a separate parameter from
	// `name` (used only for messages): they're the same string at every call site except
	// get_row_group_chunk_array, which appends a " [row group N]" suffix to `name` for display
	// but must still look the rule up by the bare column path. qc_rules/qc_null_warned/
	// qc_range_warned are keyed by this exact string (not a physical column index) because two
	// different struct-leaf paths can share one physical top-level column index -- an int-keyed
	// map would let one leaf's rule silently clobber another's. In soft mode (qc_soft), each of
	// the two checks (Null-presence, range) prints a WARNING to stdout at most once per qc_key for
	// the whole lifetime of the reader -- qc_null_warned/qc_range_warned record that, so reading/
	// prefetching/filtering the same column more than once doesn't repeat the same warning. In
	// hard mode (the default), the first violation of either check aborts the process via
	// report_fatal_error, so the throttling sets are never consulted. Called from mark_read/
	// mark_read_string (an actual typed read), parquet_reader_prefetch_columns, and
	// parquet_reader_set_filter (for a column the filter itself touches) -- i.e. every place this
	// reader already tracks as "touched" for parquet_reader_print_stat.
	static void run_qc_checks(ParquetReaderHandle *reader_handle, const std::string &qc_key, const std::string &name,
		const std::shared_ptr<arrow::Array> &array)
	{
		if (!reader_handle->qc_enabled) return;
		auto it = reader_handle->qc_rules.find(qc_key);
		if (it == reader_handle->qc_rules.end()) return;

		auto flat = flatten_for_stats(array);

		if (reader_handle->qc_null_warned.find(qc_key) == reader_handle->qc_null_warned.end())
		{
			std::string msg;
			if (run_qc_null_check(flat, it->second, name, msg))
			{
				if (!reader_handle->qc_soft)
				{
					report_fatal_error("qc hard check", msg);
				}
				std::fprintf(stdout, "WARNING: %s\n", msg.c_str());
				reader_handle->qc_null_warned.insert(qc_key);
			}
		}

		if (reader_handle->qc_range_warned.find(qc_key) == reader_handle->qc_range_warned.end())
		{
			std::string msg;
			if (run_qc_range_check(flat, it->second, name, msg))
			{
				if (!reader_handle->qc_soft)
				{
					report_fatal_error("qc hard check", msg);
				}
				std::fprintf(stdout, "WARNING: %s\n", msg.c_str());
				reader_handle->qc_range_warned.insert(qc_key);
			}
		}
	}

	// Records that a typed parquet_read_* entry point actually read `name`
	// (as opposed to it merely being cached via get_single_chunk_array's own
	// cache-fill or via parquet_reader_prefetch_columns) and what Fortran-side
	// output type it was read into -- used only for parquet_reader_print_stat.
	// Takes `array` directly (the same array the caller just decoded/sliced values from) rather
	// than looking it up in reader_handle->column_cache: a row-group-scoped read (see
	// resolve_row_group_for_row/get_row_group_chunk_array, used by the row-mode read functions)
	// never populates column_cache at all, so an at(idx) lookup there would throw
	// std::out_of_range for those callers. `name` may be a dotted struct-leaf path: was_read/
	// output_type_used stay keyed by the *physical top-level* column index (so a struct read via
	// two different leaves shows up as one parquet_reader_print_stat row for the whole struct,
	// not one row per leaf -- see CLAUDE.md's nested-struct-field design notes for why this is an
	// accepted v1 limitation), while qc (which does need per-leaf precision) is run against the
	// full, unresolved `name` -- see run_qc_checks's own comment.
	static void mark_read(ParquetReaderHandle *reader_handle, const char *name, const char *type_name,
		const std::shared_ptr<arrow::Array> &array)
	{
		auto resolved = resolve_struct_path(reader_handle->schema, name);
		auto idx = static_cast<int>(get_column_index(reader_handle, resolved.top_level_name.c_str()));
		reader_handle->was_read.insert(idx);
		reader_handle->output_type_used[idx] = type_name;
		run_qc_checks(reader_handle, name, name, array);
	}

	// Same as mark_read, but also records the declared string item_len (for parquet_reader_print_stat).
	static void mark_read_string(ParquetReaderHandle *reader_handle, const char *name, int64_t item_len,
		const std::shared_ptr<arrow::Array> &array)
	{
		auto resolved = resolve_struct_path(reader_handle->schema, name);
		auto idx = static_cast<int>(get_column_index(reader_handle, resolved.top_level_name.c_str()));
		reader_handle->was_read.insert(idx);
		reader_handle->output_type_used[idx] = "string";
		reader_handle->output_str_len_used[idx] = item_len;
		run_qc_checks(reader_handle, name, name, array);
	}

	// Returns the vector-column element count of a fixed-size-list or list array (0 for a scalar column).
	static int64_t get_col_size(const std::shared_ptr<arrow::Array> &array)
	{
		if (array->type_id() == arrow::Type::FIXED_SIZE_LIST)
		{
			auto list_arr = std::static_pointer_cast<arrow::FixedSizeListArray>(array);
			return static_cast<int64_t>(list_arr->value_length());
		}

		if (array->type_id() == arrow::Type::LIST)
		{
			auto list_arr = std::static_pointer_cast<arrow::ListArray>(array);
			if (list_arr->length() == 0)
			{
				return 0;
			}

			const auto *offsets = reinterpret_cast<const int32_t *>(list_arr->value_offsets()->data());
			auto value_length = static_cast<int64_t>(offsets[1] - offsets[0]);
			for (int64_t i = 1; i < list_arr->length(); ++i)
			{
				if (static_cast<int64_t>(offsets[i + 1] - offsets[i]) != value_length)
				{
					return 1;
				}
			}
			return value_length;
		}

		if (array->type_id() == arrow::Type::LARGE_LIST)
		{
			auto list_arr = std::static_pointer_cast<arrow::LargeListArray>(array);
			if (list_arr->length() == 0)
			{
				return 0;
			}

			const auto *offsets = reinterpret_cast<const int64_t *>(list_arr->value_offsets()->data());
			auto value_length = offsets[1] - offsets[0];
			for (int64_t i = 1; i < list_arr->length(); ++i)
			{
				if ((offsets[i + 1] - offsets[i]) != value_length)
				{
					return 1;
				}
			}
			return value_length;
		}
		return 1;
	}

	// context identifies the calling extern "C" entry point (parquet_read_*_array_column/
	// _row/_element) for report_fatal_error's diagnostic -- see the comment on
	// convert_values_to_int32 for why these report_fatal_error rather than throw:
	// a container-shape mismatch here is exactly as fatal/unrecoverable as a
	// value-type mismatch further down the same read, so it gets the same
	// clean-abort treatment instead of being left as an uncaught exception.
	static std::shared_ptr<arrow::Array> get_uniform_list_values(const std::shared_ptr<arrow::Array> &array,
		const std::string &name, int64_t nrows, int64_t col_size, const char *context)
	{
		if (array->type_id() == arrow::Type::FIXED_SIZE_LIST)
		{
			auto list_arr = std::static_pointer_cast<arrow::FixedSizeListArray>(array);
			if (list_arr->length() != nrows || static_cast<int64_t>(list_arr->value_length()) != col_size)
			{
				report_fatal_error(context, std::string("shape mismatch for column: ") + name);
			}
			return list_arr->values()->Slice(list_arr->value_offset(0), nrows * col_size);
		}
		if (array->type_id() == arrow::Type::LIST)
		{
			auto list_arr = std::static_pointer_cast<arrow::ListArray>(array);
			if (list_arr->length() != nrows)
			{
				report_fatal_error(context, std::string("nrows mismatch for column: ") + name);
			}
			for (int64_t i = 0; i < nrows; ++i)
			{
				if (static_cast<int64_t>(list_arr->value_length(i)) != col_size)
				{
					report_fatal_error(context, std::string("shape mismatch for column: ") + name);
				}
			}
			return list_arr->values()->Slice(list_arr->value_offset(0), nrows * col_size);
		}
		// GCOVR_EXCL_START -- untested compat backstop: no LARGE_LIST fixture exists (this
		// library's own writer never produces one, and no known external tool defaults to it
		// either); the LIST branch above is the one exercised via test/fixtures/list_vector.parquet.
		// The condition line itself is included in this exclusion (not just its body): the
		// FIXED_SIZE_LIST/LIST branches above both unconditionally `return`, so this `if` is
		// permanently unreachable too, not just "always executes and only the body is untested"
		// like an ordinary if-line elsewhere in this file.
		if (array->type_id() == arrow::Type::LARGE_LIST)
		{
			auto list_arr = std::static_pointer_cast<arrow::LargeListArray>(array);
			if (list_arr->length() != nrows)
			{
				report_fatal_error(context, std::string("nrows mismatch for column: ") + name);
			}
			for (int64_t i = 0; i < nrows; ++i)
			{
				if (list_arr->value_length(i) != col_size)
				{
					report_fatal_error(context, std::string("shape mismatch for column: ") + name);
				}
			}
			return list_arr->values()->Slice(list_arr->value_offset(0), nrows * col_size);
		}
		// GCOVR_EXCL_STOP
		report_fatal_error(context, std::string("type mismatch for column: ") + name +
			" (expected fixed_size_list/list/large_list, got " + array->type()->ToString() + ")"); // GCOVR_EXCL_LINE
	}

	// Copies `src` into `dst` (a fixed-width, space-padded Fortran character buffer of length item_len).
	static void copy_string_with_padding(char *dst, int64_t item_len, const std::string_view &src)
	{
		std::memset(dst, ' ', static_cast<size_t>(item_len));
		auto ncopy = std::min<int64_t>(item_len, static_cast<int64_t>(src.size()));
		if (ncopy > 0)
		{
			std::memcpy(dst, src.data(), static_cast<size_t>(ncopy));
		}
	}

	// Escapes the 5 reserved XML characters (& < > " ') in `s`, for build_votable_xml.
	static std::string xml_escape(const std::string &s)
	{
		std::string out;
		out.reserve(s.size());
		for (char c : s)
		{
			switch (c)
			{
			case '&':
				out += "&amp;";
				break;
			case '<':
				out += "&lt;";
				break;
			case '>':
				out += "&gt;";
				break;
			case '"':
				out += "&quot;";
				break;
			case '\'':
				out += "&apos;";
				break;
			default:
				out += c;
				break;
			}
		}
		return out;
	} // GCOVR_EXCL_LINE -- gcov attribution artifact under GCC: this closing brace shows uncovered
	// even though xml_escape is directly unit-tested (test_metadata.f90's VOTable XML sidecar
	// escaping test) and every case arm above it is covered.

	// Returns the current UTC time as an ISO-8601 "YYYY-MM-DDTHH:MM:SS" string.
	static std::string current_utc_timestamp()
	{
		std::time_t now = std::time(nullptr);
		std::tm tm{};
#if defined(_WIN32)
		gmtime_s(&tm, &now);
#else
		gmtime_r(&now, &tm);
#endif
		char buffer[32];
		std::strftime(buffer, sizeof(buffer), "%Y-%m-%dT%H:%M:%S", &tm);
		return std::string(buffer);
	}

	// Strips trailing spaces and NUL bytes (Fortran fixed-width buffer padding) from `s`.
	static std::string trim_right_spaces_and_nuls(const std::string &s)
	{
		size_t end = s.size();
		while (end > 0 && (s[end - 1] == ' ' || s[end - 1] == '\0'))
		{
			--end;
		}
		return s.substr(0, end);
	}

	// Builds the VOTable-style XML sidecar/header text describing the table's columns and metadata.
	static std::string build_votable_xml(const std::string &table_name,
									const std::vector<ColumnMetadata> &columns,
									const std::vector<TableMetadataEntry> &table_metadata,
									const std::string &date)
	{
		std::ostringstream xml;
		xml << "<?xml version='1.0'?>\n"
			<< "<VOTABLE version=\"1.4\" xmlns=\"http://www.ivoa.net/xml/VOTable/v1.3\">\n"
			<< "<RESOURCE>\n"
			// GCOVR_EXCL_START -- gcov attribution artifact under GCC: this continuation line of a
			// single chained operator<< statement shows uncovered even though build_votable_xml is
			// directly exercised by test_metadata.f90's VOTable generation tests.
			<< "<TABLE name=\"" << xml_escape(table_name) << "\">\n"
			// GCOVR_EXCL_STOP
			<< "<PARAM arraysize=\"19\" datatype=\"char\" name=\"DATE\" value=\""
			<< xml_escape(date) << "\">\n"
			<< "<DESCRIPTION>file creation date (YYYY-MM-DDThh:mm:ss UT)</DESCRIPTION>\n"
			<< "</PARAM>\n"
		;

		for (const auto &kv : table_metadata)
		{
			xml << "<PARAM datatype=\"char\" arraysize=\"*\" name=\""
				// GCOVR_EXCL_START -- gcov attribution artifact under GCC: same chained-statement
				// artifact as build_votable_xml's other continuation-line exclusions above; this
				// table_metadata loop is exercised by test_metadata.f90's VOTable generation tests.
				<< xml_escape(kv.key)
				// GCOVR_EXCL_STOP
				<< "\" value=\""
				<< xml_escape(kv.value)
				<< "\"";
			if (!kv.description.empty())
			{
				xml << ">\n<DESCRIPTION>" << xml_escape(kv.description) << "</DESCRIPTION>\n</PARAM>\n";
			}
			else
			{
				xml << "/>\n";
			}
		}

		for (const auto &col : columns)
		{
			// GCOVR_EXCL_START -- gcov attribution artifact under GCC: same chained-statement
			// artifact as build_votable_xml's other continuation-line exclusions above; this
			// columns loop is exercised by test_metadata.f90's VOTable generation tests.
			xml << "<FIELD datatype=\"" << xml_escape(col.data_type)
			// GCOVR_EXCL_STOP
				<< "\" name=\"" << xml_escape(col.name) << "\"";
			if (!col.unit.empty())
			{
				xml << " unit=\"" << xml_escape(col.unit) << "\"";
			}
			if (!col.ucd.empty())
			{
				xml << " ucd=\"" << xml_escape(col.ucd) << "\"";
			}
			xml << ">\n";
			if (!col.description.empty())
			{
				xml << "<DESCRIPTION>" << xml_escape(col.description) << "</DESCRIPTION>\n";
			}
			xml << "</FIELD>\n";
		}

		xml
			<< "<!-- Dummy VOTable - no DATA element -->\n"
			<< "</TABLE>\n"
			<< "</RESOURCE>\n"
			<< "</VOTABLE>\n";
		return xml.str();
	}

	// `nullable` only affects the scalar (col_size <= 1) case: the outer
	// field there is the only place a Null can ever land for a scalar
	// column, so it must say so in the schema whenever the caller actually
	// wrote one (see has_any_null). For array/vector columns (col_size >
	// 1), nulls are always element-level (never a whole missing row -- by
	// design, see parquet_append_*_column's valid_in handling below), and
	// arrow::fixed_size_list(value_type, size)'s convenience constructor
	// already builds its inner child field with nullable=true unconditionally
	// (confirmed empirically), so the outer list field itself stays
	// nullable=false always: a row's vector is never itself missing.
	static std::shared_ptr<arrow::Field> build_field(
		const std::string &name,
		const std::shared_ptr<arrow::DataType> &value_type,
		int64_t col_size,
		bool nullable = false)
	{
		if (col_size > 1)
		{
			return arrow::field(name, arrow::fixed_size_list(value_type, static_cast<int32_t>(col_size)), false);
		}
		return arrow::field(name, value_type, nullable);
	}

	// True if any of the first `n` entries of `valid_in` is 0 (a Null); false if valid_in is nullptr.
	static bool has_any_null(const int8_t *valid_in, int64_t n)
	{
		if (valid_in == nullptr) return false;
		for (int64_t i = 0; i < n; ++i)
		{
			if (valid_in[i] == 0) return true;
		}
		return false;
	}

	// Stores `field`/`array` for `name`, at its schema-declared position if the writer has a
	// schema (error stops on a repeat write), or appended in write order for a schema-less writer.
	static void append_column(
		ParquetWriterHandle *writer_handle,
		const std::string &name,
		const std::shared_ptr<arrow::Field> &field,
		const std::shared_ptr<arrow::Array> &array)
	{
		// A whole-column (parquet_write_column) write is never valid once the streaming
		// row-group API has already locked the file's schema (see parquet_finish_row_group) --
		// every column, new or previously-declared-but-untouched, must go through
		// parquet_write_column_chunk from that point on. A column that was already
		// chunk-started (but the file's schema not yet locked) is instead caught below, by the
		// ordinary "written more than once" check -- its field is already set at that point,
		// same as a column written the ordinary way twice.
		if (writer_handle->row_group_writer)
		{ // GCOVR_EXCL_START -- this throw is never caught anywhere in the call chain, so it crosses
		  // the extern "C" boundary uncaught -> std::terminate() -> abort, which discards that whole
		  // process's gcov coverage just like std::abort() does; tested via
		  // scenario_row_group_whole_column_after_streaming_started
			throw std::runtime_error("Column written via parquet_write_column after the streaming row-group API "
				"already started writing row groups: " + name + " -- every column must be written via "
				"parquet_write_column before the first parquet_new_row_group call");
		}
		// GCOVR_EXCL_STOP

		auto metadata_index = static_cast<int64_t>(-1);
		for (int64_t i = 0; i < static_cast<int64_t>(writer_handle->column_metadata.size()); ++i)
		{
			if (writer_handle->column_metadata[static_cast<size_t>(i)].name == name)
			{
				metadata_index = i;
				break;
			}
		}

		if (metadata_index >= 0)
		{
			// fields/arrays are always kept resized in lockstep with column_metadata by
			// parquet_add_column_metadata (its only growth site) -- no resize needed here.
			auto idx = static_cast<size_t>(metadata_index);
			if (writer_handle->fields[idx] || writer_handle->arrays[idx])
			{ // GCOVR_EXCL_START -- dead: parquet_write.f90's
			  // parquet_check_and_mark_written_name/parquet_mark_column_written already error stops
			  // on a repeat write before ever calling into C++.
				throw std::runtime_error("Column written more than once: " + name);
			}
			// GCOVR_EXCL_STOP

			writer_handle->fields[idx] = field;
			writer_handle->arrays[idx] = array;
			return;
		}

		check_column_count_fits_arrow_limit(writer_handle->fields.size(), name, "parquet_append_column");
		writer_handle->fields.push_back(field);
		writer_handle->arrays.push_back(array);
	}

	// Builds the flat key-value file metadata: the VOTable XML sidecar (if any columns are
	// declared), the DATE/name/version keys, every table_metadata entry, and per-column
	// unit/description/ucd/datatype keys.
	static std::shared_ptr<arrow::KeyValueMetadata> build_file_metadata(
		const std::vector<ColumnMetadata> &column_metadata,
		const std::vector<TableMetadataEntry> &table_metadata)
	{
		auto date = current_utc_timestamp();

		std::string table_name = "table";
		for (const auto &kv : table_metadata)
		{
			if (kv.key == "table")
			{
				table_name = kv.value;
				break;
			}
		}

		std::vector<std::string> keys{
			"IVOA.VOTable-Parquet.version",
			"DATE",
			"name"};
		std::vector<std::string> values{"1.0", date, table_name};

		if (!column_metadata.empty())
		{
			auto votable_xml = build_votable_xml(table_name, column_metadata, table_metadata, date);
			keys.insert(keys.begin(), "IVOA.VOTable-Parquet.content");
			values.insert(values.begin(), votable_xml);
		}

		for (const auto &kv : table_metadata)
		{
			keys.push_back(kv.key);
			values.push_back(kv.value);
		}

		for (const auto &col : column_metadata)
		{
			const auto prefix = std::string("column.") + col.name + ".";
			keys.push_back(prefix + "unit");
			values.push_back(col.unit);
			keys.push_back(prefix + "description");
			values.push_back(col.description);
			keys.push_back(prefix + "ucd");
			values.push_back(col.ucd);
			keys.push_back(prefix + "datatype");
			values.push_back(col.data_type);
			keys.push_back(prefix + "array_size");
			values.push_back(std::to_string(col.array_size));
			keys.push_back(prefix + "col_size");
			values.push_back(std::to_string(col.col_size));
		}

		return std::make_shared<arrow::KeyValueMetadata>(keys, values);
	}

	// Creates filename and returns an opaque handle to a new parquet writer for it.
	void *create_parquet_writer(const char *filename)
	{
		auto *handle = new ParquetWriterHandle{};
		auto result = arrow::io::FileOutputStream::Open(filename);
		if (!result.ok())
		{
			delete handle; // GCOVR_EXCL_LINE -- runs in open_writer_bad_path, but the report_fatal_error()
			                // below calls std::abort(), which discards that whole process's gcov data,
			                // including this line that ran just before it.
			report_fatal_error("create_parquet_writer",
				std::string("failed to open '") + filename + "' for writing: " + result.status().ToString()); // GCOVR_EXCL_LINE
		}
		handle->outfile = result.ValueOrDie();
		return handle;
	}

	// compression_name is expected already-lowercased (Fortran side does this via
	// parquet_to_lower before calling). Only the mainstream, always-available
	// codecs are exposed here; lzo/bz2/lz4_hadoop are niche and some Arrow builds
	// don't even compile support for them in, so they're deliberately left out.
	static arrow::Compression::type parse_compression_name(const std::string &compression_name)
	{
		if (compression_name == "uncompressed") return arrow::Compression::UNCOMPRESSED;
		if (compression_name == "snappy") return arrow::Compression::SNAPPY;
		if (compression_name == "gzip") return arrow::Compression::GZIP;
		if (compression_name == "zstd") return arrow::Compression::ZSTD;
		if (compression_name == "brotli") return arrow::Compression::BROTLI;
		if (compression_name == "lz4") return arrow::Compression::LZ4;
		// Dead: parquet_write.f90's parquet_open_writer already validates compression= against the
		// exact same 6-codec list before ever calling into C++ (its own "unknown compression codec"
		// error stop), so this throw has no reachable caller through the public API.
		throw std::runtime_error("Unknown compression codec: " + compression_name); // GCOVR_EXCL_LINE
	}

	// Sets compression codec/level, row-group chunk size, and threading on `handle`.
	void parquet_set_writer_options(void *handle, const char *compression_name, int compression_level, int64_t chunk_size, int use_threads)
	{
		auto writer_handle = as_handle(handle);
		writer_handle->compression_codec = parse_compression_name(compression_name);
		writer_handle->compression_level = compression_level;
		writer_handle->chunk_size = chunk_size;
		writer_handle->use_threads = (use_threads != 0);
	}

	// Resizes Arrow's global CPU thread pool -- the single pool shared by
	// every reader/writer in this process that has use_threads enabled. This
	// is NOT a per-reader/per-writer setting: it takes effect immediately for
	// all concurrent Arrow work, so call it once (e.g. at program start),
	// before opening readers/writers on other threads, rather than from
	// multiple threads with different values.
	void parquet_set_max_threads(int n)
	{
		if (n < 1)
		{
			// Dead: parquet.f90's parquet_set_max_threads already does the identical "n < 1" check
			// itself (error stop "parquet_set_max_threads: n must be >= 1") before ever calling into
			// C, so this throw has no reachable caller through the public API.
			throw std::runtime_error("parquet_set_max_threads: n must be >= 1"); // GCOVR_EXCL_LINE
		}
		auto status = arrow::SetCpuThreadPoolCapacity(n);
		if (!status.ok())
		{ // GCOVR_EXCL_START -- untested compat backstop: no known way to force Arrow's global CPU
		  // thread pool resize to actually fail (no debug hook exists for it, unlike the
		  // file-I/O-style backstops elsewhere in this file) -- SetCpuThreadPoolCapacity's own
		  // documented failure modes (e.g. an invalid/negative capacity) are already excluded by
		  // the n < 1 check above.
			throw std::runtime_error(status.ToString());
		}
		// GCOVR_EXCL_STOP
	}

	// Reports the actually-linked Arrow library's runtime version (arrow::GetBuildInfo(),
	// not just what fpm compiled against) -- used by parquet.f90's parquet_get_version(mode="arrow").
	void parquet_get_arrow_version(int *major, int *minor, int *patch)
	{
		const auto &info = arrow::GetBuildInfo();
		*major = info.version_major;
		*minor = info.version_minor;
		*patch = info.version_patch;
	}

	// Reports the compile-time Parquet C++ version (PARQUET_VERSION_MAJOR/MINOR/PATCH macros --
	// parquet-cpp has no GetBuildInfo()-style runtime call). Parquet C++ ships in lockstep with
	// Arrow from the same monorepo release, so these macros track the linked library reliably.
	// Used by parquet.f90's parquet_get_version(mode="parquet").
	void parquet_get_parquet_version(int *major, int *minor, int *patch)
	{
		*major = PARQUET_VERSION_MAJOR;
		*minor = PARQUET_VERSION_MINOR;
		*patch = PARQUET_VERSION_PATCH;
	}

	// Opens the file and parses its footer/schema only -- no column's actual
	// data is read from disk here. FileReaderBuilder::Open/Build touch just
	// enough of the file to learn the schema and per-row-group metadata
	// (row counts, column chunk byte offsets); the metadata()->num_rows()
	// call below reads the row count directly from that same footer, again
	// without touching any column data. Actual column bytes are only ever
	// read on demand, in get_single_chunk_array, the first time that
	// specific column is asked for.
	void *create_parquet_reader(const char *filename, int use_threads)
	{
		auto *handle = new ParquetReaderHandle{};
		handle->filename = filename;
		auto infile_result = arrow::io::ReadableFile::Open(filename);
		if (!infile_result.ok())
		{
			delete handle; // GCOVR_EXCL_LINE -- same reasoning as create_parquet_writer's own: the report_fatal_error() below discards this whole process's gcov data.
			report_fatal_error("create_parquet_reader",
				std::string("failed to open '") + filename + "' for reading: " + infile_result.status().ToString()); // GCOVR_EXCL_LINE
		}
		auto infile = infile_result.ValueOrDie();
		parquet::arrow::FileReaderBuilder builder;
		auto status = builder.Open(infile);
		if (!status.ok())
		{
			delete handle; // GCOVR_EXCL_LINE -- same reasoning as create_parquet_writer's own: the report_fatal_error() below discards this whole process's gcov data.
			report_fatal_error("create_parquet_reader",
				std::string("failed to open '") + filename + "': " + status.ToString()); // GCOVR_EXCL_LINE
		}
		// Arrow's own default (kArrowDefaultUseThreads) is false; enabling
		// this lets Arrow decode a column's row groups (or several columns
		// requested together, e.g. via parquet_prefetch_columns) across its
		// internal thread pool instead of strictly single-threaded. Callers
		// can opt back out via parquet_open_reader(..., use_threads=.false.),
		// e.g. to avoid oversubscription when many OpenMP threads each hold
		// their own reader (see "Thread safety" in the README).
		parquet::ArrowReaderProperties reader_properties(/*use_threads=*/use_threads != 0);
		builder.properties(reader_properties);
		status = builder.Build(&handle->reader);
		if (!status.ok())
		{
			delete handle; // GCOVR_EXCL_LINE -- same reasoning as create_parquet_writer's own: the report_fatal_error() below discards this whole process's gcov data.
			report_fatal_error("create_parquet_reader",
				std::string("failed to open '") + filename + "': " + status.ToString()); // GCOVR_EXCL_LINE
		}

		status = handle->reader->GetSchema(&handle->schema);
		if (!status.ok())
		{
			delete handle; // GCOVR_EXCL_LINE -- same reasoning as create_parquet_writer's own: the report_fatal_error() below discards this whole process's gcov data.
			report_fatal_error("create_parquet_reader",
				std::string("failed to open '") + filename + "': " + status.ToString()); // GCOVR_EXCL_LINE
		}

		auto *file_metadata = handle->reader->parquet_reader()->metadata().get();
		status = parquet::arrow::SchemaManifest::Make(
			file_metadata->schema(), file_metadata->key_value_metadata(), reader_properties, &handle->manifest);
		if (!status.ok())
		{
			delete handle; // GCOVR_EXCL_LINE -- same reasoning as create_parquet_writer's own: the report_fatal_error() below discards this whole process's gcov data.
			report_fatal_error("create_parquet_reader",
				std::string("failed to open '") + filename + "': " + status.ToString()); // GCOVR_EXCL_LINE
		}

		handle->nrows = handle->reader->parquet_reader()->metadata()->num_rows();
		handle->total_nrows = handle->nrows;
		handle->num_row_groups = handle->reader->parquet_reader()->metadata()->num_row_groups();

		auto kv_metadata = handle->schema->metadata();
		if (kv_metadata)
		{
			for (int i = 0; i < kv_metadata->size(); ++i)
			{
				handle->table_metadata_cache.emplace_back(kv_metadata->key(i), kv_metadata->value(i));
			}
		}

		return handle;
	}

	// Closes `handle`, freeing the underlying reader object.
	void close_parquet_reader(void *handle)
	{
		auto reader_handle = as_reader_handle(handle);
		delete reader_handle.release();
	}

	// Warms the column_cache for `n` columns in a single Arrow call
	// (ReadTable with an explicit column-index list), instead of the
	// separate single-column ReadColumn calls get_single_chunk_array makes
	// lazily. With use_threads enabled (see create_parquet_reader), Arrow
	// can decode these columns in parallel across its thread pool. This is
	// purely additive: any column not passed here (or any column at all, if
	// this is never called) still gets read lazily, on demand, exactly as
	// before -- prefetching just means that read is already cached by the
	// time it's asked for. names_packed holds `n` fixed-width (`item_len`
	// bytes each) column names back-to-back, the same convention this
	// codebase already uses for packed Fortran string arrays elsewhere.
	// Columns already in column_cache (from an earlier prefetch_columns
	// call, or already read lazily) are skipped, so repeated calls with
	// partially or fully overlapping column sets accumulate a union of
	// cached columns rather than re-reading columns already cached.
	void parquet_reader_prefetch_columns(void *handle, const char *names_packed, int64_t item_len, int64_t n)
	{
		auto reader_handle = as_reader_handle(handle);
		if (n <= 0) return;

		// Columns already warmed by an earlier prefetch_columns (or by an
		// earlier lazy read via get_single_chunk_array) are skipped here, so
		// that repeated/overlapping prefetch_columns calls accumulate a
		// union of cached columns instead of re-reading and re-decoding
		// columns that are already in column_cache. A requested name may be a
		// dotted struct-leaf path: the physical read/cache-fill below is
		// always keyed by its top-level column (so two leaves under the same
		// struct only trigger one disk read), while qc -- run in a separate
		// pass at the end, once every physical read this call needs is done
		// -- runs once per distinct requested name, unwrapping to the leaf
		// first, so a leaf-specific qc rule still fires correctly.
		struct Requested
		{
			std::string name;
			int idx;
			std::vector<std::string> child_path;
		};
		std::vector<Requested> requested;
		requested.reserve(static_cast<size_t>(n));

		std::vector<int> indices;
		std::vector<std::string> top_names;
		for (int64_t i = 0; i < n; ++i)
		{
			std::string name(names_packed + i * item_len, static_cast<size_t>(item_len));
			name = trim_right_spaces_and_nuls(name);
			auto resolved = resolve_struct_path(reader_handle->schema, name);
			int idx = static_cast<int>(get_column_index(reader_handle, resolved.top_level_name.c_str()));
			reader_handle->was_prefetched.insert(idx);
			requested.push_back(Requested{name, idx, resolved.child_path});

			if (reader_handle->column_cache.find(idx) != reader_handle->column_cache.end()) continue;
			if (std::find(indices.begin(), indices.end(), idx) != indices.end()) continue;
			indices.push_back(idx);
			top_names.push_back(resolved.top_level_name);
		}

		if (!indices.empty())
		{
			// ReadTable's column_indices, unlike ReadColumn's, is indexed against Parquet's flat
			// leaf schema (see ParquetReaderHandle::manifest's own comment) -- collect every leaf
			// under each requested top-level field (one for a plain scalar/vector column, several
			// for a struct) into one combined list, so this stays a single batched/parallelizable
			// Arrow call regardless of how many of the requested columns are struct-typed.
			std::vector<int> leaf_indices;
			for (int idx : indices)
			{
				collect_leaf_indices(reader_handle->manifest.schema_fields[idx], leaf_indices);
			}

			auto table_result = reader_handle->reader->ReadTable(leaf_indices);
			if (!table_result.ok())
			{ // GCOVR_EXCL_START -- file-I/O backstop, not fixture-triggerable
				throw std::runtime_error(table_result.status().ToString());
			}
			// GCOVR_EXCL_STOP
			auto table = table_result.ValueOrDie();

			for (size_t i = 0; i < indices.size(); ++i)
			{
				// The result table always reconstructs one column per distinct top-level field
				// actually touched, in original schema order (regardless of leaf request order)
				// -- so its own field name (unique at the top level) is what maps a requested
				// column back to its result position, not a positional index into `indices`.
				auto result_pos = table->schema()->GetFieldIndex(top_names[i]);
				auto chunked = table->column(result_pos);
				auto array = apply_filter_mask(reader_handle, combine_column_chunks(chunked, top_names[i]));
				reader_handle->column_cache[indices[i]] = array;
			}
		}

		// Already decoded (by an earlier prefetch, read, filter evaluation, or the fresh reads
		// just above) -- still counts as "touched" by this prefetch call, so QC still runs for it
		// here (it may not have, e.g. if the only earlier touch was a plain get_col_size/
		// get_string_length query, which doesn't run QC itself).
		for (const auto &req : requested)
		{
			auto array = reader_handle->column_cache.at(req.idx);
			if (!req.child_path.empty()) array = unwrap_struct_path(array, req.child_path);
			run_qc_checks(reader_handle, req.name, req.name, array);
		}
	}

	// Same as parquet_reader_prefetch_columns, but for every column in the
	// file (used by parquet_open_reader(..., prefetch=.true.)) -- built from
	// the schema directly instead of a caller-supplied name list, since
	// Fortran has no way to enumerate column names itself. Must be called
	// AFTER parquet_reader_set_filter (if a filter is used): a column cached
	// here before filter_mask is set would stay raw/unfiltered forever, since
	// set_filter only re-masks the filter clauses' own columns, not the
	// whole column_cache -- see parquet_open_reader_base in parquet_read.f90
	// for the call-site ordering this depends on.
	void parquet_reader_prefetch_all_columns(void *handle)
	{
		auto reader_handle = as_reader_handle(handle);
		int num_fields = reader_handle->schema->num_fields();
		if (num_fields <= 0) return;

		std::vector<int> indices;
		std::vector<std::string> names;
		indices.reserve(static_cast<size_t>(num_fields));
		names.reserve(static_cast<size_t>(num_fields));
		for (int idx = 0; idx < num_fields; ++idx)
		{
			auto cached = reader_handle->column_cache.find(idx);
			if (cached != reader_handle->column_cache.end())
			{
				// Already decoded -- still counts as "touched" by this
				// prefetch, so QC still runs for it here, mirroring
				// parquet_reader_prefetch_columns's own cache-hit handling.
				run_qc_checks(reader_handle, reader_handle->schema->field(idx)->name(), reader_handle->schema->field(idx)->name(), cached->second);
				continue;
			}
			indices.push_back(idx);
			names.push_back(reader_handle->schema->field(idx)->name());
		}

		if (indices.empty()) return;

		// See parquet_reader_prefetch_columns's identical comment: ReadTable's column_indices
		// needs every leaf under each requested top-level field, not the top-level indices
		// themselves.
		std::vector<int> leaf_indices;
		for (int idx : indices)
		{
			collect_leaf_indices(reader_handle->manifest.schema_fields[idx], leaf_indices);
		}

		auto table_result = reader_handle->reader->ReadTable(leaf_indices);
		if (!table_result.ok())
		{ // GCOVR_EXCL_START -- file-I/O backstop, not fixture-triggerable
			throw std::runtime_error(table_result.status().ToString());
		}
		// GCOVR_EXCL_STOP
		auto table = table_result.ValueOrDie();

		for (size_t i = 0; i < indices.size(); ++i)
		{
			auto result_pos = table->schema()->GetFieldIndex(names[i]);
			auto chunked = table->column(result_pos);
			auto array = apply_filter_mask(reader_handle, combine_column_chunks(chunked, names[i]));
			reader_handle->column_cache[indices[i]] = array;
			run_qc_checks(reader_handle, names[i], names[i], array);
		}
	}

	// Returns `handle`'s post-filter row count.
	int64_t parquet_reader_get_nrows(void *handle)
	{
		auto reader_handle = as_reader_handle(handle);
		return reader_handle->nrows;
	}

	// The file's true, unfiltered row count (equal to parquet_reader_get_nrows's
	// result until a filter narrows it -- see total_nrows's own comment on
	// ParquetReaderHandle). Used only by parquet_get_nrows(..., check_positive=)
	// to report "N total" alongside a zero post-filter row count.
	int64_t parquet_reader_get_total_nrows(void *handle)
	{
		auto reader_handle = as_reader_handle(handle);
		return reader_handle->total_nrows;
	}

	// Returns `handle`'s row-group count (the file's physical row-group layout, unaffected by
	// any filter). See parquet_get_num_row_groups (parquet.f90).
	int64_t parquet_reader_get_num_row_groups(void *handle)
	{
		auto reader_handle = as_reader_handle(handle);
		return reader_handle->num_row_groups;
	}

	// True (1) if `handle` was opened with an active row filter -- parquet_read_column_chunk and
	// parquet_get_chunk_size(reader,...) are both disallowed on such a reader (see
	// get_row_group_chunk_array's own comment for why filtering doesn't compose with a
	// row-group-scoped read). Checked on the Fortran side (parquet_read.f90) so the resulting
	// error stop is clean and names the file, rather than a C++-level report_fatal_error.
	int parquet_reader_has_filter(void *handle)
	{
		auto reader_handle = as_reader_handle(handle);
		return reader_handle->filter_mask ? 1 : 0;
	}

	// Returns the physical row count of row group `row_group` (1-based; already resolved/
	// validated by the Fortran caller -- see parquet_get_chunk_size's reader specifics in
	// parquet_read.f90, which check row_group against parquet_reader_get_num_row_groups first).
	// Row groups are not guaranteed uniform, so this is a genuine per-row-group query, not a
	// single file-wide constant the way the writer side's resolved chunk_size is.
	int64_t parquet_reader_get_chunk_size_at(void *handle, int64_t row_group)
	{
		auto reader_handle = as_reader_handle(handle);
		return reader_handle->reader->parquet_reader()->metadata()->RowGroup(static_cast<int>(row_group - 1))->num_rows();
	}

	// Read-time QC support for parquet_open_reader(..., maml=, qc=). Every
	// field the qc-maml declared has already been validated and parsed on
	// the Fortran side (parquet_parse_qc_maml, parquet_metadata.f90) --
	// name/has_min/min_op/min_raw/has_max/max_op/max_raw/null_allowed, one
	// packed array each, `n` entries. A name that doesn't match any column
	// in this file is silently skipped (the qc-maml is explicitly allowed to
	// declare more fields than the file actually has); everything else is
	// stored as-is into qc_rules for run_qc_checks to use once this column
	// is actually touched (read, prefetched, or filtered). Always succeeds:
	// there's nothing left to validate here that Fortran hasn't already
	// checked, so this returns void unlike parquet_reader_set_filter.
	void parquet_reader_set_qc(void *handle,
		const char *names_packed, int64_t name_len,
		const int8_t *has_min_flags, const char *min_ops_packed, int64_t min_op_len,
		const char *min_values_packed, int64_t min_value_len,
		const int8_t *has_max_flags, const char *max_ops_packed, int64_t max_op_len,
		const char *max_values_packed, int64_t max_value_len,
		const int8_t *null_allowed_flags,
		int64_t n, int8_t qc_soft)
	{
		auto reader_handle = as_reader_handle(handle);
		reader_handle->qc_enabled = true;
		reader_handle->qc_soft = (qc_soft != 0);
		if (n <= 0) return;

		for (int64_t i = 0; i < n; ++i)
		{
			std::string name(names_packed + i * name_len, static_cast<size_t>(name_len));
			name = trim_right_spaces_and_nuls(name);

			// qc-maml may declare columns (plain or dotted struct-leaf paths) not present in
			// this file -- fine, just ignore them, same tolerance as the plain-column case.
			if (!struct_path_exists(reader_handle->schema, name)) continue;

			QcRule rule;
			rule.has_min = has_min_flags[i] != 0;
			if (rule.has_min)
			{
				std::string op(min_ops_packed + i * min_op_len, static_cast<size_t>(min_op_len));
				rule.min_op = trim_right_spaces_and_nuls(op);
				std::string raw(min_values_packed + i * min_value_len, static_cast<size_t>(min_value_len));
				rule.min_raw = trim_right_spaces_and_nuls(raw);
			}
			rule.has_max = has_max_flags[i] != 0;
			if (rule.has_max)
			{
				std::string op(max_ops_packed + i * max_op_len, static_cast<size_t>(max_op_len));
				rule.max_op = trim_right_spaces_and_nuls(op);
				std::string raw(max_values_packed + i * max_value_len, static_cast<size_t>(max_value_len));
				rule.max_raw = trim_right_spaces_and_nuls(raw);
			}
			rule.null_values_allowed = null_allowed_flags[i] != 0;

			reader_handle->qc_rules[name] = rule;
		}
	}

	// Row-filtering support for parquet_open_reader(..., filter=). Every
	// clause is a single "<column> <op> [value]" rule, already tokenized on
	// the Fortran side (parquet_tokenize_filter_rule) -- this is deliberately
	// NOT a general boolean-expression parser (no AND/OR/parens inside one
	// rule string): composing several rules is done by calling
	// parquet_filter%add more than once, which this function always ANDs
	// together. See parquet_reader_set_filter below for the overall flow.

	static std::string ascii_to_lower(const std::string &s)
	{
		std::string out = s;
		for (char &c : out) c = static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
		return out;
	}

	// Evaluates one filter clause against `array` (the filter column's own,
	// still-unfiltered decoded array -- filter_mask isn't set on the reader
	// yet while this runs), AND-ing the per-row result into `combined`
	// in place. Returns false (with `err` set) on any validation failure
	// (unknown/unsupported type for the clause's operator, unparseable
	// value, ...); the caller aborts the whole parquet_reader_set_filter
	// call in that case, same as an unknown filter column.
	static bool eval_filter_clause(const std::shared_ptr<arrow::Array> &array, const std::string &colname,
		const std::string &op, bool is_string, const std::string &value_text,
		std::vector<uint8_t> &combined, std::string &err)
	{
		int64_t n = array->length();

		if (op == "is_null" || op == "is_not_null")
		{
			bool want_null = (op == "is_null");
			for (int64_t i = 0; i < n; ++i)
			{
				bool ok = want_null ? array->IsNull(i) : array->IsValid(i);
				combined[static_cast<size_t>(i)] = combined[static_cast<size_t>(i)] && ok;
			}
			return true;
		}

		switch (array->type_id())
		{
		case arrow::Type::INT32:
		case arrow::Type::INT64:
		case arrow::Type::INT8:
		case arrow::Type::INT16:
		case arrow::Type::UINT8:
		case arrow::Type::UINT16:
		case arrow::Type::UINT32:
		{
			int64_t parsed;
			if (is_string || !parse_int64_strict(value_text, parsed))
			{
				err = "value '" + value_text + "' is not a valid integer for column '" + colname + "'";
				return false;
			}
			if (array->type_id() == arrow::Type::INT32)
			{
				if (parsed < std::numeric_limits<int32_t>::min() || parsed > std::numeric_limits<int32_t>::max())
				{
					err = "value '" + value_text + "' is out of int32 range for column '" + colname + "'";
					return false;
				}
				auto arr = std::static_pointer_cast<arrow::Int32Array>(array);
				int32_t v = static_cast<int32_t>(parsed);
				for (int64_t i = 0; i < n; ++i)
				{
					bool ok = !arr->IsNull(i) && compare_op<int32_t>(arr->Value(i), v, op);
					combined[static_cast<size_t>(i)] = combined[static_cast<size_t>(i)] && ok;
				}
			}
			else if (array->type_id() == arrow::Type::INT64)
			{
				auto arr = std::static_pointer_cast<arrow::Int64Array>(array);
				for (int64_t i = 0; i < n; ++i)
				{
					bool ok = !arr->IsNull(i) && compare_op<int64_t>(arr->Value(i), parsed, op);
					combined[static_cast<size_t>(i)] = combined[static_cast<size_t>(i)] && ok;
				}
			}
			else
			{
				// INT8/INT16/UINT8/UINT16/UINT32: every value widens into
				// int64_t exactly, so compare directly with no extra range
				// pre-check (same as the plain INT64 branch above).
				for (int64_t i = 0; i < n; ++i)
				{
					bool ok = !array->IsNull(i) && compare_op<int64_t>(small_integer_value_at(array, i), parsed, op);
					combined[static_cast<size_t>(i)] = combined[static_cast<size_t>(i)] && ok;
				}
			}
			return true;
		}
		case arrow::Type::FLOAT:
		case arrow::Type::DOUBLE:
		case arrow::Type::HALF_FLOAT:
		case arrow::Type::UINT64:
		case arrow::Type::DECIMAL32:
		case arrow::Type::DECIMAL64:
		case arrow::Type::DECIMAL128:
		case arrow::Type::DECIMAL256:
		{
			double parsed;
			if (is_string || !parse_double_strict(value_text, parsed))
			{
				err = "value '" + value_text + "' is not a valid number for column '" + colname + "'";
				return false;
			}
			if (array->type_id() == arrow::Type::FLOAT || array->type_id() == arrow::Type::DOUBLE ||
				array->type_id() == arrow::Type::HALF_FLOAT)
			{
				for (int64_t i = 0; i < n; ++i)
				{
					bool ok = !array->IsNull(i) && compare_op<double>(real_family_value_at(array, i), parsed, op);
					combined[static_cast<size_t>(i)] = combined[static_cast<size_t>(i)] && ok;
				}
			}
			else if (array->type_id() == arrow::Type::UINT64)
			{
				auto arr = std::static_pointer_cast<arrow::UInt64Array>(array);
				for (int64_t i = 0; i < n; ++i)
				{
					bool ok = !arr->IsNull(i) && compare_op<double>(static_cast<double>(arr->Value(i)), parsed, op);
					combined[static_cast<size_t>(i)] = combined[static_cast<size_t>(i)] && ok;
				}
			}
			else
			{
				// DECIMAL32/64/128/256.
				for (int64_t i = 0; i < n; ++i)
				{
					bool ok = !array->IsNull(i) && compare_op<double>(decimal_value_at(array, i), parsed, op);
					combined[static_cast<size_t>(i)] = combined[static_cast<size_t>(i)] && ok;
				}
			}
			return true;
		}
		case arrow::Type::BOOL:
		{
			if (op != "==" && op != "/=")
			{
				err = "ordering comparisons ('>', '>=', '<', '<=') are not supported for boolean column '" + colname + "'";
				return false;
			}
			if (is_string)
			{
				err = "value for boolean column '" + colname + "' must be true or false (unquoted)";
				return false;
			}
			std::string lowered = ascii_to_lower(value_text);
			bool bval;
			if (lowered == "true") bval = true;
			else if (lowered == "false") bval = false;
			else
			{
				err = "value '" + value_text + "' is not true/false for boolean column '" + colname + "'";
				return false;
			}
			auto arr = std::static_pointer_cast<arrow::BooleanArray>(array);
			for (int64_t i = 0; i < n; ++i)
			{
				bool ok = !arr->IsNull(i) && ((op == "==") ? (arr->Value(i) == bval) : (arr->Value(i) != bval));
				combined[static_cast<size_t>(i)] = combined[static_cast<size_t>(i)] && ok;
			}
			return true;
		}
		case arrow::Type::STRING:
		case arrow::Type::LARGE_STRING:
		case arrow::Type::STRING_VIEW:
		{
			if (!is_string)
			{
				err = "value for string column '" + colname + "' must be double-quoted";
				return false;
			}
			auto acc = make_string_like_accessor(array);
			for (int64_t i = 0; i < n; ++i)
			{
				bool ok = !acc.is_null(i) && compare_op<std::string>(std::string(acc.get_view(i)), value_text, op);
				combined[static_cast<size_t>(i)] = combined[static_cast<size_t>(i)] && ok;
			}
			return true;
		}
		default:
			err = "column '" + colname + "' has a type that filtering does not support";
			return false;
		}
	}

	// Validates and applies a set of AND-combined filter clauses to this
	// reader: every referenced column must exist and be a plain scalar
	// column (col_size == 1; a vector/list column always fails, regardless
	// of its size). On success, updates nrows to the filtered row count,
	// stores the resulting mask on the handle (so every column decoded from
	// here on -- via get_single_chunk_array or parquet_reader_prefetch_columns
	// -- is filtered to just the matching rows), and re-filters/updates
	// column_cache for every filter column itself (already decoded above,
	// as a side effect of evaluating its own clause) so it's consistent with
	// every other column. Returns 0 on success; on failure, returns 1 and
	// writes a human-readable reason into err_out (truncated to err_cap).
	int64_t parquet_reader_set_filter(void *handle,
		const char *names_packed, int64_t name_len,
		const char *ops_packed, int64_t op_len,
		const char *values_packed, int64_t value_len,
		const int8_t *is_string_flags,
		int64_t n,
		char *err_out, int64_t err_cap)
	{
		auto reader_handle = as_reader_handle(handle);
		if (n <= 0) return 0;

		std::vector<uint8_t> combined(static_cast<size_t>(reader_handle->total_nrows), 1);
		std::vector<int> touched_indices;
		std::vector<std::string> touched_names; // every filter clause's own (possibly dotted) name, deduplicated

		for (int64_t i = 0; i < n; ++i)
		{
			std::string name(names_packed + i * name_len, static_cast<size_t>(name_len));
			name = trim_right_spaces_and_nuls(name);
			std::string op(ops_packed + i * op_len, static_cast<size_t>(op_len));
			op = trim_right_spaces_and_nuls(op);
			std::string value(values_packed + i * value_len, static_cast<size_t>(value_len));
			value = trim_right_spaces_and_nuls(value);
			bool is_string = is_string_flags[i] != 0;

			if (!struct_path_exists(reader_handle->schema, name))
			{
				std::snprintf(err_out, static_cast<size_t>(err_cap), "unknown column in filter: %s", name.c_str());
				return 1;
			}

			std::shared_ptr<arrow::Array> array;
			try
			{
				array = get_single_chunk_array(reader_handle, name.c_str());
			}
			// GCOVR_EXCL_START -- not fixture-triggerable in practice: parquet_read.f90's
			// prefetch_filter_columns unconditionally prefetches every filter column that
			// exists in the schema (via a separate ReadTable call) before parquet_apply_filter
			// ever calls into parquet_reader_set_filter, so this get_single_chunk_array call
			// always hits an already-cached column -- its own uncached-path ReadColumn failure
			// (the only throw that could reach here) is itself already excluded as a file-I/O
			// backstop with no fixture that triggers it (see get_single_chunk_array's own
			// comment). A debug-hook attempt to force an artificial throw here (to at least
			// verify the catch itself works) was tried and reverted: even an unconditional throw
			// placed at function entry, called from directly inside this try block, escaped
			// uncaught -- reproducing the same C++ exception-unwinding unreliability already
			// documented on struct_path_exists's own comment for this project's mixed
			// gfortran-driven static-library link, this time for parquet_reader_set_filter's own
			// try/catch (otherwise the one demonstrably-working catch block in this file). The
			// catch clause itself is included in this exclusion (not just its body): under GCC, a
			// catch clause never entered by any covered test shows uncovered in its own right,
			// distinct from Clang's gcov.
			catch (const std::exception &e)
			{
				std::snprintf(err_out, static_cast<size_t>(err_cap), "failed to read filter column '%s': %s", name.c_str(), e.what());
				return 1;
			}
			// GCOVR_EXCL_STOP

			if (array->type_id() == arrow::Type::FIXED_SIZE_LIST || array->type_id() == arrow::Type::LIST)
			{
				std::snprintf(err_out, static_cast<size_t>(err_cap),
					"filter column '%s' is a vector column; filtering only supports scalar columns", name.c_str());
				return 1;
			}

			auto resolved = resolve_struct_path(reader_handle->schema, name);
			int idx = static_cast<int>(get_column_index(reader_handle, resolved.top_level_name.c_str()));
			if (std::find(touched_indices.begin(), touched_indices.end(), idx) == touched_indices.end())
			{
				touched_indices.push_back(idx);
			}
			if (std::find(touched_names.begin(), touched_names.end(), name) == touched_names.end())
			{
				touched_names.push_back(name);
			}

			std::string err;
			if (!eval_filter_clause(array, name, op, is_string, value, combined, err))
			{
				// Tag the clause-level message so it is unambiguously a row-filter
				// error (vs a read-time qc check, which labels its own messages).
				// The other set_filter failures (unknown/vector/read-fail column)
				// already say "filter" themselves, so they aren't tagged again here.
				std::snprintf(err_out, static_cast<size_t>(err_cap), "filter rule: %s", err.c_str());
				return 1;
			}

			// Retain this clause (operator+value, column name stripped) for the
			// print_stat "filter" column; multiple clauses on one column are kept
			// in order and joined with ", " at print time. Keyed by the physical
			// top-level column index -- two clauses on different leaves of the
			// same struct show up merged under that struct's one print_stat row
			// (see CLAUDE.md's nested-struct-field design notes; an accepted v1
			// limitation, same as was_read/output_type_used above).
			reader_handle->filter_clauses[idx].push_back(value.empty() ? op : op + value);
		}

		arrow::BooleanBuilder mask_builder;
		auto append_status = mask_builder.AppendValues(combined.data(), static_cast<int64_t>(combined.size()));
		if (!append_status.ok())
		{ // GCOVR_EXCL_START -- BooleanBuilder allocation backstop, not fixture-triggerable
			std::snprintf(err_out, static_cast<size_t>(err_cap), "failed to build filter mask: %s", append_status.ToString().c_str());
			return 1;
		}
		// GCOVR_EXCL_STOP
		std::shared_ptr<arrow::Array> mask_array;
		auto finish_status = mask_builder.Finish(&mask_array);
		if (!finish_status.ok())
		{ // GCOVR_EXCL_START -- BooleanBuilder allocation backstop, not fixture-triggerable
			std::snprintf(err_out, static_cast<size_t>(err_cap), "failed to build filter mask: %s", finish_status.ToString().c_str());
			return 1;
		}
		// GCOVR_EXCL_STOP
		reader_handle->filter_mask = std::static_pointer_cast<arrow::BooleanArray>(mask_array);

		int64_t matched = 0;
		for (uint8_t v : combined) matched += (v != 0);
		reader_handle->nrows = matched;

		// Every filter column was decoded (and cached) above, before
		// filter_mask existed -- re-filter those specific cache entries now
		// so they're consistent with every other column, which will only
		// ever see the filtered version (via apply_filter_mask, from here on).
		// Re-filtering is keyed by physical top-level index (touched_indices,
		// deduplicated) so a struct column shared by two filtered leaves is
		// only ever filtered once; qc is then re-run separately, once per
		// distinct requested name (touched_names), unwrapping to the leaf
		// first where needed, so a leaf-specific qc rule still fires
		// correctly against the now-filtered data.
		ensure_compute_initialized();
		for (int idx : touched_indices)
		{
			auto it = reader_handle->column_cache.find(idx);
			if (it == reader_handle->column_cache.end()) continue;
			auto coerced = coerce_for_filter_kernel(it->second);
			if (!coerced.ok())
			{ // GCOVR_EXCL_START -- Cast-kernel Status backstop on already-validated input
				std::snprintf(err_out, static_cast<size_t>(err_cap), "failed to apply filter: %s", coerced.status().ToString().c_str());
				return 1;
			}
			// GCOVR_EXCL_STOP
			auto filtered = arrow::compute::Filter(coerced.ValueOrDie(), reader_handle->filter_mask);
			if (!filtered.ok())
			{ // GCOVR_EXCL_START -- Filter-kernel Status backstop on already-validated input
				std::snprintf(err_out, static_cast<size_t>(err_cap), "failed to apply filter: %s", filtered.status().ToString().c_str());
				return 1;
			}
			// GCOVR_EXCL_STOP
			it->second = filtered.ValueOrDie().make_array();
			reader_handle->was_prefetched.insert(idx);
		}

		for (const auto &touched_name : touched_names)
		{
			auto resolved = resolve_struct_path(reader_handle->schema, touched_name);
			auto idx = static_cast<int>(get_column_index(reader_handle, resolved.top_level_name.c_str()));
			auto array = reader_handle->column_cache.at(idx);
			if (!resolved.child_path.empty()) array = unwrap_struct_path(array, resolved.child_path);
			run_qc_checks(reader_handle, touched_name, touched_name, array);
		}

		return 0;
	}

	// Non-throwing existence check, so callers (parquet_prefetch_columns) can
	// validate names up front and report a clean Fortran-side error stop,
	// instead of letting get_column_index's std::runtime_error escape
	// uncaught across the Fortran/C++ boundary. `name` may be a dotted
	// struct-leaf path (see resolve_struct_path) -- this returns true only if
	// it resolves all the way down to a readable leaf, not merely to an
	// intermediate struct.
	int64_t parquet_reader_has_column(void *handle, const char *name)
	{
		auto reader_handle = as_reader_handle(handle);
		return struct_path_exists(reader_handle->schema, name) ? 1 : 0;
	}

	// Writes `name`'s canonical data-type token ("int32"/"int64"/"float32"/"float64"/"boolean"/
	// "string"/"date"/"time"/"timestamp") into `buf` (space-padded to buf_len) and returns 1, if
	// its physical Arrow type maps onto one of those nine tokens -- a FIXED_SIZE_LIST/LIST/
	// LARGE_LIST vector column is unwrapped to its element type first, so an int32 vector column
	// reports "int32" here too (col_size/vector-ness is a separate query, see
	// parquet_reader_get_column_col_size). Otherwise writes the raw Arrow type description (e.g.
	// "decimal128(10, 2)") into `buf` and returns 0, for a caller-side diagnostic message -- this
	// project's own writer never produces such a column, but a column written by a different tool
	// can (see test/fixtures/extended_types.parquet). Assumes `name` already resolves: callers
	// (parquet_column_exists/parquet_get_column_type in parquet_read.f90) always probe existence
	// via parquet_reader_has_column/check_column_exists first.
	int64_t parquet_reader_get_column_type_name(void *handle, const char *name, char *buf, int64_t buf_len)
	{
		auto reader_handle = as_reader_handle(handle);
		auto resolved = resolve_struct_path(reader_handle->schema, name);
		auto type = resolved.leaf_field->type();
		if (type->id() == arrow::Type::FIXED_SIZE_LIST || type->id() == arrow::Type::LIST ||
			type->id() == arrow::Type::LARGE_LIST)
		{
			type = type->field(0)->type();
		}
		std::string token;
		switch (type->id())
		{
		case arrow::Type::INT32: token = "int32"; break;
		case arrow::Type::INT64: token = "int64"; break;
		case arrow::Type::FLOAT: token = "float32"; break;
		case arrow::Type::DOUBLE: token = "float64"; break;
		case arrow::Type::BOOL: token = "boolean"; break;
		case arrow::Type::STRING: token = "string"; break;
		case arrow::Type::LARGE_STRING: token = "string"; break;
		case arrow::Type::DATE32: token = "date"; break;
		case arrow::Type::DATE64: token = "date"; break; // GCOVR_EXCL_LINE -- DATE64 never actually produced (see CLAUDE.md's temporal notes).
		case arrow::Type::TIME32: token = "time"; break;
		case arrow::Type::TIME64: token = "time"; break;
		case arrow::Type::TIMESTAMP: token = "timestamp"; break;
		default:
			copy_string_with_padding(buf, buf_len, type->ToString());
			return 0;
		}
		copy_string_with_padding(buf, buf_len, token);
		return 1;
	}

	// Returns the declared vector-column element count of `name` (0 for a scalar column),
	// without reading any column data for the common FIXED_SIZE_LIST case. A FIXED_SIZE_LIST
	// column's width is a schema-level constant (arrow::FixedSizeListType::list_size()), so it's
	// read straight off the already in-memory schema -- the same schema-only introspection
	// pattern max_fixed_size_list_col_size/check_explicit_chunk_size_fits_arrow_limit use on the
	// write side. This is what lets col_size be queried on a multi-billion-element column
	// without ever materializing it (see get_single_chunk_array's whole-column read, and the
	// int32 element-count ceiling documented on parquet_reader_get_column_total_elements below
	// and in CLAUDE.md's "Guarding a hard Arrow int32-only ceiling"). A plain LIST/LARGE_LIST
	// column (only ever produced by a non-this-library writer -- this library always writes
	// FIXED_SIZE_LIST for vector columns) has no such schema-level constant, since its per-row
	// width can vary; that case still falls back to get_col_size's own data-scanning heuristic
	// via get_single_chunk_array.
	int64_t parquet_reader_get_column_col_size(void *handle, const char *name)
	{
		auto reader_handle = as_reader_handle(handle);
		auto resolved = resolve_struct_path(reader_handle->schema, name);
		if (resolved.leaf_field->type()->id() == arrow::Type::FIXED_SIZE_LIST)
		{
			return static_cast<int64_t>(std::static_pointer_cast<arrow::FixedSizeListType>(resolved.leaf_field->type())->list_size());
		}
		auto array = get_single_chunk_array(reader_handle, name);
		return get_col_size(array);
	}

	// Returns the total element count (nrows * col_size) of vector column `name`, without
	// reading any column data for the common FIXED_SIZE_LIST case (see
	// parquet_reader_get_column_col_size, above) -- reader_handle->nrows already comes from the
	// file footer (create_parquet_reader), so this needs no column data read at all in that
	// case. This is the fix for a whole-column read (the old get_single_chunk_array-based
	// implementation, still used below for the non-FIXED_SIZE_LIST fallback) throwing Arrow's
	// "List index overflow" once nrows * col_size exceeds int32, purely to answer a size query --
	// see CLAUDE.md's "Guarding a hard Arrow int32-only ceiling" for the underlying limit (write
	// side only there; this is the read-side counterpart).
	int64_t parquet_reader_get_column_total_elements(void *handle, const char *name)
	{
		auto reader_handle = as_reader_handle(handle);
		auto resolved = resolve_struct_path(reader_handle->schema, name);
		if (resolved.leaf_field->type()->id() == arrow::Type::FIXED_SIZE_LIST)
		{
			auto col_size = static_cast<int64_t>(std::static_pointer_cast<arrow::FixedSizeListType>(resolved.leaf_field->type())->list_size());
			return reader_handle->nrows * col_size;
		}
		auto nrows = reader_handle->nrows;
		// Calls the same static helpers parquet_reader_get_column_col_size
		// itself uses, rather than that exported function directly -- going
		// through the exported function would re-enter as_reader_handle on
		// the same handle while this call's own guard is still held, which
		// the (deliberately non-reentrant) ConcurrencyGuard always rejects.
		auto array = get_single_chunk_array(reader_handle, name);
		auto asize = get_col_size(array);
		return nrows * asize;
	}

	// Returns the longest non-null string value actually present in string column `name`.
	int64_t parquet_reader_get_string_length(void *handle, const char *name)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		int64_t max_len = 0;

		// Sizing a read buffer doesn't depend on the caller's null policy
		// (only the actual value read, elsewhere, does) -- so this always
		// just reports the longest non-null string, silently ignoring
		// Nulls, regardless of whether the eventual parquet_read_column
		// call will be given null_value/is_valid or not.
		if (is_string_like_type(array->type_id()))
		{
			auto acc = make_string_like_accessor(array);
			for (int64_t i = 0; i < acc.length; ++i)
			{
				if (acc.is_null(i)) continue;
				max_len = std::max(max_len, static_cast<int64_t>(acc.get_view(i).size()));
			}
			return max_len;
		}

		std::shared_ptr<arrow::Array> list_values;
		if (array->type_id() == arrow::Type::FIXED_SIZE_LIST)
		{
			list_values = std::static_pointer_cast<arrow::FixedSizeListArray>(array)->values();
		}
		else if (array->type_id() == arrow::Type::LIST)
		{
			list_values = std::static_pointer_cast<arrow::ListArray>(array)->values();
		}
		else if (array->type_id() == arrow::Type::LARGE_LIST)
		{
			list_values = std::static_pointer_cast<arrow::LargeListArray>(array)->values();
		}
		else
		{ // GCOVR_EXCL_START -- this throw is never caught anywhere in the call chain, so it crosses
		  // the extern "C" boundary uncaught -> std::terminate() -> abort, discarding that whole
		  // process's gcov coverage; tested via scenario_string_length_on_non_string_column
			throw std::runtime_error(std::string("Column is not string-like: ") + name);
		}
		// GCOVR_EXCL_STOP

		auto vals = make_string_like_accessor(list_values);
		for (int64_t i = 0; i < vals.length; ++i)
		{
			if (vals.is_null(i)) continue;
			max_len = std::max(max_len, static_cast<int64_t>(vals.get_view(i).size()));
		}
		return max_len;
	}

	// parquet_get_metadata (parquet_metadata.f90) reads the whole
	// table_metadata_cache once, right after parquet_open_reader, via these
	// four accessors -- length-then-copy, the same two-step convention
	// parquet_reader_get_string_length/parquet_read_string_column already use
	// for variable-length strings. `index` is 0-based.
	int64_t parquet_reader_get_table_metadata_count(void *handle)
	{
		auto reader_handle = as_reader_handle(handle);
		return static_cast<int64_t>(reader_handle->table_metadata_cache.size());
	}

	// Returns the (key, value) pair at `index` in the reader's table_metadata_cache, or aborts
	// via report_fatal_error(context, ...) if index is out of range.
	static const std::pair<std::string, std::string> &table_metadata_entry_at(
		ParquetReaderHandle *reader_handle, int64_t index, const char *context)
	{
		if (index < 0 || index >= static_cast<int64_t>(reader_handle->table_metadata_cache.size()))
		{
			report_fatal_error(context, "table metadata index out of range");
		}
		return reader_handle->table_metadata_cache[static_cast<size_t>(index)];
	} // GCOVR_EXCL_LINE -- gcov attribution artifact: this closing brace shows uncovered even though the covered `return` above proves the body ran.

	// Returns the byte length of table metadata entry `index`'s key.
	int64_t parquet_reader_get_table_metadata_key_length(void *handle, int64_t index)
	{
		auto reader_handle = as_reader_handle(handle);
		return static_cast<int64_t>(
			table_metadata_entry_at(reader_handle, index, "parquet_reader_get_table_metadata_key_length").first.size());
	}

	// Returns the byte length of table metadata entry `index`'s value.
	int64_t parquet_reader_get_table_metadata_value_length(void *handle, int64_t index)
	{
		auto reader_handle = as_reader_handle(handle);
		return static_cast<int64_t>(
			table_metadata_entry_at(reader_handle, index, "parquet_reader_get_table_metadata_value_length").second.size());
	}

	// Copies table metadata entry `index`'s key into `buf`.
	void parquet_reader_get_table_metadata_key(void *handle, int64_t index, char *buf, int64_t buf_len)
	{
		auto reader_handle = as_reader_handle(handle);
		const auto &entry = table_metadata_entry_at(reader_handle, index, "parquet_reader_get_table_metadata_key");
		copy_string_with_padding(buf, buf_len, entry.first);
	}

	// Copies table metadata entry `index`'s value into `buf`.
	void parquet_reader_get_table_metadata_value(void *handle, int64_t index, char *buf, int64_t buf_len)
	{
		auto reader_handle = as_reader_handle(handle);
		const auto &entry = table_metadata_entry_at(reader_handle, index, "parquet_reader_get_table_metadata_value");
		copy_string_with_padding(buf, buf_len, entry.second);
	}

	// Returns a short human-readable type description for parquet_reader_print_stat,
	// e.g. "list<double>" for a vector column, else the plain Arrow type name.
	static std::string describe_parquet_type(const std::shared_ptr<arrow::Field> &field)
	{
		auto type = field->type();
		if (type->id() == arrow::Type::FIXED_SIZE_LIST || type->id() == arrow::Type::LIST)
		{
			return std::string("list<") + type->field(0)->type()->ToString() + ">";
		}
		return type->ToString();
	}

	// Extracts a min/max compute result's scalar as display text -- integers
	// print as plain integers (no trailing ".0"/spurious precision), floats
	// are trimmed to 6 significant digits, and anything else (e.g. a string
	// scalar) falls back to Arrow's own Scalar::ToString().
	static std::string format_stat_scalar(const std::shared_ptr<arrow::Scalar> &s)
	{
		switch (s->type->id())
		{
		case arrow::Type::INT32:
			return std::to_string(std::static_pointer_cast<arrow::Int32Scalar>(s)->value);
		case arrow::Type::INT64:
			return std::to_string(std::static_pointer_cast<arrow::Int64Scalar>(s)->value);
		case arrow::Type::FLOAT:
			return format_stat_double(static_cast<double>(std::static_pointer_cast<arrow::FloatScalar>(s)->value));
		case arrow::Type::DOUBLE:
			return format_stat_double(std::static_pointer_cast<arrow::DoubleScalar>(s)->value);
		case arrow::Type::STRING:
			return "\"" + std::static_pointer_cast<arrow::StringScalar>(s)->value->ToString() + "\"";
		case arrow::Type::LARGE_STRING:
			return "\"" + std::static_pointer_cast<arrow::LargeStringScalar>(s)->value->ToString() + "\"";
			// GCOVR_EXCL_START -- confirmed unreachable, not just untested: arrow::compute::MinMax
			// has no STRING_VIEW kernel registered in this Arrow build -- verified directly (a
			// passing STRING_VIEW+print_stat=.true. scenario's own printed table shows "-"/"-" for
			// min/max, meaning compute_stat_min_max's MinMax call itself already failed and
			// returned before format_stat_scalar could ever be reached with a STRING_VIEW scalar).
			// The case label itself is included in this exclusion (not just the body): under GCC, a
			// case label reachable only via a permanently-unreachable path shows uncovered in its
			// own right, distinct from Clang's gcov.
		case arrow::Type::STRING_VIEW:
			return "\"" + std::static_pointer_cast<arrow::StringViewScalar>(s)->value->ToString() + "\"";
			// GCOVR_EXCL_STOP
		default:
			return s->ToString();
		}
	}

	// Computes this column's min/max (via Arrow's own "min_max" compute
	// function, on the flattened element array for a vector column) for
	// every supported scalar type except boolean, which is reported as
	// True/False counts instead (see the boolean branch in
	// parquet_reader_print_stat) -- a boolean's min/max is always exactly
	// one of False/True/False-and-True and isn't a meaningful summary.
	// Returns false (leaving min_out/max_out untouched) if every element is
	// Null, since MinMax then has nothing to report.
	static bool compute_stat_min_max(const std::shared_ptr<arrow::Array> &array, std::string &min_out, std::string &max_out)
	{
		ensure_compute_initialized();

		auto result = arrow::compute::MinMax(array);
		if (!result.ok()) return false;
		auto struct_scalar = std::static_pointer_cast<arrow::StructScalar>(result.ValueOrDie().scalar());
		auto min_scalar = struct_scalar->field("min").ValueOrDie();
		auto max_scalar = struct_scalar->field("max").ValueOrDie();
		if (!min_scalar->is_valid || !max_scalar->is_valid) return false;
		min_out = format_stat_scalar(min_scalar);
		max_out = format_stat_scalar(max_scalar);
		return true;
	}

	// Prints a debug/diagnostic summary of this reader's activity to stdout:
	// table-level info (filename, total column/row counts) plus one row per
	// column that was either prefetched (parquet_reader_prefetch_columns) or
	// actually read (any parquet_read_* call) -- untouched columns are left
	// out entirely, rather than forcing a decode of data nobody asked for
	// just to fill in a report. Called from parquet_close_reader(print_stat=.true.)
	// before the underlying reader is closed/deleted.
	void parquet_reader_print_stat(void *handle)
	{
		auto reader_handle = as_reader_handle(handle);

		std::vector<int> touched;
		for (int idx : reader_handle->was_prefetched) touched.push_back(idx);
		for (int idx : reader_handle->was_read)
		{
			if (std::find(touched.begin(), touched.end(), idx) == touched.end()) touched.push_back(idx);
		}
		std::sort(touched.begin(), touched.end());

		std::vector<std::string> headers = {
			"col", "parquet_type", "output_type", "col_size", "len_str",
			"nulls", "min", "max", "qcmin", "qcmax", "qcmiss", "fetched", "read", "filter"};
		std::vector<std::vector<std::string>> rows;

		for (int idx : touched)
		{
			auto field = reader_handle->schema->field(idx);
			auto array = reader_handle->column_cache.at(idx);
			auto flat = flatten_for_stats(array);

			std::string col_size_str;
			auto col_size = get_col_size(array);
			if (col_size > 1) col_size_str = std::to_string(col_size);

			std::string len_str_str;
			if (is_string_like_type(flat->type_id()))
			{
				auto acc = make_string_like_accessor(flat);
				int64_t max_len = 0;
				for (int64_t i = 0; i < acc.length; ++i)
				{
					if (acc.is_null(i)) continue;
					max_len = std::max(max_len, static_cast<int64_t>(acc.get_view(i).size()));
				}
				len_str_str = std::to_string(max_len);
				auto out_len = reader_handle->output_str_len_used.find(idx);
				if (out_len != reader_handle->output_str_len_used.end())
				{
					len_str_str += " / " + std::to_string(out_len->second);
				}
			}

			std::string nulls_str = std::to_string(flat->null_count());

			std::string min_str, max_str;
			if (flat->type_id() == arrow::Type::BOOL)
			{
				auto barr = std::static_pointer_cast<arrow::BooleanArray>(flat);
				int64_t n_true = 0, n_false = 0;
				for (int64_t i = 0; i < barr->length(); ++i)
				{
					if (barr->IsNull(i)) continue;
					if (barr->Value(i)) ++n_true; else ++n_false;
				}
				min_str = "T:" + std::to_string(n_true);
				max_str = "F:" + std::to_string(n_false);
			}
			else if (!compute_stat_min_max(flat, min_str, max_str))
			{
				min_str = "-";
				max_str = "-";
			}

			auto output_type_it = reader_handle->output_type_used.find(idx);
			std::string output_type_str = output_type_it != reader_handle->output_type_used.end() ? output_type_it->second : "";

			// qc bound/miss declarations for this column (blank when none),
			// shown operator+value like the maml declared them (e.g. ">=0").
			std::string qcmin_str, qcmax_str, qcmiss_str;
			auto qc_it = reader_handle->qc_rules.find(field->name());
			if (qc_it != reader_handle->qc_rules.end())
			{
				const QcRule &rule = qc_it->second;
				if (rule.has_min) qcmin_str = rule.min_op + rule.min_raw;
				if (rule.has_max) qcmax_str = rule.max_op + rule.max_raw;
				if (rule.null_values_allowed) qcmiss_str = "Null";
			}

			// Filter clauses on this column (blank when none), column name
			// stripped, joined with ", " -- e.g. ">=0.0, <360.0".
			std::string filter_str;
			auto flt_it = reader_handle->filter_clauses.find(idx);
			if (flt_it != reader_handle->filter_clauses.end())
			{
				for (size_t i = 0; i < flt_it->second.size(); ++i)
				{
					if (i) filter_str += ", ";
					filter_str += flt_it->second[i];
				}
			}

			rows.push_back({
				field->name(),
				describe_parquet_type(field),
				output_type_str,
				col_size_str,
				len_str_str,
				nulls_str,
				min_str,
				max_str,
				qcmin_str,
				qcmax_str,
				qcmiss_str,
				reader_handle->was_prefetched.count(idx) ? "yes" : "no",
				reader_handle->was_read.count(idx) ? "yes" : "no",
				filter_str,
			});
		}

		std::vector<size_t> widths;
		for (const auto &h : headers) widths.push_back(h.size());
		for (const auto &row : rows)
		{
			for (size_t i = 0; i < row.size(); ++i) widths[i] = std::max(widths[i], row[i].size());
		}

		auto print_row = [&](const std::vector<std::string> &row)
		{
			std::string line;
			for (size_t i = 0; i < row.size(); ++i)
			{
				line += row[i];
				line.append(widths[i] - row[i].size(), ' ');
				if (i + 1 < row.size()) line += "  ";
			}
			std::fprintf(stdout, "%s\n", line.c_str());
		};

		std::fprintf(stdout, "=== parquet_reader stats ===\n");
		std::fprintf(stdout, "file: %s\n", reader_handle->filename.c_str());
		if (reader_handle->nrows != reader_handle->total_nrows)
		{
			// GCOVR_EXCL_START -- gcov attribution artifact under GCC: these continuation lines of a
			// single fprintf call show uncovered even though this exact branch is directly exercised
			// by error_scenarios.f90's print_stat_filtered_rows scenario.
			std::fprintf(stdout, "columns: %d   shown: %zu   rows: %lld (of %lld total)\n\n",
				reader_handle->schema->num_fields(), rows.size(),
				static_cast<long long>(reader_handle->nrows), static_cast<long long>(reader_handle->total_nrows));
			// GCOVR_EXCL_STOP
		}
		else
		{
			std::fprintf(stdout, "columns: %d   shown: %zu   rows: %lld\n\n",
				reader_handle->schema->num_fields(), rows.size(), static_cast<long long>(reader_handle->nrows));
		}

		print_row(headers);
		std::vector<std::string> sep;
		for (auto w : widths) sep.push_back(std::string(w, '-'));
		print_row(sep);
		for (const auto &row : rows) print_row(row);
		std::fflush(stdout);
	}

}

	// This library has no representation for a per-element missing value
	// unless the caller opts in via a validity-output buffer (`valid_out`,
	// nullable): if valid_out is null and the array contains any Parquet
	// Nulls, aborts immediately via report_fatal_error (the default, strict
	// behavior) rather than silently copying out whatever undefined bit
	// pattern Arrow happens to leave in a null slot's data buffer. Calls
	// report_fatal_error directly instead of throwing -- same rationale as
	// the type-mismatch/nrows-mismatch checks elsewhere in this file (see
	// the comment above parquet_read_int32_column): a clean, deliberate
	// abort with a diagnostic on stderr, not an exception relying on
	// unwinding that's unreliable when gfortran links the final executable
	// on macOS. If valid_out is non-null, no abort happens regardless of
	// nulls: valid_out[i] is filled with 1 (valid) / 0 (Null) for every i.
	// The caller is then responsible for overwriting the corresponding
	// data[i] with a safe default wherever valid_out[i] == 0 (see
	// fill_null_default/fill_null_default_string below) -- these two
	// responsibilities are deliberately kept separate so this function stays
	// a simple, type-agnostic yes/no null report.
	static void check_or_report_nulls(const std::shared_ptr<arrow::Array> &array, const std::string &name, int8_t *valid_out, const char *context)
	{
		if (valid_out == nullptr)
		{
			if (array->null_count() != 0)
			{
				report_fatal_error(context, std::string("column contains Null value(s), which is not supported: ") + name);
			}
			return;
		}
		for (int64_t i = 0; i < array->length(); ++i)
		{
			valid_out[i] = array->IsValid(i) ? 1 : 0;
		}
	}

	// Array/vector (list) columns carry two independent Arrow validity
	// bitmaps: the outer list array's own (row-level: the whole row's vector
	// is missing) and the flattened child values array's (element-level: one
	// scalar entry within an otherwise-present row is missing). A slot is
	// reported invalid if either is null. `list_array` is the outer list
	// array (row-level nulls are looked up at row_offset + i); `vals_any` is
	// the already-flattened/sliced child values array that data[] is about
	// to be copied from, of length `nrows * col_size`, indexed the same
	// way data[] is (k = i * col_size + j). Same abort-vs-report contract
	// as check_or_report_nulls.
	static void report_nulls_list_full(
		const std::shared_ptr<arrow::Array> &list_array,
		const std::shared_ptr<arrow::Array> &vals_any,
		const std::string &name,
		int64_t nrows, int64_t col_size, int64_t row_offset,
		int8_t *valid_out, const char *context)
	{
		bool any_null = (vals_any->null_count() != 0);
		if (!any_null)
		{
			for (int64_t i = 0; i < nrows; ++i)
			{
				if (list_array->IsNull(row_offset + i)) { any_null = true; break; }
			}
		}

		if (valid_out == nullptr)
		{
			if (any_null)
			{
				report_fatal_error(context, std::string("column contains Null value(s), which is not supported: ") + name);
			}
			return;
		}

		// valid_out must be fully populated even when any_null is false here (e.g. this specific
		// chunked-read row group has no nulls, even though other row groups of the same column
		// do) -- an early return in that case would leave valid_out uninitialized instead of
		// correctly all-valid.
		for (int64_t i = 0; i < nrows; ++i)
		{
			bool row_valid = list_array->IsValid(row_offset + i);
			for (int64_t j = 0; j < col_size; ++j)
			{
				int64_t k = i * col_size + j;
				valid_out[k] = (row_valid && vals_any->IsValid(k)) ? 1 : 0;
			}
		}
	}

	// Element-mode variant: valid_out has length `nrows` (one entry per
	// row), every entry evaluated at the same fixed array position `offset`
	// (0-based) within its row. `vals_any` is the full flattened child
	// values array (length nrows * col_size); `list_array` is the outer
	// list array.
	static void report_nulls_list_element(
		const std::shared_ptr<arrow::Array> &list_array,
		const std::shared_ptr<arrow::Array> &vals_any,
		const std::string &name,
		int64_t nrows, int64_t col_size, int64_t offset,
		int8_t *valid_out, const char *context)
	{
		bool any_null = false;
		for (int64_t i = 0; i < nrows; ++i)
		{
			if (!list_array->IsValid(i) || !vals_any->IsValid(i * col_size + offset)) { any_null = true; break; }
		}

		if (valid_out == nullptr)
		{
			if (any_null)
			{
				report_fatal_error(context, std::string("column contains Null value(s), which is not supported: ") + name);
			}
			return;
		}

		// See report_nulls_list_full's identical comment: valid_out must be fully populated even
		// when any_null is false here, not left uninitialized via an early return.
		for (int64_t i = 0; i < nrows; ++i)
		{
			valid_out[i] = (list_array->IsValid(i) && vals_any->IsValid(i * col_size + offset)) ? 1 : 0;
		}
	}

	// Zero-fills every entry of `data` marked invalid in `valid_out` (a harmless default value
	// for the Fortran side to overwrite with null_value itself, if given).
	template <typename T>
	static void fill_null_default(T *data, const int8_t *valid_out, int64_t n)
	{
		if (!valid_out) return;
		for (int64_t k = 0; k < n; ++k)
		{
			if (!valid_out[k]) data[k] = T{};
		}
	}

	// String form of fill_null_default: blanks every invalid entry's fixed-width buffer.
	static void fill_null_default_string(char *data, int64_t item_len, const int8_t *valid_out, int64_t n)
	{
		if (!valid_out) return;
		for (int64_t k = 0; k < n; ++k)
		{
			if (!valid_out[k]) copy_string_with_padding(data + k * item_len, item_len, std::string_view());
		}
	}

	// context identifies the calling extern "C" entry point (parquet_read_*_array_row) --
	// see the comment on get_uniform_list_values above for why every failure
	// path here reports cleanly instead of throwing uncaught.
	static std::shared_ptr<arrow::Array> get_row_list_values(const std::shared_ptr<arrow::Array> &array,
		const std::string &name, int64_t row_index, int64_t col_size, const char *context)
	{
		if (array->type_id() == arrow::Type::FIXED_SIZE_LIST)
		{
			auto list_arr = std::static_pointer_cast<arrow::FixedSizeListArray>(array);
			if (row_index < 1 || row_index > list_arr->length())
			{
				report_fatal_error(context, "row_index out of bounds");
			}
			if (static_cast<int64_t>(list_arr->value_length()) != col_size)
			{
				report_fatal_error(context, std::string("col_size mismatch for column: ") + name);
			}
			return list_arr->values()->Slice(list_arr->value_offset(row_index - 1), col_size);
		}
		if (array->type_id() == arrow::Type::LIST)
		{
			auto list_arr = std::static_pointer_cast<arrow::ListArray>(array);
			if (row_index < 1 || row_index > list_arr->length())
			{
				report_fatal_error(context, "row_index out of bounds");
			}
			if (static_cast<int64_t>(list_arr->value_length(row_index - 1)) != col_size)
			{
				report_fatal_error(context, std::string("col_size mismatch for column: ") + name);
			}
			return list_arr->values()->Slice(list_arr->value_offset(row_index - 1), col_size);
		}
		// GCOVR_EXCL_START -- untested compat backstop, same reasoning as get_uniform_list_values'
		// own LARGE_LIST branch (including the condition line itself, permanently unreachable
		// since the FIXED_SIZE_LIST/LIST branches above both unconditionally `return`).
		if (array->type_id() == arrow::Type::LARGE_LIST)
		{
			auto list_arr = std::static_pointer_cast<arrow::LargeListArray>(array);
			if (row_index < 1 || row_index > list_arr->length())
			{
				report_fatal_error(context, "row_index out of bounds");
			}
			if (list_arr->value_length(row_index - 1) != col_size)
			{
				report_fatal_error(context, std::string("col_size mismatch for column: ") + name);
			}
			return list_arr->values()->Slice(list_arr->value_offset(row_index - 1), col_size);
		}
		// GCOVR_EXCL_STOP
		report_fatal_error(context, std::string("type mismatch for column: ") + name +
			" (expected fixed_size_list/list/large_list, got " + array->type()->ToString() + ")"); // GCOVR_EXCL_LINE
	}

	// Forward declaration -- get_row_group_chunk_array is defined further below (it needs
	// ParquetReaderHandle's row-group bookkeeping, laid out near the _column_chunk family), but
	// resolve_row_group_for_row/read_list_primitive_row (just below) need to call it.
	static std::shared_ptr<arrow::Array> get_row_group_chunk_array(ParquetReaderHandle *reader_handle,
		const char *name, int64_t row_group, const char *context);

	// Maps a 1-based global row index to the (1-based row_group, 1-based local row-within-group)
	// pair that get_row_group_chunk_array/get_row_list_values need, by walking each row group's
	// own physical row count from the file footer (metadata()->RowGroup(i)->num_rows()) -- never
	// reads any column data. Used by read_list_primitive_row (parquet_read_array_row_mode) so a
	// single row of a vector column can be fetched by reading only the one row group it lives in,
	// instead of materializing the whole column (see get_single_chunk_array's int32 element-count
	// ceiling, documented in CLAUDE.md's "Guarding a hard Arrow int32-only ceiling"). Aborts via
	// report_fatal_error if row_index is out of range.
	static void resolve_row_group_for_row(ParquetReaderHandle *reader_handle, int64_t row_index,
		const char *context, int64_t &row_group_out, int64_t &local_row_out)
	{
		if (row_index < 1)
		{
			report_fatal_error(context, "row_index out of bounds");
		}
		auto *file_metadata = reader_handle->reader->parquet_reader()->metadata().get();
		int64_t remaining = row_index;
		for (int64_t rg = 0; rg < reader_handle->num_row_groups; ++rg)
		{
			int64_t rg_rows = file_metadata->RowGroup(static_cast<int>(rg))->num_rows();
			if (remaining <= rg_rows)
			{
				row_group_out = rg + 1;
				local_row_out = remaining;
				return;
			}
			remaining -= rg_rows;
		}
		report_fatal_error(context, "row_index out of bounds");
	} // GCOVR_EXCL_LINE -- gcov attribution artifact: this closing brace shows uncovered, even though the [[noreturn]] report_fatal_error call above it demonstrably runs.

extern "C"
{

	// Shared by both the scalar (parquet_read_*_column) and fixed-size-list
	// (parquet_read_*_array_column) numeric read paths below: each pair
	// previously duplicated the exact same per-physical-type widening/
	// narrowing switch, once over a plain array and once over a list's
	// values array. One conversion routine per output CType, called from
	// both sites, replaces both duplicated copies.
	// stride/offset let this also serve parquet_read_*_array_element (which
	// reads one strided element per row out of the full list-values array,
	// at index i*stride+offset, rather than n contiguous values from index
	// 0) -- see read_list_primitive_element below. Every other caller reads
	// a contiguous run, i.e. the default stride=1/offset=0.
	static void convert_values_to_int32(
		const std::shared_ptr<arrow::Array> &vals, int32_t *data, int64_t n, const char *name, const char *context,
		int64_t stride = 1, int64_t offset = 0)
	{
		switch (vals->type_id())
		{
		case arrow::Type::INT32:
		{
			auto arr = std::static_pointer_cast<arrow::Int32Array>(vals);
			for (int64_t i = 0; i < n; ++i) data[i] = arr->Value(offset + i * stride);
			break;
		}
		// GCOVR_EXCL_START -- this int64-source branch of convert_values_to_int32 is only reached
		// via an abort-ending extended-source-type overflow scenario (std::abort() there discards
		// that whole process's gcov coverage, including the lines that ran before it). The case
		// label itself is included in this exclusion (not just the body): under GCC, a case label
		// reachable only via an abort-ending scenario shows uncovered in its own right, distinct
		// from Clang's gcov.
		case arrow::Type::INT64:
		{
			auto arr = std::static_pointer_cast<arrow::Int64Array>(vals);
			for (int64_t i = 0; i < n; ++i)
			{
				auto v = arr->Value(offset + i * stride);
				if (v < std::numeric_limits<int32_t>::min() || v > std::numeric_limits<int32_t>::max())
				{
					report_fatal_error(context, std::string("int64->int32 overflow for column: ") + name);
				}
				data[i] = static_cast<int32_t>(v);
			}
			break;
		}
		// GCOVR_EXCL_STOP
		case arrow::Type::INT8:
		case arrow::Type::INT16:
		case arrow::Type::UINT8:
		case arrow::Type::UINT16:
		{
			// Always fits int32 exactly -- no overflow check needed.
			for (int64_t i = 0; i < n; ++i) data[i] = static_cast<int32_t>(small_integer_value_at(vals, offset + i * stride));
			break;
		}
		// UINT32 is genuinely exercised by a passing (non-aborting) round trip, unlike UINT64 just
		// below, which is only reached via extended_uint64_overflow_int32's abort-ending fixture.
		case arrow::Type::UINT32:
		{
			auto arr = std::static_pointer_cast<arrow::UInt32Array>(vals);
			for (int64_t i = 0; i < n; ++i)
			{
				uint32_t v = arr->Value(offset + i * stride);
				if (v > static_cast<uint32_t>(std::numeric_limits<int32_t>::max()))
				{
					report_fatal_error(context, std::string("uint32->int32 overflow for column: ") + name);
				}
				data[i] = static_cast<int32_t>(v);
			}
			break;
		}
		// GCOVR_EXCL_START -- only reached via extended_uint64_overflow_int32's fixture, whose
		// scenario ends in abort (discarding that whole process's gcov coverage). The case label
		// itself is included in this exclusion (not just the body): under GCC, a case label
		// reachable only via an abort-ending scenario shows uncovered in its own right, distinct
		// from Clang's gcov.
		case arrow::Type::UINT64:
		{
			auto arr = std::static_pointer_cast<arrow::UInt64Array>(vals);
			for (int64_t i = 0; i < n; ++i)
			{
				uint64_t v = arr->Value(offset + i * stride);
				if (v > static_cast<uint64_t>(std::numeric_limits<int32_t>::max()))
				{
					report_fatal_error(context, std::string("uint64->int32 overflow for column: ") + name);
				}
				data[i] = static_cast<int32_t>(v);
			}
			break;
		}
		// GCOVR_EXCL_STOP
		// Genuinely exercised by a passing (non-aborting) round trip (real_to_int32_checked itself
		// is likewise genuinely covered, unlike real_to_int64_checked).
		case arrow::Type::FLOAT:
		case arrow::Type::DOUBLE:
		case arrow::Type::HALF_FLOAT:
		{
			std::string type_name = vals->type()->ToString();
			for (int64_t i = 0; i < n; ++i)
			{
				double v = real_family_value_at(vals, offset + i * stride);
				int32_t out;
				auto status = real_to_int32_checked(v, out);
				if (status == NumericConvertStatus::kNonIntegral)
				{
					report_fatal_error(context, type_name + " value has a fractional part, cannot convert to int32 for column: " + name);
				}
				if (status == NumericConvertStatus::kOverflow)
				{
					report_fatal_error(context, type_name + "->int32 overflow for column: " + name);
				}
				data[i] = out;
			}
			break;
		}
		// GCOVR_EXCL_START -- only reached via extended_decimal_*_int32's fixtures, whose scenarios
		// end in abort (discarding that whole process's gcov coverage). The case labels themselves
		// are included in this exclusion (not just the body): under GCC, a fall-through case-label
		// group reachable only via an abort-ending scenario shows uncovered in its own right,
		// distinct from Clang's gcov.
		case arrow::Type::DECIMAL32:
		case arrow::Type::DECIMAL64:
		case arrow::Type::DECIMAL128:
		case arrow::Type::DECIMAL256:
		{
			std::string type_name = vals->type()->ToString();
			for (int64_t i = 0; i < n; ++i)
			{
				int64_t v64;
				auto status = decimal_to_int64_checked(vals, offset + i * stride, v64);
				if (status == NumericConvertStatus::kNonIntegral)
				{
					report_fatal_error(context, type_name + " value has a fractional part, cannot convert to int32 for column: " + name);
				}
				if (status == NumericConvertStatus::kOverflow ||
					v64 < std::numeric_limits<int32_t>::min() || v64 > std::numeric_limits<int32_t>::max())
				{
					report_fatal_error(context, type_name + "->int32 overflow for column: " + name);
				}
				data[i] = static_cast<int32_t>(v64);
			}
			break;
		}
		// GCOVR_EXCL_STOP
		default: // GCOVR_EXCL_LINE -- gcov attribution artifact under GCC: this label shows
		// uncovered even though the report_fatal_error() below is already excluded via the CI
		// pattern rule and this default is genuinely never taken by a covered test.
			report_fatal_error(context, std::string("type mismatch for column: ") + name +
				" (expected int32/int64, got " + vals->type()->ToString() + ")"); // GCOVR_EXCL_LINE
		}
	}

	// Same as convert_values_to_int32, but widening/narrowing to int64.
	static void convert_values_to_int64(
		const std::shared_ptr<arrow::Array> &vals, int64_t *data, int64_t n, const char *name, const char *context,
		int64_t stride = 1, int64_t offset = 0)
	{
		switch (vals->type_id())
		{
		case arrow::Type::INT64:
		{
			auto arr = std::static_pointer_cast<arrow::Int64Array>(vals);
			for (int64_t i = 0; i < n; ++i) data[i] = arr->Value(offset + i * stride);
			break;
		}
		case arrow::Type::INT32:
		case arrow::Type::INT8:
		case arrow::Type::INT16:
		case arrow::Type::UINT8:
		case arrow::Type::UINT16:
		case arrow::Type::UINT32:
		{
			// Always fits int64 exactly -- no overflow check needed.
			for (int64_t i = 0; i < n; ++i) data[i] = small_integer_value_at(vals, offset + i * stride);
			break;
		}
		case arrow::Type::UINT64:
		{
			auto arr = std::static_pointer_cast<arrow::UInt64Array>(vals);
			for (int64_t i = 0; i < n; ++i)
			{
				uint64_t v = arr->Value(offset + i * stride);
				if (v > static_cast<uint64_t>(std::numeric_limits<int64_t>::max()))
				{
					report_fatal_error(context, std::string("uint64->int64 overflow for column: ") + name);
				}
				data[i] = static_cast<int64_t>(v);
			}
			break;
		}
		// GCOVR_EXCL_START -- only reached via extended_real_*_int64's fixtures, whose scenarios end
		// in abort (discarding that whole process's gcov coverage), unlike the UINT64/DECIMAL*
		// branches below, which are also exercised by a passing, non-aborting extended-source-type
		// round trip. The case labels themselves are included in this exclusion (not just the
		// body): under GCC, a fall-through case-label group reachable only via an abort-ending
		// scenario shows uncovered in its own right, distinct from Clang's gcov.
		case arrow::Type::FLOAT:
		case arrow::Type::DOUBLE:
		case arrow::Type::HALF_FLOAT:
		{
			std::string type_name = vals->type()->ToString();
			for (int64_t i = 0; i < n; ++i)
			{
				double v = real_family_value_at(vals, offset + i * stride);
				int64_t out;
				auto status = real_to_int64_checked(v, out);
				if (status == NumericConvertStatus::kNonIntegral)
				{
					report_fatal_error(context, type_name + " value has a fractional part, cannot convert to int64 for column: " + name);
				}
				if (status == NumericConvertStatus::kOverflow)
				{
					report_fatal_error(context, type_name + "->int64 overflow for column: " + name);
				}
				data[i] = out;
			}
			break;
		}
		// GCOVR_EXCL_STOP
		case arrow::Type::DECIMAL32:
		case arrow::Type::DECIMAL64:
		case arrow::Type::DECIMAL128:
		case arrow::Type::DECIMAL256:
		{
			std::string type_name = vals->type()->ToString();
			for (int64_t i = 0; i < n; ++i)
			{
				int64_t v64;
				auto status = decimal_to_int64_checked(vals, offset + i * stride, v64);
				if (status == NumericConvertStatus::kNonIntegral)
				{
					report_fatal_error(context, type_name + " value has a fractional part, cannot convert to int64 for column: " + name);
				}
				if (status == NumericConvertStatus::kOverflow)
				{
					report_fatal_error(context, type_name + "->int64 overflow for column: " + name);
				}
				data[i] = v64;
			}
			break;
		}
		default: // GCOVR_EXCL_LINE -- gcov attribution artifact under GCC: this label shows
		// uncovered even though the report_fatal_error() below is already excluded via the CI
		// pattern rule and this default is genuinely never taken by a covered test.
			report_fatal_error(context, std::string("type mismatch for column: ") + name +
				" (expected int64/int32, got " + vals->type()->ToString() + ")"); // GCOVR_EXCL_LINE
		}
	}

	// Same as convert_values_to_int32, but converting to float32.
	static void convert_values_to_float32(
		const std::shared_ptr<arrow::Array> &vals, float *data, int64_t n, const char *name, const char *context,
		int64_t stride = 1, int64_t offset = 0)
	{
		switch (vals->type_id())
		{
		case arrow::Type::FLOAT:
		{
			auto arr = std::static_pointer_cast<arrow::FloatArray>(vals);
			for (int64_t i = 0; i < n; ++i) data[i] = arr->Value(offset + i * stride);
			break;
		}
		case arrow::Type::DOUBLE:
		{
			auto arr = std::static_pointer_cast<arrow::DoubleArray>(vals);
			for (int64_t i = 0; i < n; ++i) data[i] = static_cast<float>(arr->Value(offset + i * stride));
			break;
		}
		case arrow::Type::INT32:
		case arrow::Type::INT64:
		case arrow::Type::INT8:
		case arrow::Type::INT16:
		case arrow::Type::UINT8:
		case arrow::Type::UINT16:
		case arrow::Type::UINT32:
		{
			for (int64_t i = 0; i < n; ++i) data[i] = static_cast<float>(small_integer_value_at(vals, offset + i * stride));
			break;
		}
		case arrow::Type::UINT64:
		{
			auto arr = std::static_pointer_cast<arrow::UInt64Array>(vals);
			for (int64_t i = 0; i < n; ++i) data[i] = static_cast<float>(arr->Value(offset + i * stride));
			break;
		}
		case arrow::Type::HALF_FLOAT:
		{
			for (int64_t i = 0; i < n; ++i) data[i] = static_cast<float>(real_family_value_at(vals, offset + i * stride));
			break;
		}
		case arrow::Type::DECIMAL32:
		case arrow::Type::DECIMAL64:
		case arrow::Type::DECIMAL128:
		case arrow::Type::DECIMAL256:
		{
			for (int64_t i = 0; i < n; ++i) data[i] = static_cast<float>(decimal_value_at(vals, offset + i * stride));
			break;
		}
		default: // GCOVR_EXCL_LINE -- gcov attribution artifact under GCC: this label shows
		// uncovered even though the report_fatal_error() below is already excluded via the CI
		// pattern rule and this default is genuinely never taken by a covered test.
			report_fatal_error(context, std::string("type mismatch for column: ") + name +
				" (expected float32/float64/int32/int64, got " + vals->type()->ToString() + ")"); // GCOVR_EXCL_LINE
		}
	}

	// Same as convert_values_to_int32, but converting to float64.
	static void convert_values_to_float64(
		const std::shared_ptr<arrow::Array> &vals, double *data, int64_t n, const char *name, const char *context,
		int64_t stride = 1, int64_t offset = 0)
	{
		switch (vals->type_id())
		{
		case arrow::Type::DOUBLE:
		{
			auto arr = std::static_pointer_cast<arrow::DoubleArray>(vals);
			for (int64_t i = 0; i < n; ++i) data[i] = arr->Value(offset + i * stride);
			break;
		}
		case arrow::Type::FLOAT:
		{
			auto arr = std::static_pointer_cast<arrow::FloatArray>(vals);
			for (int64_t i = 0; i < n; ++i) data[i] = static_cast<double>(arr->Value(offset + i * stride));
			break;
		}
		case arrow::Type::INT32:
		case arrow::Type::INT64:
		case arrow::Type::INT8:
		case arrow::Type::INT16:
		case arrow::Type::UINT8:
		case arrow::Type::UINT16:
		case arrow::Type::UINT32:
		{
			for (int64_t i = 0; i < n; ++i) data[i] = static_cast<double>(small_integer_value_at(vals, offset + i * stride));
			break;
		}
		case arrow::Type::UINT64:
		{
			auto arr = std::static_pointer_cast<arrow::UInt64Array>(vals);
			for (int64_t i = 0; i < n; ++i) data[i] = static_cast<double>(arr->Value(offset + i * stride));
			break;
		}
		case arrow::Type::HALF_FLOAT:
		{
			for (int64_t i = 0; i < n; ++i) data[i] = real_family_value_at(vals, offset + i * stride);
			break;
		}
		case arrow::Type::DECIMAL32:
		case arrow::Type::DECIMAL64:
		case arrow::Type::DECIMAL128:
		case arrow::Type::DECIMAL256:
		{
			for (int64_t i = 0; i < n; ++i) data[i] = decimal_value_at(vals, offset + i * stride);
			break;
		}
		default: // GCOVR_EXCL_LINE -- gcov attribution artifact under GCC: this label shows
		// uncovered even though the report_fatal_error() below is already excluded via the CI
		// pattern rule and this default is genuinely never taken by a covered test.
			report_fatal_error(context, std::string("type mismatch for column: ") + name +
				" (expected float64/float32/int32/int64, got " + vals->type()->ToString() + ")"); // GCOVR_EXCL_LINE
		}
	}

} // extern "C"

// read_list_primitive_row/read_list_primitive_element back parquet_read_*_array_row
// and parquet_read_*_array_element -- reading one row's (or one strided
// element's) worth of a fixed-size-list/list column. These used to run their
// own separate, looser numeric conversion (numeric_value_at: silently
// accepted any of int32/int64/float32/float64 as a source and widened
// through a double, with no int64->int32 overflow check) instead of the
// same convert_values_to_* rules parquet_read_column/parquet_read_array_column
// enforce. Dispatching to convert_values_to_<CType> here instead makes a
// row/element read exactly as strict as reading the whole column: same
// accepted source types, same int64->int32 overflow check, same clean abort
// via report_fatal_error on a type mismatch. Declared outside extern "C"
// (templates cannot have C language linkage) between two extern "C" blocks,
// same as ConcurrencyGuard/append_typed_column above -- convert_values_to_*
// is already visible here via ordinary (non-template-dependent) name lookup.
// Fortran-side output type name for CType, matching the strings mark_read
// uses at every other typed read call site -- shared by read_list_primitive_row/
// _element below so their tracked output_type_used entries look the same as
// every other read function's (see parquet_reader_print_stat).
template <typename CType>
static constexpr const char *ctype_name()
{
	if constexpr (std::is_same_v<CType, int32_t>) return "int32";
	else if constexpr (std::is_same_v<CType, int64_t>) return "int64";
	else if constexpr (std::is_same_v<CType, float>) return "float32";
	else if constexpr (std::is_same_v<CType, double>) return "float64";
}

// Shared body for every parquet_read_*_array_row extern "C" entry point: reads one row's
// full element vector of a vector column `name` into `data`.
template <typename CType>
static void read_list_primitive_row(void *handle, const char *name, int64_t row_index, CType *data, int64_t col_size, int8_t *valid_out)
{
	auto reader_handle = as_reader_handle(handle);
	// A filter mask has no row-group structure of its own (see get_row_group_chunk_array's own
	// comment on why filtering doesn't compose with a row-group-scoped read), so row_index there
	// means "index into the filtered array" -- that case keeps the old whole-column path, which
	// already applies the filter via get_single_chunk_array/apply_filter_mask. Only the common,
	// unfiltered case is switched to a row-group-scoped read, so a single row can be fetched
	// without materializing the whole column (see resolve_row_group_for_row's own comment).
	std::shared_ptr<arrow::Array> array;
	int64_t local_row_index = row_index;
	if (reader_handle->filter_mask)
	{
		array = get_single_chunk_array(reader_handle, name);
	}
	else
	{
		int64_t row_group = 0;
		resolve_row_group_for_row(reader_handle, row_index, "parquet_read_array_row_mode", row_group, local_row_index);
		array = get_row_group_chunk_array(reader_handle, name, row_group, "parquet_read_array_row_mode");
	}
	auto vals_any = get_row_list_values(array, name, local_row_index, col_size, "parquet_read_array_row_mode");
	report_nulls_list_full(array, vals_any, name, 1, col_size, local_row_index - 1, valid_out, "parquet_read_array_row_mode");

	// get_row_list_values already returns a contiguous col_size-element slice
	// for this one row, so the default stride=1/offset=0 applies.
	if constexpr (std::is_same_v<CType, int32_t>)
		convert_values_to_int32(vals_any, data, col_size, name, "parquet_read_array_row_mode");
	else if constexpr (std::is_same_v<CType, int64_t>)
		convert_values_to_int64(vals_any, data, col_size, name, "parquet_read_array_row_mode");
	else if constexpr (std::is_same_v<CType, float>)
		convert_values_to_float32(vals_any, data, col_size, name, "parquet_read_array_row_mode");
	else if constexpr (std::is_same_v<CType, double>)
		convert_values_to_float64(vals_any, data, col_size, name, "parquet_read_array_row_mode");

	fill_null_default(data, valid_out, col_size);
	mark_read(reader_handle, name, ctype_name<CType>(), array);
}

// Returns col_size for `name` without reading any column data when the column is a
// FIXED_SIZE_LIST (the common case) -- same schema-only introspection as
// parquet_reader_get_column_col_size (see its own comment). Every parquet_read_*_array_element
// entry point needs col_size before it can validate col_index/compute the stride offset, so
// element-mode's row-group streaming (see stream_element_mode_row_groups below) must not force a
// whole-column read just to answer that. Falls back to a whole-column read (via
// get_single_chunk_array) only for the rare LIST/LARGE_LIST case, matching that function's own
// fallback (this library always writes FIXED_SIZE_LIST for vector columns).
static int64_t resolve_element_mode_col_size(ParquetReaderHandle *reader_handle, const char *name)
{
	auto resolved = resolve_struct_path(reader_handle->schema, name);
	if (resolved.leaf_field->type()->id() == arrow::Type::FIXED_SIZE_LIST)
	{
		return static_cast<int64_t>(std::static_pointer_cast<arrow::FixedSizeListType>(resolved.leaf_field->type())->list_size());
	}
	auto array = get_single_chunk_array(reader_handle, name);
	return get_col_size(array);
}

// Streams row group by row group for a parquet_read_*_array_element entry point. Unlike
// row_mode (read_list_primitive_row, which only ever needs ONE row group -- a single row's
// data), element mode inherently needs every row's value at the same fixed column position, i.e.
// data from EVERY row group -- it cannot skip any of them the way row_mode skips all but one.
// What this avoids is ever materializing the whole flattened nrows*col_size array in a single
// Arrow call (the original bug: identical in shape to row_mode's own pre-fix bug -- see
// resolve_row_group_for_row's comment and CLAUDE.md's "Guarding a hard Arrow int32-only
// ceiling"). Each row group's own element count (row_group_nrows*col_size) is already kept under
// the int32 ceiling by the write side's row-group auto-sizing, so reading/flattening one row
// group at a time (via get_row_group_chunk_array, the same helper row_mode's fix uses) never
// asks Arrow to build an int32-overflowing array -- even though the final output spans the whole
// file. `per_row_group` is invoked once per non-empty row group with (row_group_array,
// row_group_vals, row_group_nrows, row_offset); the caller writes into its own data/valid_out at
// row_offset (this helper doesn't know the CType/bool/string specifics of what to write).
// Returns the last row group's own array (for mark_read's bookkeeping call), or nullptr if the
// column has zero rows (no row groups at all, or every row group reported zero rows).
template <typename Fn>
static std::shared_ptr<arrow::Array> stream_element_mode_row_groups(
	ParquetReaderHandle *reader_handle, const char *name, int64_t col_size, int64_t nrows, const char *context,
	Fn &&per_row_group)
{
	auto *file_metadata = reader_handle->reader->parquet_reader()->metadata().get();
	int64_t row_offset = 0;
	std::shared_ptr<arrow::Array> last_array;
	for (int64_t rg = 0; rg < reader_handle->num_row_groups; ++rg)
	{
		int64_t rg_rows = file_metadata->RowGroup(static_cast<int>(rg))->num_rows();
		if (rg_rows == 0) continue;
		auto array = get_row_group_chunk_array(reader_handle, name, rg + 1, context);
		auto vals_any = get_uniform_list_values(array, name, rg_rows, col_size, context);
		per_row_group(array, vals_any, rg_rows, row_offset);
		row_offset += rg_rows;
		last_array = array;
	}
	if (row_offset != nrows)
	{
		report_fatal_error(context, std::string("nrows mismatch for column: ") + name);
	}
	return last_array;
} // GCOVR_EXCL_LINE -- gcov attribution artifact under GCC: this closing brace shows uncovered
// even though stream_element_mode_row_groups is heavily used by every array row/element-mode
// read (including this session's own zero-row element-mode test) and its own return statement
// above is covered.

// Shared body for every parquet_read_*_array_element extern "C" entry point: reads one
// element position (`col_index`) of a vector column `name` across every row into `data`.
template <typename CType>
static void read_list_primitive_element(void *handle, const char *name, int64_t col_index, CType *data, int64_t nrows, int8_t *valid_out)
{
	auto reader_handle = as_reader_handle(handle);
	const char *context = "parquet_read_array_element_mode";

	// See read_list_primitive_row's identical comment: a filter mask has no row-group structure
	// of its own, so that case keeps the old whole-column path (already filtered via
	// get_single_chunk_array/apply_filter_mask). Only the common, unfiltered case streams
	// row-group by row-group below.
	if (reader_handle->filter_mask)
	{
		auto array = get_single_chunk_array(reader_handle, name);
		auto col_size = get_col_size(array);
		if (col_index < 1 || col_index > col_size)
		{
			report_fatal_error(context, "col_index out of bounds");
		}
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, context);
		auto offset = col_index - 1;
		report_nulls_list_element(array, vals_any, name, nrows, col_size, offset, valid_out, context);
		if constexpr (std::is_same_v<CType, int32_t>)
			convert_values_to_int32(vals_any, data, nrows, name, context, col_size, offset);
		else if constexpr (std::is_same_v<CType, int64_t>)
			convert_values_to_int64(vals_any, data, nrows, name, context, col_size, offset);
		else if constexpr (std::is_same_v<CType, float>)
			convert_values_to_float32(vals_any, data, nrows, name, context, col_size, offset);
		else if constexpr (std::is_same_v<CType, double>)
			convert_values_to_float64(vals_any, data, nrows, name, context, col_size, offset);
		fill_null_default(data, valid_out, nrows);
		mark_read(reader_handle, name, ctype_name<CType>(), array);
		return;
	}

	auto col_size = resolve_element_mode_col_size(reader_handle, name);
	if (col_index < 1 || col_index > col_size)
	{
		report_fatal_error(context, "col_index out of bounds");
	}
	auto offset = col_index - 1;

	// vals_any (per row group) holds that row group's rows' full col_size-element vectors back
	// to back, so the value for local row i at this fixed col_index sits at a stride of col_size
	// apart, starting at offset -- not contiguous, hence passing stride/offset explicitly here
	// (unlike row_mode's contiguous single-row slice).
	auto last_array = stream_element_mode_row_groups(reader_handle, name, col_size, nrows, context,
		[&](const std::shared_ptr<arrow::Array> &array, const std::shared_ptr<arrow::Array> &vals_any,
			int64_t rg_rows, int64_t row_offset)
		{
			int8_t *valid_slice = valid_out ? valid_out + row_offset : nullptr;
			report_nulls_list_element(array, vals_any, name, rg_rows, col_size, offset, valid_slice, context);
			if constexpr (std::is_same_v<CType, int32_t>)
				convert_values_to_int32(vals_any, data + row_offset, rg_rows, name, context, col_size, offset);
			else if constexpr (std::is_same_v<CType, int64_t>)
				convert_values_to_int64(vals_any, data + row_offset, rg_rows, name, context, col_size, offset);
			else if constexpr (std::is_same_v<CType, float>)
				convert_values_to_float32(vals_any, data + row_offset, rg_rows, name, context, col_size, offset);
			else if constexpr (std::is_same_v<CType, double>)
				convert_values_to_float64(vals_any, data + row_offset, rg_rows, name, context, col_size, offset);
		});

	fill_null_default(data, valid_out, nrows);
	// A zero-row column has no row groups to have set `last_array` from -- fall back to the
	// (cheap, since empty) whole-column read purely so mark_read has an array for its
	// was_read/output_type_used/run_qc_checks bookkeeping.
	if (!last_array)
	{
		last_array = get_single_chunk_array(reader_handle, name);
	}
	mark_read(reader_handle, name, ctype_name<CType>(), last_array);
}

// ============================================================================
// Temporal (date/time/timestamp) support for parquet_temporal.f90's element types.
//
// Buffer transport across this boundary (kept single-valued so the vector/chunk/row/element
// plumbing above is reused unchanged):
//   * date      -- int32 days since 1970-01-01 (Arrow date32's own value). This side
//                  normalizes a DATE64 (int64 ms) column to days on read, and always writes
//                  date32; date has no unit.
//   * time      -- int64 canonical nanoseconds-of-day. This side scales the file's own unit
//                  (time32[s/ms], time64[us/ns]) to/from ns; canonical ns-of-day never
//                  overflows an int64 in any unit, so the scaling (and the write-side
//                  exact-precision check) lives here in C++.
//   * timestamp -- int64 value in the column's own file unit, with that unit reported/accepted
//                  as a separate int32 selector (1=s, 2=ms, 3=us, 4=ns). Unlike time, a
//                  timestamp's full int64 range CAN overflow when rescaled between units, so
//                  the (range-checked) unit conversion is done on the Fortran side instead
//                  (parquet_temporal's set_unix/to_unix); this side only reports the stored
//                  unit on read and builds timestamp(unit, tz) on write. INT96 columns are
//                  decoded by Arrow to timestamp[ns] before they ever reach here.
// The selector integers match parquet_temporal.f90's parquet_unit_* constants exactly.
// ============================================================================

// Every SECOND case/fallback arm in the three switches below is GCOVR_EXCL'd. Two different
// reasons, not one: the fallback `return` after each switch is unreachable because these three
// functions exhaustively cover all 4 arrow::TimeUnit enumerators that exist today (kept only as
// a guard against a future Arrow release adding a 5th). The SECOND arms are unreachable for a
// stronger reason: Parquet's physical format has no seconds-resolution TIME/TIMESTAMP encoding
// at all, so unit_sel/arrow::TimeUnit::SECOND can never legitimately flow through here from
// either direction -- not just from this library's own writer (parse_temporal_suffix rejects
// "s"/"seconds" at parse time), but from ANY file: confirmed empirically that Arrow's own writer
// silently coerces a SECOND-unit array to MILLI on write even with store_schema() set, so no
// tool can ever produce a file with a genuine SECOND-unit TIME/TIMESTAMP column to read back
// either. See CLAUDE.md's "The parquet_temporal module" section.

// Maps a parquet_unit_* selector (1..4) to arrow::TimeUnit; aborts on an out-of-range selector.
static arrow::TimeUnit::type temporal_selector_to_arrow_unit(int32_t unit, const char *context)
{
	switch (unit)
	{
	case 1: return arrow::TimeUnit::SECOND; // GCOVR_EXCL_LINE
	case 2: return arrow::TimeUnit::MILLI;
	case 3: return arrow::TimeUnit::MICRO;
	case 4: return arrow::TimeUnit::NANO;
	default: // GCOVR_EXCL_LINE -- gcov attribution artifact under GCC: this label shows uncovered
	// even though the report_fatal_error() below is already excluded via the CI pattern rule and
	// this default is genuinely never taken by a covered test.
		report_fatal_error(context, "invalid time unit selector (expected 1..4)");
	}
	return arrow::TimeUnit::MICRO; // unreachable (report_fatal_error does not return)
}

// Inverse of temporal_selector_to_arrow_unit: arrow::TimeUnit -> a parquet_unit_* selector.
static int32_t arrow_unit_to_temporal_selector(arrow::TimeUnit::type unit)
{
	switch (unit)
	{
	case arrow::TimeUnit::SECOND: return 1; // GCOVR_EXCL_LINE
	case arrow::TimeUnit::MILLI:  return 2;
	case arrow::TimeUnit::MICRO:  return 3;
	case arrow::TimeUnit::NANO:   return 4;
	}
	return 3; // GCOVR_EXCL_LINE -- unreachable (all four TimeUnit values are covered above)
}

// Nanoseconds per one tick of `unit`: SECOND -> 1e9 ... NANO -> 1.
static int64_t temporal_ns_per_unit(arrow::TimeUnit::type unit)
{
	switch (unit)
	{
	case arrow::TimeUnit::SECOND: return 1000000000LL; // GCOVR_EXCL_LINE
	case arrow::TimeUnit::MILLI:  return 1000000LL;
	case arrow::TimeUnit::MICRO:  return 1000LL;
	case arrow::TimeUnit::NANO:   return 1LL;
	}
	return 1LL; // GCOVR_EXCL_LINE -- unreachable
}

// Fills `data` with days-since-1970-01-01 (Parquet DATE / Arrow date32's own value), decoding
// either a DATE32 column (identity) or a DATE64 column (milliseconds since the epoch, which
// Arrow guarantees is an exact multiple of 86,400,000 -- verified here rather than assumed).
// Signature matches convert_values_to_int32 so the vector/chunk/row/element plumbing is reused;
// the default branch is the strict-typing guard (a non-date column read as a date aborts).
//
// The DATE64 branch is defensive, unreachable dead code for any genuine Parquet file: the
// Parquet format's DATE logical type is specified to annotate an int32 physical value only
// (days since the epoch) -- there is no int64/DATE64 physical representation at the format
// level at all, confirmed empirically too (arrow::Date64Array written via
// parquet::arrow::WriteTable, even with ArrowWriterProperties::store_schema(), is always
// coerced to DATE32 physical on write, so no tool -- this library or any other -- can ever
// produce a file this branch would fire on). Kept only in case a future Arrow/Parquet version
// changes that; GCOVR_EXCL'd since it is not reachable to test via any real fixture.
static void convert_date_values(const std::shared_ptr<arrow::Array> &vals, int32_t *data, int64_t n,
	const char *name, const char *context, int64_t stride = 1, int64_t offset = 0)
{
	switch (vals->type_id())
	{
	case arrow::Type::DATE32:
	{
		auto arr = std::static_pointer_cast<arrow::Date32Array>(vals);
		for (int64_t i = 0; i < n; ++i) data[i] = arr->Value(offset + i * stride);
		break;
	}
	case arrow::Type::DATE64: // GCOVR_EXCL_START
	{
		auto arr = std::static_pointer_cast<arrow::Date64Array>(vals);
		for (int64_t i = 0; i < n; ++i)
		{
			int64_t ms = arr->Value(offset + i * stride);
			if (ms % 86400000LL != 0)
			{
				report_fatal_error(context, std::string("date64 value is not a whole number of days for column: ") + name);
			}
			data[i] = static_cast<int32_t>(ms / 86400000LL);
		}
		break;
	}
	// GCOVR_EXCL_STOP
	default: // GCOVR_EXCL_LINE -- gcov attribution artifact under GCC: this label shows uncovered
	// even though the report_fatal_error() below is already excluded via the CI pattern rule and
	// this default is genuinely never taken by a covered test.
		report_fatal_error(context, std::string("type mismatch for column: ") + name +
			" (expected date, got " + vals->type()->ToString() + ")"); // GCOVR_EXCL_LINE
	}
}

// Fills `data` with canonical nanoseconds-of-day, scaling a TIME32 (seconds/millis) or TIME64
// (micros/nanos) column's own unit up to ns. Signature matches convert_values_to_int64 for
// plumbing reuse; the default branch is the strict-typing guard.
static void convert_time_values(const std::shared_ptr<arrow::Array> &vals, int64_t *data, int64_t n,
	const char *name, const char *context, int64_t stride = 1, int64_t offset = 0)
{
	switch (vals->type_id())
	{
	case arrow::Type::TIME32:
	{
		auto arr = std::static_pointer_cast<arrow::Time32Array>(vals);
		int64_t scale = temporal_ns_per_unit(std::static_pointer_cast<arrow::Time32Type>(vals->type())->unit());
		for (int64_t i = 0; i < n; ++i) data[i] = static_cast<int64_t>(arr->Value(offset + i * stride)) * scale;
		break;
	}
	case arrow::Type::TIME64:
	{
		auto arr = std::static_pointer_cast<arrow::Time64Array>(vals);
		int64_t scale = temporal_ns_per_unit(std::static_pointer_cast<arrow::Time64Type>(vals->type())->unit());
		for (int64_t i = 0; i < n; ++i) data[i] = arr->Value(offset + i * stride) * scale;
		break;
	}
	default: // GCOVR_EXCL_LINE -- gcov attribution artifact under GCC: this label shows uncovered
	// even though the report_fatal_error() below is already excluded via the CI pattern rule and
	// this default is genuinely never taken by a covered test.
		report_fatal_error(context, std::string("type mismatch for column: ") + name +
			" (expected time, got " + vals->type()->ToString() + ")"); // GCOVR_EXCL_LINE
	}
}

// Fills `data` with a TIMESTAMP column's raw stored int64 values (in the column's own file
// unit -- the unit itself is reported separately, see timestamp_unit_selector_of). Signature
// matches convert_values_to_int64 for plumbing reuse; a non-timestamp column aborts (strict).
static void convert_timestamp_values(const std::shared_ptr<arrow::Array> &vals, int64_t *data, int64_t n,
	const char *name, const char *context, int64_t stride = 1, int64_t offset = 0)
{
	if (vals->type_id() != arrow::Type::TIMESTAMP)
	{
		report_fatal_error(context, std::string("type mismatch for column: ") + name +
			" (expected timestamp, got " + vals->type()->ToString() + ")"); // GCOVR_EXCL_LINE
	}
	auto arr = std::static_pointer_cast<arrow::TimestampArray>(vals);
	for (int64_t i = 0; i < n; ++i) data[i] = arr->Value(offset + i * stride);
}

// Returns the parquet_unit_* selector of a TIMESTAMP value array (aborts if it is not one).
static int32_t timestamp_unit_selector_of(const std::shared_ptr<arrow::Array> &vals, const char *name, const char *context)
{
	if (vals->type_id() != arrow::Type::TIMESTAMP)
	{
		report_fatal_error(context, std::string("type mismatch for column: ") + name +
			" (expected timestamp, got " + vals->type()->ToString() + ")"); // GCOVR_EXCL_LINE
	}
	return arrow_unit_to_temporal_selector(std::static_pointer_cast<arrow::TimestampType>(vals->type())->unit());
} // GCOVR_EXCL_LINE -- gcov attribution artifact: this closing brace shows uncovered even though the covered `return` above proves the body ran.

// The Arrow value type of a TIME column of the given unit: TIME32 for seconds/millis, TIME64
// for micros/nanos (Arrow forbids the other two combinations).
static std::shared_ptr<arrow::DataType> temporal_time_value_type(int32_t unit_selector, const char *context)
{
	auto unit = temporal_selector_to_arrow_unit(unit_selector, context);
	return (unit == arrow::TimeUnit::SECOND || unit == arrow::TimeUnit::MILLI) ? arrow::time32(unit) : arrow::time64(unit);
}

// The Arrow value type of a TIMESTAMP column of the given unit, UTC-adjusted iff `is_utc`.
static std::shared_ptr<arrow::DataType> temporal_timestamp_value_type(int32_t unit_selector, int32_t is_utc, const char *context)
{
	return arrow::timestamp(temporal_selector_to_arrow_unit(unit_selector, context), is_utc != 0 ? "UTC" : "");
} // GCOVR_EXCL_LINE -- gcov attribution artifact: this closing brace shows uncovered even though the covered `return` above proves the body ran.

// Builds a scalar (col_size == 1) or fixed-size-list (col_size > 1) Arrow TIME array from
// canonical nanoseconds-of-day input, scaling ns down to the target file unit. A value with
// finer precision than the unit (ns not divisible by the unit's ns count) aborts rather than
// silently truncating. SECOND/MILLI produce a TIME32 column, MICRO/NANO a TIME64 column
// (Arrow forbids the other two combinations), matching the value width Fortran expects on read.
static std::shared_ptr<arrow::Array> build_time_array(const int64_t *ns, int64_t nrows, int64_t col_size,
	int32_t unit_selector, const int8_t *valid_in, const char *name, const char *context)
{
	auto unit = temporal_selector_to_arrow_unit(unit_selector, context);
	int64_t scale = temporal_ns_per_unit(unit);
	bool use32 = (unit == arrow::TimeUnit::SECOND || unit == arrow::TimeUnit::MILLI);
	auto value_type = use32 ? arrow::time32(unit) : arrow::time64(unit);
	int64_t total = nrows * col_size;

	// Appends every (possibly null) scaled value into `vb` -- a Time32Builder or Time64Builder.
	auto append_all = [&](auto &vb)
	{
		using BuilderT = std::remove_reference_t<decltype(vb)>;
		using ValueT = typename BuilderT::value_type;
		for (int64_t i = 0; i < total; ++i)
		{
			arrow::Status s;
			if (valid_in != nullptr && valid_in[i] == 0)
			{
				s = vb.AppendNull();
			}
			else
			{
				if (ns[i] % scale != 0)
				{
					report_fatal_error(context, std::string("time value has finer precision than the column's declared "
						"unit for column: ") + name); // GCOVR_EXCL_LINE
				}
				s = vb.Append(static_cast<ValueT>(ns[i] / scale));
			}
			if (!s.ok()) throw std::runtime_error(s.ToString());
		}
	};

	std::shared_ptr<arrow::Array> array;
	if (col_size > 1)
	{
		check_col_size_fits_arrow_limit(col_size, name, context);
		std::shared_ptr<arrow::ArrayBuilder> value_builder = use32
			? std::static_pointer_cast<arrow::ArrayBuilder>(std::make_shared<arrow::Time32Builder>(value_type, arrow::default_memory_pool()))
			: std::static_pointer_cast<arrow::ArrayBuilder>(std::make_shared<arrow::Time64Builder>(value_type, arrow::default_memory_pool()));
		arrow::FixedSizeListBuilder list_builder(arrow::default_memory_pool(), value_builder, static_cast<int32_t>(col_size));
		auto s = list_builder.AppendValues(nrows);
		if (!s.ok()) throw std::runtime_error(s.ToString());
		if (use32) append_all(*std::static_pointer_cast<arrow::Time32Builder>(value_builder));
		else append_all(*std::static_pointer_cast<arrow::Time64Builder>(value_builder));
		s = list_builder.Finish(&array);
		if (!s.ok()) throw std::runtime_error(s.ToString());
	}
	else if (use32)
	{
		arrow::Time32Builder builder(value_type, arrow::default_memory_pool());
		append_all(builder);
		auto s = builder.Finish(&array);
		if (!s.ok()) throw std::runtime_error(s.ToString());
	}
	else
	{
		arrow::Time64Builder builder(value_type, arrow::default_memory_pool());
		append_all(builder);
		auto s = builder.Finish(&array);
		if (!s.ok()) throw std::runtime_error(s.ToString());
	}
	return array;
}

// Builds a scalar or fixed-size-list Arrow TIMESTAMP array from int64 values already expressed
// in `unit_selector`'s unit (Fortran did the canonical->unit conversion and its range check).
// `is_utc` selects the "UTC" (adjusted-to-UTC instant) vs "" (timezone-naive local) Arrow type.
static std::shared_ptr<arrow::Array> build_timestamp_array(const int64_t *values, int64_t nrows, int64_t col_size,
	int32_t unit_selector, int32_t is_utc, const int8_t *valid_in, const char *name, const char *context)
{
	auto unit = temporal_selector_to_arrow_unit(unit_selector, context);
	auto value_type = arrow::timestamp(unit, is_utc != 0 ? "UTC" : "");
	int64_t total = nrows * col_size;

	auto append_all = [&](arrow::TimestampBuilder &vb)
	{
		for (int64_t i = 0; i < total; ++i)
		{
			arrow::Status s = (valid_in != nullptr && valid_in[i] == 0) ? vb.AppendNull() : vb.Append(values[i]);
			if (!s.ok()) throw std::runtime_error(s.ToString());
		}
	};

	std::shared_ptr<arrow::Array> array;
	if (col_size > 1)
	{
		check_col_size_fits_arrow_limit(col_size, name, context);
		auto value_builder = std::make_shared<arrow::TimestampBuilder>(value_type, arrow::default_memory_pool());
		arrow::FixedSizeListBuilder list_builder(arrow::default_memory_pool(), value_builder, static_cast<int32_t>(col_size));
		auto s = list_builder.AppendValues(nrows);
		if (!s.ok()) throw std::runtime_error(s.ToString());
		append_all(*value_builder);
		s = list_builder.Finish(&array);
		if (!s.ok()) throw std::runtime_error(s.ToString());
	}
	else
	{
		arrow::TimestampBuilder builder(value_type, arrow::default_memory_pool());
		append_all(builder);
		auto s = builder.Finish(&array);
		if (!s.ok()) throw std::runtime_error(s.ToString());
	}
	return array;
}

// Stashes a streamed temporal row-group `array` into pending_chunk_arrays for `name`, building
// its field (always nullable, for the same reason append_typed_column_chunk documents) on the
// column's first chunk. Shared by the temporal parquet_write_*_column_chunk entry points.
static void stash_temporal_column_chunk(ParquetWriterHandle *writer_handle, const char *name, size_t idx,
	bool first_chunk_ever, const std::shared_ptr<arrow::Array> &array,
	const std::shared_ptr<arrow::DataType> &value_type, int64_t col_size)
{
	if (first_chunk_ever)
	{
		if (writer_handle->fields.size() <= idx) writer_handle->fields.resize(idx + 1);
		writer_handle->fields[idx] = build_field(name, value_type, col_size, /*nullable=*/true);
	}
	if (writer_handle->arrays.size() <= idx) writer_handle->arrays.resize(idx + 1);
	writer_handle->pending_chunk_arrays[static_cast<int>(idx)] = array;
}

// Resolves `name` to its leaf temporal value type (unwrapping a FIXED_SIZE_LIST vector column),
// schema-only (no column data read) -- shared by the parquet_reader_get_column_time_* queries.
static std::shared_ptr<arrow::DataType> resolve_temporal_value_type(ParquetReaderHandle *reader_handle, const char *name)
{
	auto resolved = resolve_struct_path(reader_handle->schema, name);
	auto type = resolved.leaf_field->type();
	if (type->id() == arrow::Type::FIXED_SIZE_LIST)
	{
		return std::static_pointer_cast<arrow::FixedSizeListType>(type)->value_type();
	}
	return type;
}

// Shared body for every temporal parquet_read_*_array_row entry point -- the temporal
// counterpart of read_list_primitive_row (which is a CType template and so cannot dispatch on
// the int32/int64 transport types date/time/timestamp reuse). `convert` is one of
// convert_{date,time,timestamp}_values; `unit_out`, when non-null (timestamp only), receives the
// column's stored unit selector.
template <typename OutType, typename ConvertFn>
static void read_temporal_row(void *handle, const char *name, int64_t row_index, OutType *data, int64_t col_size,
	int8_t *valid_out, const char *context, const char *type_name, int32_t *unit_out, ConvertFn convert)
{
	auto reader_handle = as_reader_handle(handle);
	std::shared_ptr<arrow::Array> array;
	int64_t local_row_index = row_index;
	if (reader_handle->filter_mask)
	{
		array = get_single_chunk_array(reader_handle, name);
	}
	else
	{
		int64_t row_group = 0;
		resolve_row_group_for_row(reader_handle, row_index, context, row_group, local_row_index);
		array = get_row_group_chunk_array(reader_handle, name, row_group, context);
	}
	auto vals_any = get_row_list_values(array, name, local_row_index, col_size, context);
	report_nulls_list_full(array, vals_any, name, 1, col_size, local_row_index - 1, valid_out, context);
	if (unit_out) *unit_out = timestamp_unit_selector_of(vals_any, name, context);
	convert(vals_any, data, col_size, name, context, 1, 0);
	fill_null_default(data, valid_out, col_size);
	mark_read(reader_handle, name, type_name, array);
}

// Shared body for every temporal parquet_read_*_array_element entry point -- the temporal
// counterpart of read_list_primitive_element. Streams row-group by row-group in the unfiltered
// case exactly as that function does.
template <typename OutType, typename ConvertFn>
static void read_temporal_element(void *handle, const char *name, int64_t col_index, OutType *data, int64_t nrows,
	int8_t *valid_out, const char *context, const char *type_name, int32_t *unit_out, ConvertFn convert)
{
	auto reader_handle = as_reader_handle(handle);
	if (reader_handle->filter_mask)
	{
		auto array = get_single_chunk_array(reader_handle, name);
		auto col_size = get_col_size(array);
		if (col_index < 1 || col_index > col_size)
		{
			report_fatal_error(context, "col_index out of bounds");
		}
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, context);
		auto offset = col_index - 1;
		report_nulls_list_element(array, vals_any, name, nrows, col_size, offset, valid_out, context);
		if (unit_out) *unit_out = timestamp_unit_selector_of(vals_any, name, context);
		convert(vals_any, data, nrows, name, context, col_size, offset);
		fill_null_default(data, valid_out, nrows);
		mark_read(reader_handle, name, type_name, array);
		return;
	}

	auto col_size = resolve_element_mode_col_size(reader_handle, name);
	if (col_index < 1 || col_index > col_size)
	{
		report_fatal_error(context, "col_index out of bounds");
	}
	auto offset = col_index - 1;
	auto last_array = stream_element_mode_row_groups(reader_handle, name, col_size, nrows, context,
		[&](const std::shared_ptr<arrow::Array> &array, const std::shared_ptr<arrow::Array> &vals_any,
			int64_t rg_rows, int64_t row_offset)
		{
			(void)array;
			int8_t *valid_slice = valid_out ? valid_out + row_offset : nullptr;
			report_nulls_list_element(array, vals_any, name, rg_rows, col_size, offset, valid_slice, context);
			if (unit_out) *unit_out = timestamp_unit_selector_of(vals_any, name, context);
			convert(vals_any, data + row_offset, rg_rows, name, context, col_size, offset);
		});
	fill_null_default(data, valid_out, nrows);
	if (!last_array)
	{
		// Zero-row column: no row group set last_array, and there are no values to derive the
		// unit from -- take it from the schema-declared leaf type instead (timestamp only).
		last_array = get_single_chunk_array(reader_handle, name);
		if (unit_out)
		{
			auto vt = resolve_temporal_value_type(reader_handle, name);
			*unit_out = arrow_unit_to_temporal_selector(std::static_pointer_cast<arrow::TimestampType>(vt)->unit());
		}
	}
	mark_read(reader_handle, name, type_name, last_array);
}

extern "C"
{

	// parquet_read_{int32,int64,float32,float64,bool8,string}_column are the
	// direct target of parquet_read_column -- the README documents that
	// reading a column whose physical Parquet type doesn't match what was
	// requested aborts the process via a C++-level abort, not a clean
	// Fortran error stop (see "Limitations"). The `default:` branch below
	// (an unrecognized physical type) calls report_fatal_error directly
	// instead of throwing: this project's own toolchain testing
	// found that a C++ exception thrown and caught within the very same
	// function can still go uncaught when the final executable is linked by
	// gfortran on macOS -- gfortran's driver passes `-no_compact_unwind` to
	// the linker, which breaks libc++abi's stack unwinding for objects
	// compiled by clang++, so a try/catch here would be unreliable across
	// this specific toolchain combination. Calling report_fatal_error
	// directly (same as ConcurrencyGuard elsewhere in this file) sidesteps
	// exception unwinding entirely and aborts cleanly regardless.
	void parquet_read_int32_column(void *handle, const char *name, int32_t *data, int64_t nrows, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		if (array->length() != nrows)
		{
			report_fatal_error("parquet_read_int32_column", std::string("nrows mismatch for column: ") + name);
		}
		check_or_report_nulls(array, name, valid_out, "parquet_read_int32_column");
		convert_values_to_int32(array, data, nrows, name, "parquet_read_int32_column");
		fill_null_default(data, valid_out, nrows);
		mark_read(reader_handle, name, "int32", array);
	}

	// Same as parquet_read_int32_column, but for int64.
	void parquet_read_int64_column(void *handle, const char *name, int64_t *data, int64_t nrows, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		if (array->length() != nrows)
		{
			report_fatal_error("parquet_read_int64_column", std::string("nrows mismatch for column: ") + name);
		}
		check_or_report_nulls(array, name, valid_out, "parquet_read_int64_column");
		convert_values_to_int64(array, data, nrows, name, "parquet_read_int64_column");
		fill_null_default(data, valid_out, nrows);
		mark_read(reader_handle, name, "int64", array);
	}

	// Same as parquet_read_int32_column, but for float32.
	void parquet_read_float32_column(void *handle, const char *name, float *data, int64_t nrows, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		if (array->length() != nrows)
		{
			report_fatal_error("parquet_read_float32_column", std::string("nrows mismatch for column: ") + name);
		}
		check_or_report_nulls(array, name, valid_out, "parquet_read_float32_column");
		convert_values_to_float32(array, data, nrows, name, "parquet_read_float32_column");
		fill_null_default(data, valid_out, nrows);
		mark_read(reader_handle, name, "float32", array);
	}

	// Same as parquet_read_int32_column, but for float64.
	void parquet_read_float64_column(void *handle, const char *name, double *data, int64_t nrows, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		if (array->length() != nrows)
		{
			report_fatal_error("parquet_read_float64_column", std::string("nrows mismatch for column: ") + name);
		}
		check_or_report_nulls(array, name, valid_out, "parquet_read_float64_column");
		convert_values_to_float64(array, data, nrows, name, "parquet_read_float64_column");
		fill_null_default(data, valid_out, nrows);
		mark_read(reader_handle, name, "float64", array);
	}

	// Same as parquet_read_int32_column, but for boolean (bool8) columns.
	void parquet_read_bool8_column(void *handle, const char *name, int8_t *data, int64_t nrows, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		if (array->type_id() != arrow::Type::BOOL)
		{
			report_fatal_error("parquet_read_bool8_column", std::string("type mismatch for column: ") + name +
				" (expected bool, got " + array->type()->ToString() + ")"); // GCOVR_EXCL_LINE
		}
		auto arr = std::static_pointer_cast<arrow::BooleanArray>(array);
		if (arr->length() != nrows)
		{
			report_fatal_error("parquet_read_bool8_column", std::string("nrows mismatch for column: ") + name);
		}
		check_or_report_nulls(arr, name, valid_out, "parquet_read_bool8_column");
		for (int64_t i = 0; i < nrows; ++i)
		{
			data[i] = arr->Value(i) ? 1 : 0;
		}
		fill_null_default(data, valid_out, nrows);
		mark_read(reader_handle, name, "bool8", array);
	}

	// Same as parquet_read_int32_column, but for string columns (fixed-width, space-padded output).
	void parquet_read_string_column(void *handle, const char *name, char *data, int64_t item_len, int64_t nrows, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		if (!is_string_like_type(array->type_id()))
		{
			report_fatal_error("parquet_read_string_column", std::string("type mismatch for column: ") + name +
				" (expected string, got " + array->type()->ToString() + ")"); // GCOVR_EXCL_LINE
		}
		auto arr = make_string_like_accessor(array);
		if (arr.length != nrows)
		{
			report_fatal_error("parquet_read_string_column", std::string("nrows mismatch for column: ") + name);
		}
		check_or_report_nulls(array, name, valid_out, "parquet_read_string_column");
		for (int64_t i = 0; i < nrows; ++i)
		{
			auto view = arr.get_view(i);
			copy_string_with_padding(data + i * item_len, item_len, view);
		}
		fill_null_default_string(data, item_len, valid_out, nrows);
		mark_read_string(reader_handle, name, item_len, array);
	}

	// Compact counterpart to parquet_read_string_column, above: instead of copying one string at
	// a time into a fixed-width padded Fortran buffer, hands back the decoded column's own
	// offsets/data/validity buffers directly (see extract_string_buffers), for the caller
	// (parquet_read.f90) to bulk-append straight into a parquet_string_column via its own
	// append_buffers. The array is already kept alive indefinitely by get_single_chunk_array's
	// own column_cache, so -- unlike the row-group-scoped chunk variant below -- no extra pinning
	// is needed here: the returned pointers stay valid for the reader's whole remaining lifetime,
	// not just until the next call.
	void parquet_read_string_column_buffers(void *handle, const char *name,
		int64_t *nrows_out, int64_t *nchars_out,
		const void **offsets_out, const void **data_out, const void **validity_out,
		int8_t *offsets_int32_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		if (!is_offset_string_type(array->type_id()))
		{
			report_fatal_error("parquet_read_column", std::string("type mismatch for column: ") + name +
				" (expected string, got " + array->type()->ToString() + // GCOVR_EXCL_LINE
				(array->type_id() == arrow::Type::STRING_VIEW ? // GCOVR_EXCL_LINE
					" -- STRING_VIEW columns are not supported by this compact buffer read; " // GCOVR_EXCL_LINE
					"use a fixed-width parquet_read_column instead" : "") + ")"); // GCOVR_EXCL_LINE
		}
		extract_string_buffers(array, nrows_out, nchars_out, offsets_out, data_out, validity_out, offsets_int32_out);
		mark_read(reader_handle, name, "string", array);
	}

	// Reads the full vector int32 column `name` (every row) into `data`.
	void parquet_read_int32_array_column(void *handle, const char *name, int32_t *data, int64_t nrows, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, "parquet_read_int32_array_column");
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out, "parquet_read_int32_array_column");
		int64_t total = nrows * col_size;
		convert_values_to_int32(vals_any, data, total, name, "parquet_read_int32_array_column");
		fill_null_default(data, valid_out, total);
		mark_read(reader_handle, name, "int32", array);
	}

	// Same as parquet_read_int32_array_column, but for int64.
	void parquet_read_int64_array_column(void *handle, const char *name, int64_t *data, int64_t nrows, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, "parquet_read_int64_array_column");
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out, "parquet_read_int64_array_column");
		int64_t total = nrows * col_size;
		convert_values_to_int64(vals_any, data, total, name, "parquet_read_int64_array_column");
		fill_null_default(data, valid_out, total);
		mark_read(reader_handle, name, "int64", array);
	}

	// Same as parquet_read_int32_array_column, but for float32.
	void parquet_read_float32_array_column(void *handle, const char *name, float *data, int64_t nrows, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, "parquet_read_float32_array_column");
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out, "parquet_read_float32_array_column");
		int64_t total = nrows * col_size;
		convert_values_to_float32(vals_any, data, total, name, "parquet_read_float32_array_column");
		fill_null_default(data, valid_out, total);
		mark_read(reader_handle, name, "float32", array);
	}

	// Same as parquet_read_int32_array_column, but for float64.
	void parquet_read_float64_array_column(void *handle, const char *name, double *data, int64_t nrows, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, "parquet_read_float64_array_column");
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out, "parquet_read_float64_array_column");
		int64_t total = nrows * col_size;
		convert_values_to_float64(vals_any, data, total, name, "parquet_read_float64_array_column");
		fill_null_default(data, valid_out, total);
		mark_read(reader_handle, name, "float64", array);
	}

	// Same as parquet_read_int32_array_column, but for boolean (bool8) columns.
	void parquet_read_bool8_array_column(void *handle, const char *name, int8_t *data, int64_t nrows, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, "parquet_read_bool8_array_column");
		if (vals_any->type_id() != arrow::Type::BOOL)
		{
			report_fatal_error("parquet_read_bool8_array_column", std::string("type mismatch for list values in column: ") + name +
				" (expected bool, got " + vals_any->type()->ToString() + ")"); // GCOVR_EXCL_LINE
		}
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out, "parquet_read_bool8_array_column");
		auto vals = std::static_pointer_cast<arrow::BooleanArray>(vals_any);
		for (int64_t i = 0; i < nrows * col_size; ++i)
		{
			data[i] = vals->Value(i) ? 1 : 0;
		}
		fill_null_default(data, valid_out, nrows * col_size);
		mark_read(reader_handle, name, "bool8", array);
	}

	// Same as parquet_read_int32_array_column, but for string columns (fixed-width, space-padded output).
	void parquet_read_string_array_column(void *handle, const char *name, char *data, int64_t item_len, int64_t nrows, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, "parquet_read_string_array_column");
		if (!is_string_like_type(vals_any->type_id()))
		{
			report_fatal_error("parquet_read_string_array_column", std::string("type mismatch for list values in column: ") + name +
				" (expected string, got " + vals_any->type()->ToString() + ")"); // GCOVR_EXCL_LINE
		}
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out, "parquet_read_string_array_column");
		auto vals = make_string_like_accessor(vals_any);
		for (int64_t i = 0; i < nrows * col_size; ++i)
		{
			copy_string_with_padding(data + i * item_len, item_len, vals.get_view(i));
		}
		fill_null_default_string(data, item_len, valid_out, nrows * col_size);
		mark_read_string(reader_handle, name, item_len, array);
	}

	// Reads one row (row_index) of vector int32 column `name` into `data`.
	void parquet_read_int32_array_row(void *handle, const char *name, int64_t row_index, int32_t *data, int64_t col_size, int8_t *valid_out)
	{
		read_list_primitive_row<int32_t>(handle, name, row_index, data, col_size, valid_out);
	}

	// Same as parquet_read_int32_array_row, but for int64.
	void parquet_read_int64_array_row(void *handle, const char *name, int64_t row_index, int64_t *data, int64_t col_size, int8_t *valid_out)
	{
		read_list_primitive_row<int64_t>(handle, name, row_index, data, col_size, valid_out);
	}

	// Same as parquet_read_int32_array_row, but for float32.
	void parquet_read_float32_array_row(void *handle, const char *name, int64_t row_index, float *data, int64_t col_size, int8_t *valid_out)
	{
		read_list_primitive_row<float>(handle, name, row_index, data, col_size, valid_out);
	}

	// Same as parquet_read_int32_array_row, but for float64.
	void parquet_read_float64_array_row(void *handle, const char *name, int64_t row_index, double *data, int64_t col_size, int8_t *valid_out)
	{
		read_list_primitive_row<double>(handle, name, row_index, data, col_size, valid_out);
	}

	// Same as parquet_read_int32_array_row, but for boolean (bool8) columns (not templated,
	// since bool8 has no primitive Arrow numeric type to widen/narrow via convert_values_to_*).
	void parquet_read_bool8_array_row(void *handle, const char *name, int64_t row_index, int8_t *data, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		// See read_list_primitive_row's identical comment: a filter mask keeps the old
		// whole-column path (row_index means "index into the filtered array" there); the
		// unfiltered common case is row-group-scoped so one row can be fetched without
		// materializing the whole column.
		std::shared_ptr<arrow::Array> array;
		int64_t local_row_index = row_index;
		if (reader_handle->filter_mask)
		{
			array = get_single_chunk_array(reader_handle, name);
		}
		else
		{
			int64_t row_group = 0;
			resolve_row_group_for_row(reader_handle, row_index, "parquet_read_bool8_array_row", row_group, local_row_index);
			array = get_row_group_chunk_array(reader_handle, name, row_group, "parquet_read_bool8_array_row");
		}
		auto vals_any = get_row_list_values(array, name, local_row_index, col_size, "parquet_read_bool8_array_row");
		if (vals_any->type_id() != arrow::Type::BOOL)
			report_fatal_error("parquet_read_bool8_array_row", std::string("type mismatch for list values in column: ") + name +
				" (expected bool, got " + vals_any->type()->ToString() + ")"); // GCOVR_EXCL_LINE
		report_nulls_list_full(array, vals_any, name, 1, col_size, local_row_index - 1, valid_out, "parquet_read_bool8_array_row");

		auto vals = std::static_pointer_cast<arrow::BooleanArray>(vals_any);
		for (int64_t j = 0; j < col_size; ++j)
		{
			data[j] = vals->Value(j) ? 1 : 0;
		}
		fill_null_default(data, valid_out, col_size);
		mark_read(reader_handle, name, "bool8", array);
	}

	// Same as parquet_read_bool8_array_row, but for string columns (fixed-width, space-padded output).
	void parquet_read_string_array_row(void *handle, const char *name, int64_t row_index, char *data, int64_t item_len, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		// See read_list_primitive_row's identical comment: a filter mask keeps the old
		// whole-column path (row_index means "index into the filtered array" there); the
		// unfiltered common case is row-group-scoped so one row can be fetched without
		// materializing the whole column.
		std::shared_ptr<arrow::Array> array;
		int64_t local_row_index = row_index;
		if (reader_handle->filter_mask)
		{
			array = get_single_chunk_array(reader_handle, name);
		}
		else
		{
			int64_t row_group = 0;
			resolve_row_group_for_row(reader_handle, row_index, "parquet_read_string_array_row", row_group, local_row_index);
			array = get_row_group_chunk_array(reader_handle, name, row_group, "parquet_read_string_array_row");
		}
		auto vals_any = get_row_list_values(array, name, local_row_index, col_size, "parquet_read_string_array_row");
		if (!is_string_like_type(vals_any->type_id()))
			report_fatal_error("parquet_read_string_array_row", std::string("type mismatch for list values in column: ") + name +
				" (expected string, got " + vals_any->type()->ToString() + ")"); // GCOVR_EXCL_LINE
		report_nulls_list_full(array, vals_any, name, 1, col_size, local_row_index - 1, valid_out, "parquet_read_string_array_row");

		auto vals = make_string_like_accessor(vals_any);
		for (int64_t j = 0; j < col_size; ++j)
		{
			copy_string_with_padding(data + j * item_len, item_len, vals.get_view(j));
		}
		fill_null_default_string(data, item_len, valid_out, col_size);
		mark_read_string(reader_handle, name, item_len, array);
	}

	// Reads one element position (col_index) of vector int32 column `name` across every row into `data`.
	void parquet_read_int32_array_element(void *handle, const char *name, int64_t col_index, int32_t *data, int64_t nrows, int64_t, int8_t *valid_out)
	{
		read_list_primitive_element<int32_t>(handle, name, col_index, data, nrows, valid_out);
	}

	// Same as parquet_read_int32_array_element, but for int64.
	void parquet_read_int64_array_element(void *handle, const char *name, int64_t col_index, int64_t *data, int64_t nrows, int64_t, int8_t *valid_out)
	{
		read_list_primitive_element<int64_t>(handle, name, col_index, data, nrows, valid_out);
	}

	// Same as parquet_read_int32_array_element, but for float32.
	void parquet_read_float32_array_element(void *handle, const char *name, int64_t col_index, float *data, int64_t nrows, int64_t, int8_t *valid_out)
	{
		read_list_primitive_element<float>(handle, name, col_index, data, nrows, valid_out);
	}

	// Same as parquet_read_int32_array_element, but for float64.
	void parquet_read_float64_array_element(void *handle, const char *name, int64_t col_index, double *data, int64_t nrows, int64_t, int8_t *valid_out)
	{
		read_list_primitive_element<double>(handle, name, col_index, data, nrows, valid_out);
	}

	// Same as parquet_read_int32_array_element, but for boolean (bool8) columns (not templated,
	// since bool8 has no primitive Arrow numeric type to widen/narrow via convert_values_to_*).
	// Streams row group by row group when unfiltered -- see stream_element_mode_row_groups's own
	// comment for why element mode (unlike row_mode) needs every row group, not just one.
	void parquet_read_bool8_array_element(void *handle, const char *name, int64_t col_index, int8_t *data, int64_t nrows, int64_t, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		const char *context = "parquet_read_bool8_array_element";

		// See read_list_primitive_element's identical comment: a filter mask keeps the old
		// whole-column path.
		if (reader_handle->filter_mask)
		{
			auto array = get_single_chunk_array(reader_handle, name);
			auto col_size = get_col_size(array);
			if (col_index < 1 || col_index > col_size)
				report_fatal_error(context, "col_index out of bounds");
			auto vals_any = get_uniform_list_values(array, name, nrows, col_size, context);
			if (vals_any->type_id() != arrow::Type::BOOL)
				report_fatal_error(context, std::string("type mismatch for list values in column: ") + name +
					" (expected bool, got " + vals_any->type()->ToString() + ")"); // GCOVR_EXCL_LINE
			auto offset = col_index - 1;
			report_nulls_list_element(array, vals_any, name, nrows, col_size, offset, valid_out, context);
			auto vals = std::static_pointer_cast<arrow::BooleanArray>(vals_any);
			for (int64_t i = 0; i < nrows; ++i)
			{
				data[i] = vals->Value(i * col_size + offset) ? 1 : 0;
			}
			fill_null_default(data, valid_out, nrows);
			mark_read(reader_handle, name, "bool8", array);
			return;
		}

		auto col_size = resolve_element_mode_col_size(reader_handle, name);
		if (col_index < 1 || col_index > col_size)
			report_fatal_error(context, "col_index out of bounds");
		auto offset = col_index - 1;
		auto last_array = stream_element_mode_row_groups(reader_handle, name, col_size, nrows, context,
			// GCOVR_EXCL_START -- gcov attribution artifact under GCC: this lambda parameter-list
			// line shows uncovered even though this bool element-mode read path is directly
			// exercised by the vector-column test suite (its body's report_fatal_error is
			// separately excluded via the CI pattern rule below).
			[&](const std::shared_ptr<arrow::Array> &array, const std::shared_ptr<arrow::Array> &vals_any,
				// GCOVR_EXCL_STOP
				int64_t rg_rows, int64_t row_offset)
			{
				if (vals_any->type_id() != arrow::Type::BOOL)
					report_fatal_error(context, std::string("type mismatch for list values in column: ") + name +
						" (expected bool, got " + vals_any->type()->ToString() + ")"); // GCOVR_EXCL_LINE
				int8_t *valid_slice = valid_out ? valid_out + row_offset : nullptr;
				report_nulls_list_element(array, vals_any, name, rg_rows, col_size, offset, valid_slice, context);
				auto vals = std::static_pointer_cast<arrow::BooleanArray>(vals_any);
				for (int64_t i = 0; i < rg_rows; ++i)
				{
					data[row_offset + i] = vals->Value(i * col_size + offset) ? 1 : 0;
				}
			});
		fill_null_default(data, valid_out, nrows);
		if (!last_array) last_array = get_single_chunk_array(reader_handle, name);
		mark_read(reader_handle, name, "bool8", last_array);
	}

	// Same as parquet_read_bool8_array_element, but for string columns (fixed-width, space-padded
	// output). Streams row group by row group when unfiltered, same as
	// parquet_read_bool8_array_element above.
	void parquet_read_string_array_element(void *handle, const char *name, int64_t col_index, char *data, int64_t item_len, int64_t nrows, int64_t, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		const char *context = "parquet_read_string_array_element";

		// See read_list_primitive_element's identical comment: a filter mask keeps the old
		// whole-column path.
		if (reader_handle->filter_mask)
		{
			auto array = get_single_chunk_array(reader_handle, name);
			auto col_size = get_col_size(array);
			if (col_index < 1 || col_index > col_size)
				report_fatal_error(context, "col_index out of bounds");
			auto vals_any = get_uniform_list_values(array, name, nrows, col_size, context);
			if (!is_string_like_type(vals_any->type_id()))
				report_fatal_error(context, std::string("type mismatch for list values in column: ") + name +
					" (expected string, got " + vals_any->type()->ToString() + ")"); // GCOVR_EXCL_LINE
			auto offset = col_index - 1;
			report_nulls_list_element(array, vals_any, name, nrows, col_size, offset, valid_out, context);
			auto vals = make_string_like_accessor(vals_any);
			for (int64_t i = 0; i < nrows; ++i)
			{
				copy_string_with_padding(data + i * item_len, item_len, vals.get_view(i * col_size + offset));
			}
			fill_null_default_string(data, item_len, valid_out, nrows);
			mark_read_string(reader_handle, name, item_len, array);
			return;
		}

		auto col_size = resolve_element_mode_col_size(reader_handle, name);
		if (col_index < 1 || col_index > col_size)
			report_fatal_error(context, "col_index out of bounds");
		auto offset = col_index - 1;
		auto last_array = stream_element_mode_row_groups(reader_handle, name, col_size, nrows, context,
			// GCOVR_EXCL_START -- gcov attribution artifact under GCC: this lambda parameter-list
			// line shows uncovered even though this string element-mode read path is directly
			// exercised by the vector-column test suite (its body's report_fatal_error is
			// separately excluded via the CI pattern rule below).
			[&](const std::shared_ptr<arrow::Array> &array, const std::shared_ptr<arrow::Array> &vals_any,
				// GCOVR_EXCL_STOP
				int64_t rg_rows, int64_t row_offset)
			{
				if (!is_string_like_type(vals_any->type_id()))
					report_fatal_error(context, std::string("type mismatch for list values in column: ") + name +
						" (expected string, got " + vals_any->type()->ToString() + ")"); // GCOVR_EXCL_LINE
				int8_t *valid_slice = valid_out ? valid_out + row_offset : nullptr;
				report_nulls_list_element(array, vals_any, name, rg_rows, col_size, offset, valid_slice, context);
				auto vals = make_string_like_accessor(vals_any);
				for (int64_t i = 0; i < rg_rows; ++i)
				{
					copy_string_with_padding(data + (row_offset + i) * item_len, item_len, vals.get_view(i * col_size + offset));
				}
			});
		fill_null_default_string(data, item_len, valid_out, nrows);
		if (!last_array) last_array = get_single_chunk_array(reader_handle, name);
		mark_read_string(reader_handle, name, item_len, last_array);
	}

	// ------------------------------------------------------------------------
	// Temporal read entry points (date/time/timestamp). See the transport note
	// on convert_date_values/build_time_array above. Each mirrors its int64
	// counterpart, swapping in the temporal value converter; timestamp variants
	// additionally report the column's stored unit via `unit_out`.
	// ------------------------------------------------------------------------

	// --- date (int32 days) ---
	void parquet_read_date_column(void *handle, const char *name, int32_t *data, int64_t nrows, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		if (array->length() != nrows) report_fatal_error("parquet_read_date_column", std::string("nrows mismatch for column: ") + name);
		check_or_report_nulls(array, name, valid_out, "parquet_read_date_column");
		convert_date_values(array, data, nrows, name, "parquet_read_date_column");
		fill_null_default(data, valid_out, nrows);
		mark_read(reader_handle, name, "date", array);
	}

	void parquet_read_date_array_column(void *handle, const char *name, int32_t *data, int64_t nrows, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, "parquet_read_date_array_column");
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out, "parquet_read_date_array_column");
		convert_date_values(vals_any, data, nrows * col_size, name, "parquet_read_date_array_column");
		fill_null_default(data, valid_out, nrows * col_size);
		mark_read(reader_handle, name, "date", array);
	}

	void parquet_read_date_column_chunk(void *handle, const char *name, int64_t row_group, int32_t *data, int64_t nrows, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_row_group_chunk_array(reader_handle, name, row_group, "parquet_read_date_column_chunk");
		if (array->length() != nrows) report_fatal_error("parquet_read_date_column_chunk", std::string("nrows mismatch for column: ") + name);
		check_or_report_nulls(array, name, valid_out, "parquet_read_date_column_chunk");
		convert_date_values(array, data, nrows, name, "parquet_read_date_column_chunk");
		fill_null_default(data, valid_out, nrows);
	}

	void parquet_read_date_array_column_chunk(void *handle, const char *name, int64_t row_group, int32_t *data, int64_t nrows, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_row_group_chunk_array(reader_handle, name, row_group, "parquet_read_date_array_column_chunk");
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, "parquet_read_date_array_column_chunk");
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out, "parquet_read_date_array_column_chunk");
		convert_date_values(vals_any, data, nrows * col_size, name, "parquet_read_date_array_column_chunk");
		fill_null_default(data, valid_out, nrows * col_size);
	}

	void parquet_read_date_array_row(void *handle, const char *name, int64_t row_index, int32_t *data, int64_t col_size, int8_t *valid_out)
	{
		read_temporal_row(handle, name, row_index, data, col_size, valid_out, "parquet_read_array_row_mode", "date",
			nullptr, convert_date_values);
	}

	void parquet_read_date_array_element(void *handle, const char *name, int64_t col_index, int32_t *data, int64_t nrows, int64_t, int8_t *valid_out)
	{
		read_temporal_element(handle, name, col_index, data, nrows, valid_out, "parquet_read_array_element_mode", "date",
			nullptr, convert_date_values);
	}

	// --- time (int64 canonical nanoseconds-of-day) ---
	void parquet_read_time_column(void *handle, const char *name, int64_t *data, int64_t nrows, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		if (array->length() != nrows) report_fatal_error("parquet_read_time_column", std::string("nrows mismatch for column: ") + name);
		check_or_report_nulls(array, name, valid_out, "parquet_read_time_column");
		convert_time_values(array, data, nrows, name, "parquet_read_time_column");
		fill_null_default(data, valid_out, nrows);
		mark_read(reader_handle, name, "time", array);
	}

	void parquet_read_time_array_column(void *handle, const char *name, int64_t *data, int64_t nrows, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, "parquet_read_time_array_column");
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out, "parquet_read_time_array_column");
		convert_time_values(vals_any, data, nrows * col_size, name, "parquet_read_time_array_column");
		fill_null_default(data, valid_out, nrows * col_size);
		mark_read(reader_handle, name, "time", array);
	}

	void parquet_read_time_column_chunk(void *handle, const char *name, int64_t row_group, int64_t *data, int64_t nrows, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_row_group_chunk_array(reader_handle, name, row_group, "parquet_read_time_column_chunk");
		if (array->length() != nrows) report_fatal_error("parquet_read_time_column_chunk", std::string("nrows mismatch for column: ") + name);
		check_or_report_nulls(array, name, valid_out, "parquet_read_time_column_chunk");
		convert_time_values(array, data, nrows, name, "parquet_read_time_column_chunk");
		fill_null_default(data, valid_out, nrows);
	}

	void parquet_read_time_array_column_chunk(void *handle, const char *name, int64_t row_group, int64_t *data, int64_t nrows, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_row_group_chunk_array(reader_handle, name, row_group, "parquet_read_time_array_column_chunk");
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, "parquet_read_time_array_column_chunk");
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out, "parquet_read_time_array_column_chunk");
		convert_time_values(vals_any, data, nrows * col_size, name, "parquet_read_time_array_column_chunk");
		fill_null_default(data, valid_out, nrows * col_size);
	}

	void parquet_read_time_array_row(void *handle, const char *name, int64_t row_index, int64_t *data, int64_t col_size, int8_t *valid_out)
	{
		read_temporal_row(handle, name, row_index, data, col_size, valid_out, "parquet_read_array_row_mode", "time",
			nullptr, convert_time_values);
	}

	void parquet_read_time_array_element(void *handle, const char *name, int64_t col_index, int64_t *data, int64_t nrows, int64_t, int8_t *valid_out)
	{
		read_temporal_element(handle, name, col_index, data, nrows, valid_out, "parquet_read_array_element_mode", "time",
			nullptr, convert_time_values);
	}

	// --- timestamp (int64 value in the column's own unit + reported `unit_out`) ---
	void parquet_read_timestamp_column(void *handle, const char *name, int64_t *data, int64_t nrows, int32_t *unit_out, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		if (array->length() != nrows) report_fatal_error("parquet_read_timestamp_column", std::string("nrows mismatch for column: ") + name);
		check_or_report_nulls(array, name, valid_out, "parquet_read_timestamp_column");
		*unit_out = timestamp_unit_selector_of(array, name, "parquet_read_timestamp_column");
		convert_timestamp_values(array, data, nrows, name, "parquet_read_timestamp_column");
		fill_null_default(data, valid_out, nrows);
		mark_read(reader_handle, name, "timestamp", array);
	}

	void parquet_read_timestamp_array_column(void *handle, const char *name, int64_t *data, int64_t nrows, int64_t col_size, int32_t *unit_out, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, "parquet_read_timestamp_array_column");
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out, "parquet_read_timestamp_array_column");
		*unit_out = timestamp_unit_selector_of(vals_any, name, "parquet_read_timestamp_array_column");
		convert_timestamp_values(vals_any, data, nrows * col_size, name, "parquet_read_timestamp_array_column");
		fill_null_default(data, valid_out, nrows * col_size);
		mark_read(reader_handle, name, "timestamp", array);
	}

	void parquet_read_timestamp_column_chunk(void *handle, const char *name, int64_t row_group, int64_t *data, int64_t nrows, int32_t *unit_out, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_row_group_chunk_array(reader_handle, name, row_group, "parquet_read_timestamp_column_chunk");
		if (array->length() != nrows) report_fatal_error("parquet_read_timestamp_column_chunk", std::string("nrows mismatch for column: ") + name);
		check_or_report_nulls(array, name, valid_out, "parquet_read_timestamp_column_chunk");
		*unit_out = timestamp_unit_selector_of(array, name, "parquet_read_timestamp_column_chunk");
		convert_timestamp_values(array, data, nrows, name, "parquet_read_timestamp_column_chunk");
		fill_null_default(data, valid_out, nrows);
	}

	void parquet_read_timestamp_array_column_chunk(void *handle, const char *name, int64_t row_group, int64_t *data, int64_t nrows, int64_t col_size, int32_t *unit_out, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_row_group_chunk_array(reader_handle, name, row_group, "parquet_read_timestamp_array_column_chunk");
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, "parquet_read_timestamp_array_column_chunk");
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out, "parquet_read_timestamp_array_column_chunk");
		*unit_out = timestamp_unit_selector_of(vals_any, name, "parquet_read_timestamp_array_column_chunk");
		convert_timestamp_values(vals_any, data, nrows * col_size, name, "parquet_read_timestamp_array_column_chunk");
		fill_null_default(data, valid_out, nrows * col_size);
	}

	void parquet_read_timestamp_array_row(void *handle, const char *name, int64_t row_index, int64_t *data, int64_t col_size, int32_t *unit_out, int8_t *valid_out)
	{
		read_temporal_row(handle, name, row_index, data, col_size, valid_out, "parquet_read_array_row_mode", "timestamp",
			unit_out, convert_timestamp_values);
	}

	void parquet_read_timestamp_array_element(void *handle, const char *name, int64_t col_index, int64_t *data, int64_t nrows, int64_t, int32_t *unit_out, int8_t *valid_out)
	{
		read_temporal_element(handle, name, col_index, data, nrows, valid_out, "parquet_read_array_element_mode", "timestamp",
			unit_out, convert_timestamp_values);
	}

	// --- column time-unit / timezone queries (parquet_get_column_time_info) ---

	// Returns the parquet_unit_* selector (1..4) of a TIME/TIMESTAMP column `name`; aborts for
	// any other column type (there is no unit to report). Schema-only (reads no column data).
	int32_t parquet_reader_get_column_time_unit(void *handle, const char *name)
	{
		auto reader_handle = as_reader_handle(handle);
		auto vt = resolve_temporal_value_type(reader_handle, name);
		switch (vt->id())
		{
		case arrow::Type::TIME32: return arrow_unit_to_temporal_selector(std::static_pointer_cast<arrow::Time32Type>(vt)->unit());
		case arrow::Type::TIME64: return arrow_unit_to_temporal_selector(std::static_pointer_cast<arrow::Time64Type>(vt)->unit());
		case arrow::Type::TIMESTAMP: return arrow_unit_to_temporal_selector(std::static_pointer_cast<arrow::TimestampType>(vt)->unit());
		default: // GCOVR_EXCL_LINE -- gcov attribution artifact under GCC: this label shows
		// uncovered even though the report_fatal_error() below is already excluded via the CI
		// pattern rule and this default is genuinely never taken by a covered test.
			report_fatal_error("parquet_get_column_time_info", std::string("column is not a time/timestamp column: ") + name +
				" (type " + vt->ToString() + ")"); // GCOVR_EXCL_LINE
		}
		return 3; // unreachable
	}

	// Byte length of a TIMESTAMP column's timezone string (0 for a naive timestamp or a TIME
	// column); aborts for a non-time/timestamp column.
	int64_t parquet_reader_get_column_timezone_length(void *handle, const char *name)
	{
		auto reader_handle = as_reader_handle(handle);
		auto vt = resolve_temporal_value_type(reader_handle, name);
		if (vt->id() == arrow::Type::TIMESTAMP)
			return static_cast<int64_t>(std::static_pointer_cast<arrow::TimestampType>(vt)->timezone().size());
		if (vt->id() == arrow::Type::TIME32 || vt->id() == arrow::Type::TIME64) return 0;
		report_fatal_error("parquet_get_column_time_info", std::string("column is not a time/timestamp column: ") + name +
			" (type " + vt->ToString() + ")"); // GCOVR_EXCL_LINE
		return 0; // unreachable
	}

	// Copies a TIMESTAMP column's timezone string into `buf` (blank for naive/TIME).
	void parquet_reader_get_column_timezone(void *handle, const char *name, char *buf, int64_t buf_len)
	{
		auto reader_handle = as_reader_handle(handle);
		auto vt = resolve_temporal_value_type(reader_handle, name);
		std::string tz;
		if (vt->id() == arrow::Type::TIMESTAMP) tz = std::static_pointer_cast<arrow::TimestampType>(vt)->timezone();
		// GCOVR_EXCL_START -- dead: parquet_read.f90's parquet_get_column_time_info only calls this
		// function when parquet_reader_get_column_timezone_length's own result was > 0, which is
		// only ever true for TIMESTAMP -- so this function is never reached at all (TIMESTAMP or
		// otherwise) except with vt->id() == TIMESTAMP, making this whole else-if branch (and the
		// TIME32/TIME64 case it's meant to skip past) unreachable through the public API.
		else if (vt->id() != arrow::Type::TIME32 && vt->id() != arrow::Type::TIME64)
			report_fatal_error("parquet_get_column_time_info", std::string("column is not a time/timestamp column: ") + name);
		// GCOVR_EXCL_STOP
		copy_string_with_padding(buf, buf_len, tz);
	}

} // extern "C"

	// Reads (uncached, always freshly from disk) row group `row_group`'s data for column `name`,
	// bypassing get_single_chunk_array's whole-column cache entirely -- this is the whole point of
	// a row-group-chunked read: bounded memory, one row group at a time, never materializing the
	// whole column. Disallowed together with an active filter (checked Fortran-side via
	// parquet_reader_has_filter, before this is ever reached): the filter mask is a single flat
	// mask sized to the *whole unfiltered file*, with no row-group structure of its own, so there
	// is no coherent way to say "this filtered subset of row group N" without an entirely separate
	// filter-to-row-group mapping -- out of scope for this version (see doc/pages/reading.md's
	// "Streaming/chunked reads" section). Also records `row_group` as read (for
	// parquet_reader_check_complete) and runs per-row-group qc (run_qc_checks -- reusing the exact
	// same whole-column check functions, just scoped to this one row group's own array: a
	// hard-mode violation aborts naming this row group, a soft-mode one warns at most once per
	// column, same throttling as every other read path).
	static std::shared_ptr<arrow::Array> get_row_group_chunk_array(ParquetReaderHandle *reader_handle,
		const char *name, int64_t row_group, const char *context)
	{
		auto resolved = resolve_struct_path(reader_handle->schema, name);
		auto idx = get_column_index(reader_handle, resolved.top_level_name.c_str());
		auto leaf_idx = resolve_single_leaf_index(reader_handle, static_cast<int>(idx), resolved.child_path);
		std::shared_ptr<arrow::Table> table;
		auto status =
			reader_handle->reader->ReadRowGroup(static_cast<int>(row_group - 1), {static_cast<int>(leaf_idx)}, &table);
		if (!status.ok())
		{ // GCOVR_EXCL_START -- I/O backstop: row_group is already validated by
		  // resolve_row_group_for_row before this is ever called.
			throw std::runtime_error(status.ToString());
		}
		// GCOVR_EXCL_STOP
		auto array = combine_column_chunks(table->column(0), resolved.top_level_name);
		if (!resolved.child_path.empty())
		{
			array = unwrap_struct_path(array, resolved.child_path);
		}
		reader_handle->chunk_read_row_groups[static_cast<int>(idx)].insert(row_group);
		run_qc_checks(reader_handle, name, std::string(name) + " [row group " + std::to_string(row_group) + "]", array);
		return array;
	}

extern "C"
{

	// parquet_read_{int32,int64,float32,float64,bool8,string}_column_chunk are the direct targets
	// of parquet_read_column_chunk -- the row-group-scoped counterpart of parquet_read_*_column
	// (see get_row_group_chunk_array, above), for reading a large column one row group at a time
	// instead of materializing it whole. Same null/type/nrows-mismatch reporting contract as the
	// whole-column reads; no mark_read call, since was_read/output_type_used (parquet_reader_print_stat)
	// are scoped to whole-column reads only.
	void parquet_read_int32_column_chunk(void *handle, const char *name, int64_t row_group, int32_t *data, int64_t nrows, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_row_group_chunk_array(reader_handle, name, row_group, "parquet_read_int32_column_chunk");
		if (array->length() != nrows)
		{
			report_fatal_error("parquet_read_int32_column_chunk", std::string("nrows mismatch for column: ") + name);
		}
		check_or_report_nulls(array, name, valid_out, "parquet_read_int32_column_chunk");
		convert_values_to_int32(array, data, nrows, name, "parquet_read_int32_column_chunk");
		fill_null_default(data, valid_out, nrows);
	}

	// Same as parquet_read_int32_column_chunk, but for int64.
	void parquet_read_int64_column_chunk(void *handle, const char *name, int64_t row_group, int64_t *data, int64_t nrows, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_row_group_chunk_array(reader_handle, name, row_group, "parquet_read_int64_column_chunk");
		if (array->length() != nrows)
		{
			report_fatal_error("parquet_read_int64_column_chunk", std::string("nrows mismatch for column: ") + name);
		}
		check_or_report_nulls(array, name, valid_out, "parquet_read_int64_column_chunk");
		convert_values_to_int64(array, data, nrows, name, "parquet_read_int64_column_chunk");
		fill_null_default(data, valid_out, nrows);
	}

	// Same as parquet_read_int32_column_chunk, but for float32.
	void parquet_read_float32_column_chunk(void *handle, const char *name, int64_t row_group, float *data, int64_t nrows, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_row_group_chunk_array(reader_handle, name, row_group, "parquet_read_float32_column_chunk");
		if (array->length() != nrows)
		{
			report_fatal_error("parquet_read_float32_column_chunk", std::string("nrows mismatch for column: ") + name);
		}
		check_or_report_nulls(array, name, valid_out, "parquet_read_float32_column_chunk");
		convert_values_to_float32(array, data, nrows, name, "parquet_read_float32_column_chunk");
		fill_null_default(data, valid_out, nrows);
	}

	// Same as parquet_read_int32_column_chunk, but for float64.
	void parquet_read_float64_column_chunk(void *handle, const char *name, int64_t row_group, double *data, int64_t nrows, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_row_group_chunk_array(reader_handle, name, row_group, "parquet_read_float64_column_chunk");
		if (array->length() != nrows)
		{
			report_fatal_error("parquet_read_float64_column_chunk", std::string("nrows mismatch for column: ") + name);
		}
		check_or_report_nulls(array, name, valid_out, "parquet_read_float64_column_chunk");
		convert_values_to_float64(array, data, nrows, name, "parquet_read_float64_column_chunk");
		fill_null_default(data, valid_out, nrows);
	}

	// Same as parquet_read_int32_column_chunk, but for boolean (bool8) columns.
	void parquet_read_bool8_column_chunk(void *handle, const char *name, int64_t row_group, int8_t *data, int64_t nrows, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_row_group_chunk_array(reader_handle, name, row_group, "parquet_read_bool8_column_chunk");
		if (array->type_id() != arrow::Type::BOOL)
		{
			report_fatal_error("parquet_read_bool8_column_chunk", std::string("type mismatch for column: ") + name +
				" (expected bool, got " + array->type()->ToString() + ")"); // GCOVR_EXCL_LINE
		}
		auto arr = std::static_pointer_cast<arrow::BooleanArray>(array);
		if (arr->length() != nrows)
		{
			report_fatal_error("parquet_read_bool8_column_chunk", std::string("nrows mismatch for column: ") + name);
		}
		check_or_report_nulls(arr, name, valid_out, "parquet_read_bool8_column_chunk");
		for (int64_t i = 0; i < nrows; ++i)
		{
			data[i] = arr->Value(i) ? 1 : 0;
		}
		fill_null_default(data, valid_out, nrows);
	}

	// Same as parquet_read_int32_column_chunk, but for string columns (fixed-width, space-padded output).
	void parquet_read_string_column_chunk(void *handle, const char *name, int64_t row_group, char *data, int64_t item_len, int64_t nrows, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_row_group_chunk_array(reader_handle, name, row_group, "parquet_read_string_column_chunk");
		if (!is_string_like_type(array->type_id()))
		{
			report_fatal_error("parquet_read_string_column_chunk", std::string("type mismatch for column: ") + name +
				" (expected string, got " + array->type()->ToString() + ")"); // GCOVR_EXCL_LINE
		}
		auto arr = make_string_like_accessor(array);
		if (arr.length != nrows)
		{
			report_fatal_error("parquet_read_string_column_chunk", std::string("nrows mismatch for column: ") + name);
		}
		check_or_report_nulls(array, name, valid_out, "parquet_read_string_column_chunk");
		for (int64_t i = 0; i < nrows; ++i)
		{
			auto view = arr.get_view(i);
			copy_string_with_padding(data + i * item_len, item_len, view);
		}
		fill_null_default_string(data, item_len, valid_out, nrows);
	}

	// Compact counterpart to parquet_read_string_column_chunk, above -- same buffer-handoff idea
	// as parquet_read_string_column_buffers, but sourced from get_row_group_chunk_array instead
	// of the whole-column cache. That array is otherwise never retained anywhere once this call
	// returns (unlike a whole-column read's, which column_cache keeps alive indefinitely), so it
	// is pinned in reader_handle->last_chunk_buffers_array for the caller (parquet_read.f90,
	// which always consumes the returned pointers immediately via append_buffers, before any
	// other call on this reader) -- see that field's own comment. No mark_read call here, same as
	// every other *_column_chunk read: was_read/output_type_used (parquet_reader_print_stat) are
	// scoped to whole-column reads only; QC for a chunked read already runs inside
	// get_row_group_chunk_array itself.
	void parquet_read_string_column_chunk_buffers(void *handle, const char *name, int64_t row_group,
		int64_t *nrows_out, int64_t *nchars_out,
		const void **offsets_out, const void **data_out, const void **validity_out,
		int8_t *offsets_int32_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_row_group_chunk_array(reader_handle, name, row_group, "parquet_read_column_chunk");
		if (!is_offset_string_type(array->type_id()))
		{
			report_fatal_error("parquet_read_column_chunk", std::string("type mismatch for column: ") + name +
				" (expected string, got " + array->type()->ToString() + // GCOVR_EXCL_LINE
				(array->type_id() == arrow::Type::STRING_VIEW ? // GCOVR_EXCL_LINE
					" -- STRING_VIEW columns are not supported by this compact buffer read; " // GCOVR_EXCL_LINE
					"use a fixed-width parquet_read_column instead" : "") + ")"); // GCOVR_EXCL_LINE
		}
		reader_handle->last_chunk_buffers_array = array;
		extract_string_buffers(array, nrows_out, nchars_out, offsets_out, data_out, validity_out, offsets_int32_out);
	}

	// Reads row group `row_group`'s full vector int32 column `name` into `data`.
	void parquet_read_int32_array_column_chunk(void *handle, const char *name, int64_t row_group, int32_t *data, int64_t nrows, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_row_group_chunk_array(reader_handle, name, row_group, "parquet_read_int32_array_column_chunk");
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, "parquet_read_int32_array_column_chunk");
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out, "parquet_read_int32_array_column_chunk");
		int64_t total = nrows * col_size;
		convert_values_to_int32(vals_any, data, total, name, "parquet_read_int32_array_column_chunk");
		fill_null_default(data, valid_out, total);
	}

	// Same as parquet_read_int32_array_column_chunk, but for int64.
	void parquet_read_int64_array_column_chunk(void *handle, const char *name, int64_t row_group, int64_t *data, int64_t nrows, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_row_group_chunk_array(reader_handle, name, row_group, "parquet_read_int64_array_column_chunk");
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, "parquet_read_int64_array_column_chunk");
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out, "parquet_read_int64_array_column_chunk");
		int64_t total = nrows * col_size;
		convert_values_to_int64(vals_any, data, total, name, "parquet_read_int64_array_column_chunk");
		fill_null_default(data, valid_out, total);
	}

	// Same as parquet_read_int32_array_column_chunk, but for float32.
	void parquet_read_float32_array_column_chunk(void *handle, const char *name, int64_t row_group, float *data, int64_t nrows, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_row_group_chunk_array(reader_handle, name, row_group, "parquet_read_float32_array_column_chunk");
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, "parquet_read_float32_array_column_chunk");
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out, "parquet_read_float32_array_column_chunk");
		int64_t total = nrows * col_size;
		convert_values_to_float32(vals_any, data, total, name, "parquet_read_float32_array_column_chunk");
		fill_null_default(data, valid_out, total);
	}

	// Same as parquet_read_int32_array_column_chunk, but for float64.
	void parquet_read_float64_array_column_chunk(void *handle, const char *name, int64_t row_group, double *data, int64_t nrows, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_row_group_chunk_array(reader_handle, name, row_group, "parquet_read_float64_array_column_chunk");
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, "parquet_read_float64_array_column_chunk");
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out, "parquet_read_float64_array_column_chunk");
		int64_t total = nrows * col_size;
		convert_values_to_float64(vals_any, data, total, name, "parquet_read_float64_array_column_chunk");
		fill_null_default(data, valid_out, total);
	}

	// Same as parquet_read_int32_array_column_chunk, but for boolean (bool8) columns.
	void parquet_read_bool8_array_column_chunk(void *handle, const char *name, int64_t row_group, int8_t *data, int64_t nrows, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_row_group_chunk_array(reader_handle, name, row_group, "parquet_read_bool8_array_column_chunk");
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, "parquet_read_bool8_array_column_chunk");
		if (vals_any->type_id() != arrow::Type::BOOL)
		{
			report_fatal_error("parquet_read_bool8_array_column_chunk", std::string("type mismatch for list values in column: ") + name +
				" (expected bool, got " + vals_any->type()->ToString() + ")"); // GCOVR_EXCL_LINE
		}
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out, "parquet_read_bool8_array_column_chunk");
		auto vals = std::static_pointer_cast<arrow::BooleanArray>(vals_any);
		for (int64_t i = 0; i < nrows * col_size; ++i)
		{
			data[i] = vals->Value(i) ? 1 : 0;
		}
		fill_null_default(data, valid_out, nrows * col_size);
	}

	// Same as parquet_read_int32_array_column_chunk, but for string columns (fixed-width, space-padded output).
	void parquet_read_string_array_column_chunk(void *handle, const char *name, int64_t row_group, char *data, int64_t item_len, int64_t nrows, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_row_group_chunk_array(reader_handle, name, row_group, "parquet_read_string_array_column_chunk");
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, "parquet_read_string_array_column_chunk");
		if (!is_string_like_type(vals_any->type_id()))
		{
			report_fatal_error("parquet_read_string_array_column_chunk", std::string("type mismatch for list values in column: ") + name +
				" (expected string, got " + vals_any->type()->ToString() + ")"); // GCOVR_EXCL_LINE
		}
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out, "parquet_read_string_array_column_chunk");
		auto vals = make_string_like_accessor(vals_any);
		for (int64_t i = 0; i < nrows * col_size; ++i)
		{
			copy_string_with_padding(data + i * item_len, item_len, vals.get_view(i));
		}
		fill_null_default_string(data, item_len, valid_out, nrows * col_size);
	}

	// Called from parquet_close_reader(check_complete=.true.), before close_parquet_reader: for
	// every column that received at least one parquet_read_column_chunk call this reader's
	// lifetime, verifies every one of the file's num_row_groups row groups was actually read for
	// that column -- catches a caller that forgot to loop through every row group (or exited a
	// chunked-read loop early by mistake). Row-mode/whole-column reads never touch
	// chunk_read_row_groups, so they're excluded, matching parquet_close_reader's own doc-comment.
	// `hard` (1/0) mirrors qc_soft: hard aborts via report_fatal_error naming the column and its
	// missing row group(s); soft prints a WARNING (to stdout, same as a soft qc violation) and
	// continues, once per incomplete column.
	void parquet_reader_check_complete(void *handle, int hard)
	{
		auto reader_handle = as_reader_handle(handle);
		for (const auto &entry : reader_handle->chunk_read_row_groups)
		{
			int idx = entry.first;
			const auto &touched = entry.second;
			if (static_cast<int64_t>(touched.size()) >= reader_handle->num_row_groups) continue;

			std::string missing;
			for (int64_t rg = 1; rg <= reader_handle->num_row_groups; ++rg)
			{
				if (touched.find(rg) == touched.end())
				{
					if (!missing.empty()) missing += ", ";
					missing += std::to_string(rg);
				}
			}
			std::string colname = reader_handle->schema->field(idx)->name();
			std::string msg = "column '" + colname + "' was read via parquet_read_column_chunk but not every row " +
				"group was read -- missing row group(s): " + missing;
			if (hard)
			{
				report_fatal_error("parquet_close_reader", msg);
			}
			std::fprintf(stdout, "WARNING: %s\n", msg.c_str());
		}
	}

	// Declares one column's schema metadata on a schema-less writer, growing fields/arrays to match.
	void parquet_add_column_metadata(
		void *handle,
		const char *name,
		const char *unit,
		const char *description,
		const char *ucd,
		const char *data_type,
		int64_t array_size,
		int64_t col_size)
	{
		auto writer_handle = as_handle(handle);
		check_column_count_fits_arrow_limit(writer_handle->column_metadata.size(), name, "parquet_add_column_metadata");
		writer_handle->column_metadata.push_back(ColumnMetadata{
			name,
			unit,
			description,
			ucd,
			data_type,
			array_size,
			col_size});

		const auto target_size = writer_handle->column_metadata.size();
		if (writer_handle->fields.size() < target_size)
		{
			writer_handle->fields.resize(target_size);
		}
		if (writer_handle->arrays.size() < target_size)
		{
			writer_handle->arrays.resize(target_size);
		}
	}

	// Adds one flat key-value table metadata entry to `handle`.
	void parquet_add_table_metadata(void *handle, const char *key, const char *value, const char *description)
	{
		auto writer_handle = as_handle(handle);
		writer_handle->table_metadata.push_back(TableMetadataEntry{
			std::string(key),
			std::string(value),
			std::string(description)});
	}

} // extern "C"

// Shared by parquet_append_{int32,int64,float32,float64,bool8}_column below:
// these five were near-identical copies differing only in Builder type and
// the raw pointer type passed to AppendValues (the same widening pattern for
// a plain column vs. a col_size > 1 fixed-size-list column). Templated here
// instead, since a fix to one of the five previously had to be manually
// replicated into the other four. Declared outside extern "C" (templates
// cannot have C language linkage) between two extern "C" blocks -- as_handle,
// append_column, build_field and has_any_null above are already visible here
// via ordinary (non-template-dependent) name lookup.
template <typename BuilderType, typename ValueType>
static void append_typed_column(void *handle, const char *name, const ValueType *data, int64_t nrows, int64_t col_size,
	const int8_t *valid_in, const std::shared_ptr<arrow::DataType> &value_type)
{
	auto writer_handle = as_handle(handle);

	std::shared_ptr<arrow::Array> array;
	auto valid_bytes = reinterpret_cast<const uint8_t *>(valid_in);

	if (col_size > 1)
	{
		check_col_size_fits_arrow_limit(col_size, name, "parquet_append_column");
		auto value_builder = std::make_shared<BuilderType>();
		arrow::FixedSizeListBuilder list_builder(arrow::default_memory_pool(), value_builder, static_cast<int32_t>(col_size));
		auto status = list_builder.AppendValues(nrows);
		if (!status.ok())
			throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE
		status = value_builder->AppendValues(data, nrows * col_size, valid_bytes);
		if (!status.ok())
			throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE
		status = list_builder.Finish(&array);
		if (!status.ok())
			throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE
	}
	else
	{
		BuilderType builder;
		auto status = builder.AppendValues(data, nrows, valid_bytes);
		if (!status.ok())
			throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE
		status = builder.Finish(&array);
		if (!status.ok())
			throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE
	}

	append_column(writer_handle, name, build_field(name, value_type, col_size, has_any_null(valid_in, nrows * col_size)), array);
}

// Shared precondition checks for every parquet_write_*_column_chunk entry point below.
// column_metadata only ever holds entries for a *schema-enforced* writer's declared columns
// (populated once, upfront, by parquet_open_writer -- see parquet_add_column_info's only
// caller, in parquet_write.f90); a schema-less writer never populates it at all, exactly
// mirroring append_column's own "not found -> push new" fallback for the batch
// (parquet_write_column) path -- so a schema-less column here is instead found (or, on its
// first-ever chunk, newly registered) by name directly in `fields`. Enforces every
// row-group-streaming invariant: a row group must be open (parquet_new_row_group), a column can
// never be written both as a whole array (parquet_write_column) and via chunks, a new column
// can never appear after the first row group's schema has already locked in (see
// close_parquet_writer/parquet_finish_row_group), and a column can only be chunk-written once
// per row group. Returns the column's index into fields/arrays; `first_chunk_ever` reports
// whether this is the column's very first chunk ever (its field still needs to be built) so
// callers don't need to re-derive that separately.
static size_t check_column_chunk_write_preconditions(ParquetWriterHandle *writer_handle, const char *name,
	bool &first_chunk_ever)
{
	if (!writer_handle->in_row_group)
	{
		report_fatal_error("parquet_write_column_chunk",
			"column '" + std::string(name) + "': no row group is open -- call parquet_new_row_group first"); // GCOVR_EXCL_LINE
	}

	int64_t metadata_index = -1;
	for (size_t i = 0; i < writer_handle->column_metadata.size(); ++i)
	{
		if (writer_handle->column_metadata[i].name == name) { metadata_index = static_cast<int64_t>(i); break; }
	}

	size_t idx;
	if (metadata_index >= 0)
	{
		idx = static_cast<size_t>(metadata_index);
		first_chunk_ever = idx >= writer_handle->fields.size() || !writer_handle->fields[idx];
	}
	else
	{
		// Schema-less: find an already-established (whole or chunk-started) column of this
		// name, if any; otherwise this is a brand-new column, appended past the end of fields
		// (mirroring append_column's own push-new fallback) rather than into column_metadata's
		// index space, which schema-less writers never use.
		idx = writer_handle->fields.size();
		for (size_t i = 0; i < writer_handle->fields.size(); ++i)
		{
			if (writer_handle->fields[i] && writer_handle->fields[i]->name() == name) { idx = i; break; }
		}
		first_chunk_ever = idx == writer_handle->fields.size();
		if (first_chunk_ever) check_column_count_fits_arrow_limit(writer_handle->fields.size(), name, "parquet_write_column_chunk");
	}

	if (idx < writer_handle->arrays.size() && writer_handle->arrays[idx])
	{
		report_fatal_error("parquet_write_column_chunk", "column '" + std::string(name) +
			"': already fully written via parquet_write_column -- cannot also write it via parquet_write_column_chunk"); // GCOVR_EXCL_LINE
	}
	if (first_chunk_ever && writer_handle->row_group_writer)
	{
		report_fatal_error("parquet_write_column_chunk", "column '" + std::string(name) +
			"': introduced after the first row group was already written -- every column must appear in the " // GCOVR_EXCL_LINE
			"first row group, since a Parquet file's schema is fixed once the first row group is written"); // GCOVR_EXCL_LINE
	}
	if (!first_chunk_ever && writer_handle->pending_chunk_arrays.count(static_cast<int>(idx)))
	{
		report_fatal_error("parquet_write_column_chunk", "column '" + std::string(name) +
			"': already written for this row group -- call parquet_finish_row_group before writing it again"); // GCOVR_EXCL_LINE
	}
	return idx;
} // GCOVR_EXCL_LINE -- closing-brace gcov attribution artifact; return idx above is itself covered

// Streaming counterpart to append_typed_column, above: builds a small array covering just
// writer_handle->current_row_group_nrows rows (this row group's slice), rather than the whole
// file's nrows. Never touches writer_handle->arrays -- doing so would mark the column "whole"
// (see close_parquet_writer's own reconciliation check) -- the built array is instead stashed in
// pending_chunk_arrays, consumed and cleared by parquet_finish_row_group.
template <typename BuilderType, typename ValueType>
static void append_typed_column_chunk(void *handle, const char *name, const ValueType *data, int64_t col_size,
	const int8_t *valid_in, const std::shared_ptr<arrow::DataType> &value_type)
{
	auto writer_handle = as_handle(handle);
	bool first_chunk_ever;
	auto idx = check_column_chunk_write_preconditions(writer_handle, name, first_chunk_ever);
	auto nrows = writer_handle->current_row_group_nrows;

	if (col_size > 1)
	{
		check_col_size_fits_arrow_limit(col_size, name, "parquet_write_column_chunk");
		check_chunk_size_fits_limit_for_col_size(nrows, name, col_size, "parquet_write_column_chunk", "nrows",
		"reduce this row group's nrows (parquet_new_row_group) or this column's col_size");
	}

	std::shared_ptr<arrow::Array> array;
	auto valid_bytes = reinterpret_cast<const uint8_t *>(valid_in);

	if (col_size > 1)
	{
		auto value_builder = std::make_shared<BuilderType>();
		arrow::FixedSizeListBuilder list_builder(arrow::default_memory_pool(), value_builder, static_cast<int32_t>(col_size));
		auto status = list_builder.AppendValues(nrows);
		if (!status.ok())
			throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE
		status = value_builder->AppendValues(data, nrows * col_size, valid_bytes);
		if (!status.ok())
			throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE
		status = list_builder.Finish(&array);
		if (!status.ok())
			throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE
	}
	else
	{
		BuilderType builder;
		auto status = builder.AppendValues(data, nrows, valid_bytes);
		if (!status.ok())
			throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE
		status = builder.Finish(&array);
		if (!status.ok())
			throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE
	}

	if (first_chunk_ever)
	{
		if (writer_handle->fields.size() <= idx) writer_handle->fields.resize(idx + 1);
		// Always nullable, unlike append_typed_column's has_any_null-based decision: this
		// field is fixed the moment the first row group locks the schema (see
		// check_column_chunk_write_preconditions), long before every row group's data -- and
		// thus every possible null -- has been seen. Fixing nullable=false from a null-free
		// first chunk would make a *later* row group's genuine null rejected by Arrow, so this
		// always allows it instead (col_size > 1's build_field ignores this argument anyway --
		// see its own comment).
		writer_handle->fields[idx] = build_field(name, value_type, col_size, /*nullable=*/true);
	}
	if (writer_handle->arrays.size() <= idx) writer_handle->arrays.resize(idx + 1);
	writer_handle->pending_chunk_arrays[static_cast<int>(idx)] = array;
}

extern "C"
{

	// Appends one int32 column's values (scalar or, for col_size > 1, fixed-size-list) to `handle`.
	void parquet_append_int32_column(void *handle, const char *name, const int32_t *data, int64_t nrows, int64_t col_size, const int8_t *valid_in)
	{
		append_typed_column<arrow::Int32Builder>(handle, name, data, nrows, col_size, valid_in, arrow::int32());
	}

	// Same as parquet_append_int32_column, but for int64.
	void parquet_append_int64_column(void *handle, const char *name, const int64_t *data, int64_t nrows, int64_t col_size, const int8_t *valid_in)
	{
		append_typed_column<arrow::Int64Builder>(handle, name, data, nrows, col_size, valid_in, arrow::int64());
	}

	// Same as parquet_append_int32_column, but for float32.
	void parquet_append_float32_column(void *handle, const char *name, const float *data, int64_t nrows, int64_t col_size, const int8_t *valid_in)
	{
		append_typed_column<arrow::FloatBuilder>(handle, name, data, nrows, col_size, valid_in, arrow::float32());
	}

	// Same as parquet_append_int32_column, but for float64.
	void parquet_append_float64_column(void *handle, const char *name, const double *data, int64_t nrows, int64_t col_size, const int8_t *valid_in)
	{
		append_typed_column<arrow::DoubleBuilder>(handle, name, data, nrows, col_size, valid_in, arrow::float64());
	}

	// Same as parquet_append_int32_column, but for boolean (bool8) columns.
	void parquet_append_bool8_column(void *handle, const char *name, const int8_t *data, int64_t nrows, int64_t col_size, const int8_t *valid_in)
	{
		append_typed_column<arrow::BooleanBuilder>(
			handle, name, reinterpret_cast<const uint8_t *>(data), nrows, col_size, valid_in, arrow::boolean());
	}

	// --- temporal whole-column appends (date/time/timestamp). See the transport note on
	//     convert_date_values/build_time_array. Date reuses the numeric append_typed_column
	//     template (Date32Builder is default-constructible and takes int32 values); time and
	//     timestamp use their dedicated array builders. ---
	void parquet_append_date_column(void *handle, const char *name, const int32_t *data, int64_t nrows, int64_t col_size, const int8_t *valid_in)
	{
		append_typed_column<arrow::Date32Builder>(handle, name, data, nrows, col_size, valid_in, arrow::date32());
	}

	void parquet_append_time_column(void *handle, const char *name, const int64_t *data, int64_t nrows, int64_t col_size,
		int32_t unit, const int8_t *valid_in)
	{
		auto writer_handle = as_handle(handle);
		auto array = build_time_array(data, nrows, col_size, unit, valid_in, name, "parquet_append_time_column");
		append_column(writer_handle, name,
			build_field(name, temporal_time_value_type(unit, "parquet_append_time_column"), col_size,
				has_any_null(valid_in, nrows * col_size)), array);
	}

	void parquet_append_timestamp_column(void *handle, const char *name, const int64_t *data, int64_t nrows, int64_t col_size,
		int32_t unit, int32_t is_utc, const int8_t *valid_in)
	{
		auto writer_handle = as_handle(handle);
		auto array = build_timestamp_array(data, nrows, col_size, unit, is_utc, valid_in, name, "parquet_append_timestamp_column");
		append_column(writer_handle, name,
			build_field(name, temporal_timestamp_value_type(unit, is_utc, "parquet_append_timestamp_column"), col_size,
				has_any_null(valid_in, nrows * col_size)), array);
	}

	// Appends one scalar string column's values to `handle`. Auto-selects arrow::utf8()
	// (int32 offsets) or, if this column's own byte payload would overflow that (see
	// would_overflow_string_offset_limit), arrow::large_utf8() (int64 offsets) instead -- every
	// read-side site that decodes a string column accepts both (see is_string_like_type).
	// The builder-type-dependent part is a local generic lambda rather than a free function
	// template so it can stay inside this extern "C" block (function templates can't have C
	// language linkage -- see compare_op's own comment, above the block, for the same reason).
	void parquet_append_string_column(void *handle, const char *name, const char *data, int64_t item_len, int64_t nrows, const int8_t *valid_in)
	{
		auto writer_handle = as_handle(handle);

		auto build = [&](auto &builder) -> std::shared_ptr<arrow::Array>
		{
			auto status = arrow::Status::OK();
			for (int64_t i = 0; i < nrows; ++i)
			{
				if (valid_in != nullptr && valid_in[i] == 0)
				{
					status = builder.AppendNull();
				}
				else
				{
					const char *raw = data + i * item_len;
					std::string value(raw, static_cast<size_t>(item_len));
					value = trim_right_spaces_and_nuls(value);
					status = builder.Append(value);
				}
				if (!status.ok())
					throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE
			}

			std::shared_ptr<arrow::Array> array;
			status = builder.Finish(&array);
			if (!status.ok())
				throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE
			return array;
		};

		int64_t limit = g_debug_string_offset_limit > 0 ? g_debug_string_offset_limit : kArrowInt32OffsetLimit;
		bool use_large = would_overflow_string_offset_limit(nrows, item_len, limit);

		std::shared_ptr<arrow::Array> array;
		if (use_large)
		{
			arrow::LargeStringBuilder builder;
			array = build(builder);
		}
		else
		{
			arrow::StringBuilder builder;
			array = build(builder);
		}

		append_column(writer_handle, name,
			build_field(name, use_large ? arrow::large_utf8() : arrow::utf8(), 1, has_any_null(valid_in, nrows)), array);
	}

	// Appends one vector string column's values to `handle`. Same arrow::utf8()/large_utf8()
	// auto-selection as parquet_append_string_column, above -- see its own comment.
	void parquet_append_string_array_column(void *handle, const char *name, const char *data, int64_t item_len, int64_t nrows, int64_t col_size, const int8_t *valid_in)
	{
		auto writer_handle = as_handle(handle);
		check_col_size_fits_arrow_limit(col_size, name, "parquet_append_string_array_column");

		auto build = [&](auto value_builder) -> std::shared_ptr<arrow::Array>
		{
			arrow::FixedSizeListBuilder list_builder(arrow::default_memory_pool(), value_builder, static_cast<int32_t>(col_size));

			auto status = list_builder.AppendValues(nrows);
			if (!status.ok())
				throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE

			for (int64_t i = 0; i < nrows * col_size; ++i)
			{
				if (valid_in != nullptr && valid_in[i] == 0)
				{
					status = value_builder->AppendNull();
				}
				else
				{
					const char *raw = data + i * item_len;
					std::string value(raw, static_cast<size_t>(item_len));
					value = trim_right_spaces_and_nuls(value);
					status = value_builder->Append(value);
				}
				if (!status.ok())
					throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE
			}

			std::shared_ptr<arrow::Array> array;
			status = list_builder.Finish(&array);
			if (!status.ok())
				throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE
			return array;
		};

		int64_t limit = g_debug_string_offset_limit > 0 ? g_debug_string_offset_limit : kArrowInt32OffsetLimit;
		bool use_large = would_overflow_string_offset_limit(nrows * col_size, item_len, limit);

		std::shared_ptr<arrow::Array> array = use_large
			? build(std::make_shared<arrow::LargeStringBuilder>())
			: build(std::make_shared<arrow::StringBuilder>());

		append_column(writer_handle, name, build_field(name, use_large ? arrow::large_utf8() : arrow::utf8(), col_size), array);
	}

	// Appends one scalar string column straight from a parquet_string_column's own raw buffers
	// (offsets/data/validity, from parquet_strings.f90's raw_buffers) -- the compact counterpart
	// to parquet_append_string_column above, skipping both the fixed-width padded intermediate
	// and its trim_right_spaces_and_nuls step entirely (the source is already exact, unpadded
	// string content -- one copy total, the builder's own, instead of pad-then-trim-then-copy).
	// `offsets` is nrows+1 int64 values (0-based, offsets[0]=0); `validity` is a bit-packed
	// Arrow-style bitmap (LSB-first, 1=valid) or nullptr when the column has no nulls -- exactly
	// parquet_string_column's own internal layout, so no conversion is needed on either side of
	// this call. Always builds arrow::large_utf8(), matching the source column's own int64-offset
	// storage -- unlike parquet_append_string_column's small/large auto-selection, there is no
	// int32-offset variant to consider here at all.
	void parquet_append_string_column_buffers(void *handle, const char *name, int64_t nrows, int64_t nchars,
		const int64_t *offsets, const char *data, const uint8_t *validity)
	{
		auto writer_handle = as_handle(handle);
		(void)nchars; // implied by offsets[nrows]; kept as an argument for a self-describing C signature.

		arrow::LargeStringBuilder builder;
		arrow::Status status;
		bool any_null = false;
		for (int64_t i = 0; i < nrows; ++i)
		{
			if (validity != nullptr && !arrow::bit_util::GetBit(validity, static_cast<uint64_t>(i)))
			{
				any_null = true;
				status = builder.AppendNull();
			}
			else
			{
				status = builder.Append(data + offsets[i], offsets[i + 1] - offsets[i]);
			}
			if (!status.ok())
				throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE
		}

		std::shared_ptr<arrow::Array> array;
		status = builder.Finish(&array);
		if (!status.ok())
			throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE

		append_column(writer_handle, name, build_field(name, arrow::large_utf8(), 1, any_null), array);
	}

	// --- Streaming row-group API: parquet_new_row_group / parquet_write_*_column_chunk /
	// parquet_finish_row_group. See close_parquet_writer for how a streaming writer's close
	// differs from the WriteTable-based batch path above, and check_column_chunk_write_
	// preconditions/append_typed_column_chunk (further above, outside this extern "C" block)
	// for the shared validation/array-building logic every parquet_write_*_column_chunk
	// function below is built on. ---

	void parquet_new_row_group(void *handle, int64_t nrows)
	{
		auto writer_handle = as_handle(handle);
		if (writer_handle->in_row_group)
		{
			report_fatal_error("parquet_new_row_group",
				"a row group is already open -- call parquet_finish_row_group before starting another"); // GCOVR_EXCL_LINE
		}
		if (nrows <= 0)
		{
			report_fatal_error("parquet_new_row_group", "nrows (" + std::to_string(nrows) + ") must be positive");
		}
		// Validates against every vector column already known (schema-enforced, or established
		// by an earlier row group for a schema-less writer). A brand-new schema-less column's
		// own col_size isn't known yet here -- that's validated later instead, at its first
		// parquet_write_column_chunk call.
		check_chunk_size_fits_metadata_limit(nrows, writer_handle->column_metadata, "parquet_new_row_group", "nrows",
			"pass a smaller nrows to parquet_new_row_group");

		writer_handle->in_row_group = true;
		writer_handle->current_row_group_nrows = nrows;
		writer_handle->pending_chunk_arrays.clear();
	}

	void parquet_write_int32_column_chunk(void *handle, const char *name, const int32_t *data, int64_t col_size, const int8_t *valid_in)
	{
		append_typed_column_chunk<arrow::Int32Builder>(handle, name, data, col_size, valid_in, arrow::int32());
	}

	void parquet_write_int64_column_chunk(void *handle, const char *name, const int64_t *data, int64_t col_size, const int8_t *valid_in)
	{
		append_typed_column_chunk<arrow::Int64Builder>(handle, name, data, col_size, valid_in, arrow::int64());
	}

	void parquet_write_float32_column_chunk(void *handle, const char *name, const float *data, int64_t col_size, const int8_t *valid_in)
	{
		append_typed_column_chunk<arrow::FloatBuilder>(handle, name, data, col_size, valid_in, arrow::float32());
	}

	void parquet_write_float64_column_chunk(void *handle, const char *name, const double *data, int64_t col_size, const int8_t *valid_in)
	{
		append_typed_column_chunk<arrow::DoubleBuilder>(handle, name, data, col_size, valid_in, arrow::float64());
	}

	void parquet_write_bool8_column_chunk(void *handle, const char *name, const int8_t *data, int64_t col_size, const int8_t *valid_in)
	{
		append_typed_column_chunk<arrow::BooleanBuilder>(
			handle, name, reinterpret_cast<const uint8_t *>(data), col_size, valid_in, arrow::boolean());
	}

	// --- temporal streaming (row-group-chunked) appends (date/time/timestamp). Date reuses the
	//     numeric append_typed_column_chunk template; time and timestamp build this row group's
	//     array (nrows = current_row_group_nrows) and stash it via stash_temporal_column_chunk. ---
	void parquet_write_date_column_chunk(void *handle, const char *name, const int32_t *data, int64_t col_size, const int8_t *valid_in)
	{
		append_typed_column_chunk<arrow::Date32Builder>(handle, name, data, col_size, valid_in, arrow::date32());
	}

	void parquet_write_time_column_chunk(void *handle, const char *name, const int64_t *data, int64_t col_size, int32_t unit, const int8_t *valid_in)
	{
		auto writer_handle = as_handle(handle);
		bool first_chunk_ever;
		auto idx = check_column_chunk_write_preconditions(writer_handle, name, first_chunk_ever);
		auto nrows = writer_handle->current_row_group_nrows;
		if (col_size > 1)
		{
			check_chunk_size_fits_limit_for_col_size(nrows, name, col_size, "parquet_write_column_chunk", "nrows",
				"reduce this row group's nrows (parquet_new_row_group) or this column's col_size");
		}
		auto array = build_time_array(data, nrows, col_size, unit, valid_in, name, "parquet_write_time_column_chunk");
		stash_temporal_column_chunk(writer_handle, name, idx, first_chunk_ever, array,
			temporal_time_value_type(unit, "parquet_write_time_column_chunk"), col_size);
	}

	void parquet_write_timestamp_column_chunk(void *handle, const char *name, const int64_t *data, int64_t col_size,
		int32_t unit, int32_t is_utc, const int8_t *valid_in)
	{
		auto writer_handle = as_handle(handle);
		bool first_chunk_ever;
		auto idx = check_column_chunk_write_preconditions(writer_handle, name, first_chunk_ever);
		auto nrows = writer_handle->current_row_group_nrows;
		if (col_size > 1)
		{
			check_chunk_size_fits_limit_for_col_size(nrows, name, col_size, "parquet_write_column_chunk", "nrows",
				"reduce this row group's nrows (parquet_new_row_group) or this column's col_size");
		}
		auto array = build_timestamp_array(data, nrows, col_size, unit, is_utc, valid_in, name, "parquet_write_timestamp_column_chunk");
		stash_temporal_column_chunk(writer_handle, name, idx, first_chunk_ever, array,
			temporal_timestamp_value_type(unit, is_utc, "parquet_write_timestamp_column_chunk"), col_size);
	}

	// Streaming counterpart to parquet_append_string_column: unlike that whole-column write,
	// which auto-selects arrow::utf8()/arrow::large_utf8() from the *whole* column's byte
	// payload, a streamed column's field is fixed the moment the first row group locks the
	// schema -- long before every row group's string bytes have been seen. Always uses
	// arrow::large_utf8() instead, unconditionally, so no later row group's cumulative bytes can
	// ever exceed what the already-fixed type supports (the cost is a slightly larger offset
	// buffer even for a small file, imperceptible in practice and irrelevant for the large files
	// this API exists for).
	void parquet_write_string_column_chunk(void *handle, const char *name, const char *data, int64_t item_len,
		const int8_t *valid_in)
	{
		auto writer_handle = as_handle(handle);
		bool first_chunk_ever;
		auto idx = check_column_chunk_write_preconditions(writer_handle, name, first_chunk_ever);
		auto nrows = writer_handle->current_row_group_nrows;

		arrow::LargeStringBuilder builder;
		arrow::Status status;
		for (int64_t i = 0; i < nrows; ++i)
		{
			if (valid_in != nullptr && valid_in[i] == 0)
			{
				status = builder.AppendNull();
			}
			else
			{
				const char *raw = data + i * item_len;
				std::string value(raw, static_cast<size_t>(item_len));
				value = trim_right_spaces_and_nuls(value);
				status = builder.Append(value);
			}
			if (!status.ok())
				throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE
		}
		std::shared_ptr<arrow::Array> array;
		status = builder.Finish(&array);
		if (!status.ok())
			throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE

		if (first_chunk_ever)
		{
			if (writer_handle->fields.size() <= idx) writer_handle->fields.resize(idx + 1);
			writer_handle->fields[idx] = build_field(name, arrow::large_utf8(), 1, /*nullable=*/true);
		}
		if (writer_handle->arrays.size() <= idx) writer_handle->arrays.resize(idx + 1);
		writer_handle->pending_chunk_arrays[static_cast<int>(idx)] = array;
	}

	// Vector-string counterpart to parquet_write_string_column_chunk, above -- same
	// always-arrow::large_utf8() reasoning.
	void parquet_write_string_array_column_chunk(void *handle, const char *name, const char *data, int64_t item_len,
		int64_t col_size, const int8_t *valid_in)
	{
		auto writer_handle = as_handle(handle);
		bool first_chunk_ever;
		auto idx = check_column_chunk_write_preconditions(writer_handle, name, first_chunk_ever);
		auto nrows = writer_handle->current_row_group_nrows;

		check_col_size_fits_arrow_limit(col_size, name, "parquet_write_column_chunk");
		check_chunk_size_fits_limit_for_col_size(nrows, name, col_size, "parquet_write_column_chunk", "nrows",
		"reduce this row group's nrows (parquet_new_row_group) or this column's col_size");

		auto value_builder = std::make_shared<arrow::LargeStringBuilder>();
		arrow::FixedSizeListBuilder list_builder(arrow::default_memory_pool(), value_builder, static_cast<int32_t>(col_size));
		auto status = list_builder.AppendValues(nrows);
		if (!status.ok())
			throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE

		for (int64_t i = 0; i < nrows * col_size; ++i)
		{
			if (valid_in != nullptr && valid_in[i] == 0)
			{
				status = value_builder->AppendNull();
			}
			else
			{
				const char *raw = data + i * item_len;
				std::string value(raw, static_cast<size_t>(item_len));
				value = trim_right_spaces_and_nuls(value);
				status = value_builder->Append(value);
			}
			if (!status.ok())
				throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE
		}

		std::shared_ptr<arrow::Array> array;
		status = list_builder.Finish(&array);
		if (!status.ok())
			throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE

		if (first_chunk_ever)
		{
			if (writer_handle->fields.size() <= idx) writer_handle->fields.resize(idx + 1);
			writer_handle->fields[idx] = build_field(name, arrow::large_utf8(), col_size);
		}
		if (writer_handle->arrays.size() <= idx) writer_handle->arrays.resize(idx + 1);
		writer_handle->pending_chunk_arrays[static_cast<int>(idx)] = array;
	}

	// Streaming counterpart to parquet_append_string_column_buffers: builds this row group's
	// array straight from a parquet_string_column's own raw buffers, same buffer contract as
	// parquet_append_string_column_buffers above. Always arrow::large_utf8() and always
	// nullable=true on first_chunk_ever, same reasoning as parquet_write_string_column_chunk's
	// own comment (a later row group's genuine null must not be rejected by an already-locked
	// non-nullable field).
	void parquet_write_string_column_chunk_buffers(void *handle, const char *name, int64_t nrows, int64_t nchars,
		const int64_t *offsets, const char *data, const uint8_t *validity)
	{
		auto writer_handle = as_handle(handle);
		(void)nchars; // implied by offsets[nrows]; kept as an argument for a self-describing C signature.
		bool first_chunk_ever;
		auto idx = check_column_chunk_write_preconditions(writer_handle, name, first_chunk_ever);

		arrow::LargeStringBuilder builder;
		arrow::Status status;
		for (int64_t i = 0; i < nrows; ++i)
		{
			if (validity != nullptr && !arrow::bit_util::GetBit(validity, static_cast<uint64_t>(i)))
			{
				status = builder.AppendNull();
			}
			else
			{
				status = builder.Append(data + offsets[i], offsets[i + 1] - offsets[i]);
			}
			if (!status.ok())
				throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE
		}
		std::shared_ptr<arrow::Array> array;
		status = builder.Finish(&array);
		if (!status.ok())
			throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE

		if (first_chunk_ever)
		{
			if (writer_handle->fields.size() <= idx) writer_handle->fields.resize(idx + 1);
			writer_handle->fields[idx] = build_field(name, arrow::large_utf8(), 1, /*nullable=*/true);
		}
		if (writer_handle->arrays.size() <= idx) writer_handle->arrays.resize(idx + 1);
		writer_handle->pending_chunk_arrays[static_cast<int>(idx)] = array;
	}

	// Ends the currently-open row group: verifies every column known so far has data for it
	// (either a slice of a whole parquet_write_column array, or a pending chunk -- see
	// check_column_chunk_write_preconditions), lazily opens the underlying row-group-oriented
	// FileWriter on the very first call (locking the file's schema from every column
	// established by then -- see the comment on ParquetWriterHandle::row_group_writer), then
	// writes one column chunk per column, in schema order, as parquet::arrow::FileWriter::
	// WriteColumnChunk requires.
	void parquet_finish_row_group(void *handle)
	{
		auto writer_handle = as_handle(handle);
		if (!writer_handle->in_row_group)
		{
			report_fatal_error("parquet_finish_row_group", "no row group is open -- call parquet_new_row_group first");
		}

		auto nrows = writer_handle->current_row_group_nrows;
		bool opening_first_row_group = !writer_handle->row_group_writer;

		if (opening_first_row_group && !writer_handle->column_metadata.empty())
		{
			// Schema-enforced writer: every declared column must actually appear in the first
			// row group -- mirrors close_parquet_writer's "Missing column data before close"
			// check for the batch (WriteTable) path, just fired here instead so a forgotten
			// column is caught immediately rather than only once the whole file has already
			// been streamed. A no-op for a schema-less writer: there, column_metadata only ever
			// gains an entry together with its field, in the same call (see
			// parquet_add_column_info), so this condition can never actually trigger for one.
			for (size_t i = 0; i < writer_handle->column_metadata.size(); ++i)
			{
				if (i >= writer_handle->fields.size() || !writer_handle->fields[i])
				{
					report_fatal_error("parquet_finish_row_group", "column '" +
						writer_handle->column_metadata[i].name + "' has no data in the first row group"); // GCOVR_EXCL_LINE
				}
			}
		}

		for (size_t i = 0; i < writer_handle->fields.size(); ++i)
		{
			if (!writer_handle->fields[i]) continue; // Not yet established by anything -- only possible
			                                          // before the first row group's schema locks in
			                                          // (checked above, for schema-enforced writers).
			bool is_whole = i < writer_handle->arrays.size() && writer_handle->arrays[i] != nullptr;
			bool has_pending = writer_handle->pending_chunk_arrays.count(static_cast<int>(i)) > 0;
			if (!is_whole && !has_pending)
			{
				report_fatal_error("parquet_finish_row_group",
					"column '" + writer_handle->fields[i]->name() + "' has no data for this row group"); // GCOVR_EXCL_LINE
			}
			if (is_whole && writer_handle->streamed_rows_total + nrows > writer_handle->arrays[i]->length())
			{
				report_fatal_error("parquet_finish_row_group", "column '" + writer_handle->fields[i]->name() +
					"' has " + std::to_string(writer_handle->arrays[i]->length()) + // GCOVR_EXCL_LINE
					" rows (written via parquet_write_column), but row groups have already covered " + // GCOVR_EXCL_LINE
					std::to_string(writer_handle->streamed_rows_total) + " of them and this row group would add " + // GCOVR_EXCL_LINE
					std::to_string(nrows) + " more, exceeding the column's own row count"); // GCOVR_EXCL_LINE
			}
		}

		if (opening_first_row_group)
		{
			if (writer_handle->fields.empty())
			{
				report_fatal_error("parquet_finish_row_group", "no columns have been written -- nothing to write");
			}
			auto metadata = build_file_metadata(writer_handle->column_metadata, writer_handle->table_metadata);
			auto schema = arrow::schema(writer_handle->fields, metadata);

			parquet::ArrowWriterProperties::Builder arrow_writer_builder;
			arrow_writer_builder.store_schema();
			arrow_writer_builder.set_use_threads(writer_handle->use_threads);
			auto arrow_writer_properties = arrow_writer_builder.build();
			auto writer_properties = parquet::WriterProperties::Builder()
				.compression(writer_handle->compression_codec)
				->compression_level(writer_handle->compression_level)
				->build();

			auto result = parquet::arrow::FileWriter::Open(*schema, arrow::default_memory_pool(),
				writer_handle->outfile, writer_properties, arrow_writer_properties);
			if (!result.ok())
				throw std::runtime_error(result.status().ToString()); // GCOVR_EXCL_LINE
			writer_handle->row_group_writer = std::move(result).ValueOrDie();
		}

		auto status = writer_handle->row_group_writer->NewRowGroup();
		if (!status.ok())
			throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE

		for (size_t i = 0; i < writer_handle->fields.size(); ++i)
		{
			std::shared_ptr<arrow::Array> array;
			auto pending_it = writer_handle->pending_chunk_arrays.find(static_cast<int>(i));
			if (pending_it != writer_handle->pending_chunk_arrays.end())
			{
				array = pending_it->second;
			}
			else
			{
				array = writer_handle->arrays[i]->Slice(writer_handle->streamed_rows_total, nrows);
			}
			status = writer_handle->row_group_writer->WriteColumnChunk(*array);
			if (!status.ok())
				throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE
		}

		writer_handle->streamed_rows_total += nrows;
		writer_handle->pending_chunk_arrays.clear();
		writer_handle->in_row_group = false;
		writer_handle->current_row_group_nrows = 0;
	}

	// Returns the writer's resolved/authoritative row-group size -- see resolve_chunk_size.
	// Usable at any point after parquet_open_writer, including before any column has been
	// written.
	int64_t parquet_writer_get_chunk_size(void *handle)
	{
		auto writer_handle = as_handle(handle);
		return resolve_chunk_size(writer_handle);
	}

	// Test-only: overrides g_debug_string_offset_limit (see its own comment for why this is a
	// process-global) so test/error_scenarios.f90's scenario_large_utf8_roundtrip can exercise
	// the arrow::large_utf8() write/read path with a tiny fixture instead of needing genuine
	// multi-gigabyte string data. Safe as a process-global specifically because that scenario
	// runs as its own isolated subprocess (see tools/run_error_scenarios.sh's pattern, already
	// used this way elsewhere), so it can never race with a concurrently-running test-drive
	// test's own string columns. Not part of the public Fortran API: reachable only via a
	// bind(C) interface declared directly in test/error_scenarios.f90, never
	// src/parquet_bindings.f90. Pass n<=0 to restore the real production limit.
	void parquet_debug_set_string_offset_limit(int64_t n)
	{
		g_debug_string_offset_limit = n;
	}

	// Test-only: overrides g_debug_col_size_limit (see its own comment) so
	// test/error_scenarios.f90's scenario_col_size_overflow can exercise the
	// check_col_size_fits_arrow_limit abort path with a tiny fixture instead of a genuinely
	// oversized vector column. Same process-global/subprocess-isolation reasoning as
	// parquet_debug_set_string_offset_limit, above. Pass n<=0 to restore the real production limit.
	// GCOVR_EXCL'd: scenario_col_size_overflow always ends by aborting via
	// check_col_size_fits_arrow_limit's report_fatal_error, which discards the whole process's
	// gcov data -- so this setter, though genuinely called every time, never shows as covered
	// either. Collateral of the same std::abort()-discards-coverage mechanism, not a separate gap.
	void parquet_debug_set_col_size_limit(int64_t n) // GCOVR_EXCL_START
	{
		g_debug_col_size_limit = n;
	}
	// GCOVR_EXCL_STOP

	// Test-only: overrides g_debug_list_element_count_limit (see its own comment) so
	// test/error_scenarios.f90's scenario_list_element_count_overflow can exercise the
	// check_list_element_count_fits_arrow_limit abort path with a tiny fixture instead of a
	// genuinely huge (nrows * col_size > 2^31-1) vector column. Same process-global/
	// subprocess-isolation reasoning as parquet_debug_set_string_offset_limit, above. Pass n<=0
	// to restore the real production limit.
	void parquet_debug_set_list_element_count_limit(int64_t n)
	{
		g_debug_list_element_count_limit = n;
	}

	// Test-only: overrides g_debug_column_count_limit (see its own comment) so
	// test/error_scenarios.f90's scenario_column_count_overflow can exercise the
	// check_column_count_fits_arrow_limit abort path with a tiny fixture instead of a genuinely
	// huge number of columns. Same process-global/subprocess-isolation reasoning as
	// parquet_debug_set_string_offset_limit, above. Pass n<=0 to restore the real production limit.
	// GCOVR_EXCL'd: same reasoning as parquet_debug_set_col_size_limit above -- scenario_column_
	// count_overflow always ends by aborting via check_column_count_fits_arrow_limit's
	// report_fatal_error, which discards the whole process's gcov data.
	void parquet_debug_set_column_count_limit(int64_t n) // GCOVR_EXCL_START
	{
		g_debug_column_count_limit = n;
	}
	// GCOVR_EXCL_STOP

	// Test-only: overrides g_debug_force_whole_column_read_error (see its own comment) so
	// test/error_scenarios.f90's scenario_col_size_and_row_mode_avoid_whole_column_read can prove
	// parquet_get_col_size/parquet_get_column_total_elements/parquet_read_array_row_mode never
	// take get_single_chunk_array's whole-column-read path for a FIXED_SIZE_LIST column, on a
	// tiny fixture -- without needing a genuinely oversized (nrows * col_size > 2^31-1) column.
	// Same process-global/subprocess-isolation reasoning as parquet_debug_set_string_offset_limit,
	// above. Pass 0 to restore normal (non-forced-error) behavior.
	void parquet_debug_set_force_whole_column_read_error(int enable)
	{
		g_debug_force_whole_column_read_error = (enable != 0);
	}

	// Test-only: returns g_debug_physical_column_read_count (see its own comment) -- lets
	// test/error_scenarios.f90's scenario_nested_struct_shares_cached_read prove that reading two
	// different leaf paths under the same top-level struct column only triggers one genuine disk
	// read of that struct, i.e. that struct-path resolution shares get_single_chunk_array's
	// existing column_cache rather than re-reading per leaf path.
	int64_t parquet_debug_get_physical_column_read_count()
	{
		return g_debug_physical_column_read_count;
	}

	// Test-only: resets g_debug_physical_column_read_count to 0, so a scenario can zero the
	// counter right before the specific reads it wants to measure.
	void parquet_debug_reset_physical_column_read_count()
	{
		g_debug_physical_column_read_count = 0;
	}

	// Test-only: writes a tiny fixture file with one arrow::utf8_view() column named `name`,
	// bypassing this library's own writer entirely -- unlike LARGE_STRING (reachable through
	// parquet_append_string_column once would_overflow_string_offset_limit trips), this library
	// never writes STRING_VIEW itself, so exercising the read side (is_string_like_type/
	// make_string_like_accessor's STRING_VIEW branches) needs a file built directly with Arrow's
	// own StringViewBuilder + parquet::arrow::WriteTable(..., store_schema()) -- the same
	// stored-Arrow-schema mechanism that lets a file written by another Arrow-based tool round-
	// trip its column as STRING_VIEW on read, per is_string_like_type's own comment. Five fixed
	// rows deliberately cover StringView's inlined-vs-out-of-line boundary (values <= 12 bytes
	// are stored inline in the view itself; longer ones spill to an out-of-line data buffer):
	// a short inlined value, an empty inlined value, a Null, a long out-of-line value, and a
	// value exactly at the 12-byte inline boundary. Reachable only via a bind(C) interface
	// declared locally in test/error_scenarios.f90, never src/parquet_bindings.f90 -- same
	// convention as every other parquet_debug_* hook in this file.
	// Test-only: converts days-since-1970-01-01 to a civil (year, month, day) using Arrow's
	// vendored copy of Howard Hinnant's date library -- the reference implementation
	// src/parquet_temporal.f90's own pure-Fortran civil_from_days is cross-validated against
	// by test/test_temporal.f90's "Arrow cross-validation" test. The vendored library's `year`
	// is a 16-bit type, so callers must stay within years +-32767 (the test does). Reachable
	// only via a bind(C) interface declared locally in the test file, never
	// src/parquet_bindings.f90 -- same convention as every other parquet_debug_* hook here.
	void parquet_debug_civil_from_days(int64_t days_since_epoch, int32_t *year, int32_t *month, int32_t *day)
	{
		using namespace arrow_vendored::date;
		const year_month_day ymd{sys_days{days{static_cast<int>(days_since_epoch)}}};
		*year = static_cast<int32_t>(static_cast<int>(ymd.year()));
		*month = static_cast<int32_t>(static_cast<unsigned>(ymd.month()));
		*day = static_cast<int32_t>(static_cast<unsigned>(ymd.day()));
	}

	// Test-only: the inverse of parquet_debug_civil_from_days -- civil (year, month, day) to
	// days-since-1970-01-01 via Arrow's vendored date library, cross-validating
	// src/parquet_temporal.f90's days_from_civil. Same +-32767 year bound and same
	// locally-declared-bind(C)-only convention as above.
	void parquet_debug_days_from_civil(int32_t year_in, int32_t month_in, int32_t day_in,
		int64_t *days_since_epoch)
	{
		using namespace arrow_vendored::date;
		const sys_days sd{year{year_in}/month{static_cast<unsigned>(month_in)}/day{static_cast<unsigned>(day_in)}};
		*days_since_epoch = static_cast<int64_t>(sd.time_since_epoch().count());
	}

	void parquet_debug_write_string_view_fixture(const char *path, const char *column_name)
	{
		arrow::StringViewBuilder builder;
		auto check = [](const arrow::Status &st)
		{
			if (!st.ok()) throw std::runtime_error("parquet_debug_write_string_view_fixture: " + st.ToString());
		};
		check(builder.Append("short"));
		check(builder.Append(""));
		check(builder.AppendNull());
		check(builder.Append("this value exceeds twelve bytes for sure"));
		check(builder.Append("exactly12chr"));

		std::shared_ptr<arrow::Array> array;
		check(builder.Finish(&array));

		auto field = arrow::field(column_name, arrow::utf8_view());
		auto schema = arrow::schema({field});
		auto table = arrow::Table::Make(schema, {array});

		// file-I/O backstop, not fixture-triggerable.
		auto outfile_result = arrow::io::FileOutputStream::Open(path);
		if (!outfile_result.ok())
			throw std::runtime_error("parquet_debug_write_string_view_fixture: failed to open '" + // GCOVR_EXCL_LINE
				std::string(path) + "': " + outfile_result.status().ToString()); // GCOVR_EXCL_LINE
		auto outfile = outfile_result.ValueOrDie();

		parquet::ArrowWriterProperties::Builder arrow_writer_builder;
		arrow_writer_builder.store_schema();
		auto arrow_writer_properties = arrow_writer_builder.build();
		auto writer_properties = parquet::WriterProperties::Builder().build();

		auto status = parquet::arrow::WriteTable(*table, arrow::default_memory_pool(), outfile,
			table->num_rows(), writer_properties, arrow_writer_properties);
		check(status);
		check(outfile->Close());
	}

	// Test-only: writes a tiny fixture file with one temporal column of a physical
	// representation this library's own writer never produces, bypassing the writer entirely --
	// same convention as parquet_debug_write_string_view_fixture above. `variant`:
	//   "int96"    -- a legacy INT96-encoded timestamp column (nanosecond precision; the
	//                 pre-Parquet-2.0 encoding old Impala/Spark files use). Reachable only via
	//                 ArrowWriterProperties::enable_deprecated_int96_timestamps(), which this
	//                 library's own writer never sets.
	//   "tz"       -- a timestamp[us] column with a real, non-UTC IANA timezone string
	//                 ("America/New_York"), confirming an arbitrary tz (not just UTC/naive, the
	//                 only two this library's own writer produces) round-trips and is reported
	//                 correctly by parquet_get_column_time_info.
	// Five rows each, with one Null, mirroring the row count/null pattern this library's own
	// fixtures use elsewhere in this file.
	void parquet_debug_write_datetime_fixture(const char *path, const char *column_name, const char *variant)
	{
		std::string v(variant);
		auto check = [](const arrow::Status &st)
		{
			if (!st.ok()) throw std::runtime_error("parquet_debug_write_datetime_fixture: " + st.ToString());
		};

		std::shared_ptr<arrow::DataType> value_type;
		std::shared_ptr<arrow::Array> array;
		parquet::ArrowWriterProperties::Builder arrow_writer_builder;
		arrow_writer_builder.store_schema();

		// Five instants a few years apart, in nanoseconds since the epoch (2021-03-14T09:26:53
		// plus fractional seconds, then +1 year steps), row 3 is Null.
		const int64_t ns_values[5] = {
			1615714013123456789LL, 1647250013123456789LL, 0LL, 1710408413123456789LL, 1741944413123456789LL};

		if (v == "int96")
		{
			value_type = arrow::timestamp(arrow::TimeUnit::NANO);
			arrow::TimestampBuilder builder(value_type, arrow::default_memory_pool());
			for (int i = 0; i < 5; ++i)
			{
				if (i == 2) check(builder.AppendNull());
				else check(builder.Append(ns_values[i]));
			}
			check(builder.Finish(&array));
			arrow_writer_builder.enable_deprecated_int96_timestamps();
		}
		else if (v == "tz")
		{
			value_type = arrow::timestamp(arrow::TimeUnit::MICRO, "America/New_York");
			arrow::TimestampBuilder builder(value_type, arrow::default_memory_pool());
			for (int i = 0; i < 5; ++i)
			{
				if (i == 2) check(builder.AppendNull());
				else check(builder.Append(ns_values[i]/1000LL));
			}
			check(builder.Finish(&array));
		}
		else
		{
			throw std::runtime_error("parquet_debug_write_datetime_fixture: unknown variant: " + v); // GCOVR_EXCL_LINE
		}

		auto field = arrow::field(column_name, value_type, /*nullable=*/true);
		auto schema = arrow::schema({field});
		auto table = arrow::Table::Make(schema, {array});

		// file-I/O backstop, not fixture-triggerable.
		auto outfile_result = arrow::io::FileOutputStream::Open(path);
		if (!outfile_result.ok())
			throw std::runtime_error("parquet_debug_write_datetime_fixture: failed to open '" + // GCOVR_EXCL_LINE
				std::string(path) + "': " + outfile_result.status().ToString()); // GCOVR_EXCL_LINE
		auto outfile = outfile_result.ValueOrDie();

		auto arrow_writer_properties = arrow_writer_builder.build();
		auto writer_properties = parquet::WriterProperties::Builder().build();
		auto status = parquet::arrow::WriteTable(*table, arrow::default_memory_pool(), outfile,
			table->num_rows(), writer_properties, arrow_writer_properties);
		check(status);
		check(outfile->Close());
	}

	// Test-only: writes a tiny fixture file with plain (variable-width) arrow::list()/
	// arrow::large_list() columns, bypassing the writer entirely -- same convention as
	// parquet_debug_write_string_view_fixture/parquet_debug_write_datetime_fixture above. This
	// library's own writer only ever emits FIXED_SIZE_LIST for vector columns (see
	// append_column), so LIST/LARGE_LIST only exist in foreign-written files. `variant`:
	//   "mismatch" -- "lst" (LIST<int32>) and "large_lst" (LARGE_LIST<int32>), 3 rows each with a
	//                 different element count per row ([], [1,2], [3,4,5]) -- exercises
	//                 get_col_size's heterogeneous-row-width return-1 branch for both list kinds.
	//   "empty"    -- "lst" and "large_lst" again, but 0 rows -- exercises get_col_size's
	//                 whole-array-empty return-0 branch for both list kinds (distinct from an
	//                 empty *row*, i.e. a row whose own list has zero elements, which the
	//                 "mismatch" variant's first row already covers as an ordinary row width).
	//   "strings"  -- "lst_str" (LIST<utf8>) and "large_lst_str" (LARGE_LIST<utf8>), 3 uniform-width
	//                 rows with one Null element -- exercises parquet_reader_get_string_length's
	//                 LIST/LARGE_LIST branches and flatten_for_stats's LIST branch (LARGE_LIST is
	//                 returned unflattened by flatten_for_stats -- see its own comment).
	void parquet_debug_write_list_fixture(const char *path, const char *variant)
	{
		std::string v(variant);
		auto check = [](const arrow::Status &st)
		{
			if (!st.ok()) throw std::runtime_error("parquet_debug_write_list_fixture: " + st.ToString());
		};

		std::vector<std::shared_ptr<arrow::Field>> fields;
		std::vector<std::shared_ptr<arrow::Array>> arrays;

		if (v == "mismatch" || v == "empty")
		{
			auto int_values = std::make_shared<arrow::Int32Builder>();
			arrow::ListBuilder lst_builder(arrow::default_memory_pool(), int_values);
			auto large_int_values = std::make_shared<arrow::Int32Builder>();
			arrow::LargeListBuilder large_lst_builder(arrow::default_memory_pool(), large_int_values);

			if (v == "mismatch")
			{
				const std::vector<std::vector<int32_t>> rows = {{}, {1, 2}, {3, 4, 5}};
				for (const auto &row : rows)
				{
					check(lst_builder.Append());
					check(large_lst_builder.Append());
					for (auto val : row)
					{
						check(int_values->Append(val));
						check(large_int_values->Append(val));
					}
				}
			}

			std::shared_ptr<arrow::Array> lst_array, large_lst_array;
			check(lst_builder.Finish(&lst_array));
			check(large_lst_builder.Finish(&large_lst_array));

			fields.push_back(arrow::field("lst", arrow::list(arrow::int32()), /*nullable=*/true));
			fields.push_back(arrow::field("large_lst", arrow::large_list(arrow::int32()), /*nullable=*/true));
			arrays.push_back(lst_array);
			arrays.push_back(large_lst_array);
		}
		else if (v == "strings")
		{
			auto str_values = std::make_shared<arrow::StringBuilder>();
			arrow::ListBuilder lst_builder(arrow::default_memory_pool(), str_values);
			auto large_str_values = std::make_shared<arrow::StringBuilder>();
			arrow::LargeListBuilder large_lst_builder(arrow::default_memory_pool(), large_str_values);

			const std::vector<std::vector<const char *>> rows = {{"alpha", "b"}, {"charlie", nullptr}, {"d", "echo"}};
			for (const auto &row : rows)
			{
				check(lst_builder.Append());
				check(large_lst_builder.Append());
				for (auto val : row)
				{
					if (val)
					{
						check(str_values->Append(val));
						check(large_str_values->Append(val));
					}
					else
					{
						check(str_values->AppendNull());
						check(large_str_values->AppendNull());
					}
				}
			}

			std::shared_ptr<arrow::Array> lst_array, large_lst_array;
			check(lst_builder.Finish(&lst_array));
			check(large_lst_builder.Finish(&large_lst_array));

			fields.push_back(arrow::field("lst_str", arrow::list(arrow::utf8()), /*nullable=*/true));
			fields.push_back(arrow::field("large_lst_str", arrow::large_list(arrow::utf8()), /*nullable=*/true));
			arrays.push_back(lst_array);
			arrays.push_back(large_lst_array);
		}
		else
		{
			throw std::runtime_error("parquet_debug_write_list_fixture: unknown variant: " + v); // GCOVR_EXCL_LINE
		}

		auto schema = arrow::schema(fields);
		auto table = arrow::Table::Make(schema, arrays);

		// file-I/O backstop, not fixture-triggerable.
		auto outfile_result = arrow::io::FileOutputStream::Open(path);
		if (!outfile_result.ok())
			throw std::runtime_error("parquet_debug_write_list_fixture: failed to open '" + // GCOVR_EXCL_LINE
				std::string(path) + "': " + outfile_result.status().ToString()); // GCOVR_EXCL_LINE
		auto outfile = outfile_result.ValueOrDie();

		parquet::ArrowWriterProperties::Builder arrow_writer_builder;
		arrow_writer_builder.store_schema();
		auto arrow_writer_properties = arrow_writer_builder.build();
		auto writer_properties = parquet::WriterProperties::Builder().build();
		auto status = parquet::arrow::WriteTable(*table, arrow::default_memory_pool(), outfile,
			table->num_rows(), writer_properties, arrow_writer_properties);
		check(status);
		check(outfile->Close());
	}

	// Builds the final Arrow table from every appended column, writes it to the output file
	// (error stops if any declared column was never written), and frees `handle`. A writer that
	// used the streaming row-group API (row_group_writer set -- see parquet_finish_row_group)
	// takes a completely different path: the file was already opened and written to
	// incrementally, one row group at a time, so there is no table to build here at all --
	// close_streaming_writer (below) only needs to verify completeness and finalize the footer.
	static void close_streaming_writer(ConcurrencyGuard<ParquetWriterHandle> &writer_handle)
	{
		if (writer_handle->in_row_group)
		{ // GCOVR_EXCL_START -- this throw is never caught anywhere in the call chain, so it crosses
		  // the extern "C" boundary uncaught -> std::terminate() -> abort, discarding that whole
		  // process's gcov coverage; tested via scenario_row_group_dangling_at_close
			delete writer_handle.release();
			throw std::runtime_error("A row group was started via parquet_new_row_group but never finished via "
				"parquet_finish_row_group before close");
		}
		// GCOVR_EXCL_STOP

		for (size_t i = 0; i < writer_handle->fields.size(); ++i)
		{
			if (i >= writer_handle->arrays.size() || !writer_handle->arrays[i]) continue; // streamed column, not
			                                                                              // whole -- nothing to
			                                                                              // reconcile against.
			if (writer_handle->arrays[i]->length() != writer_handle->streamed_rows_total)
			{ // GCOVR_EXCL_START -- this throw is never caught anywhere in the call chain, so it
			  // crosses the extern "C" boundary uncaught -> std::terminate() -> abort, discarding
			  // that whole process's gcov coverage; tested via
			  // scenario_row_group_whole_column_undercovered
				auto name = writer_handle->fields[i]->name();
				auto declared = writer_handle->arrays[i]->length();
				auto covered = writer_handle->streamed_rows_total;
				delete writer_handle.release();
				throw std::runtime_error("column '" + name + "' has " + std::to_string(declared) +
					" rows (written via parquet_write_column), but only " + std::to_string(covered) +
					" were covered by row groups written via parquet_new_row_group/parquet_write_column_chunk/"
					"parquet_finish_row_group -- every row of a column written as a whole array must also be "
					"covered by a row group");
			}
			// GCOVR_EXCL_STOP
		}

		auto status = writer_handle->row_group_writer->Close();
		if (!status.ok())
		{ // GCOVR_EXCL_START -- file-I/O backstop, not fixture-triggerable
			delete writer_handle.release();
			throw std::runtime_error(status.ToString());
		}
		// GCOVR_EXCL_STOP
		status = writer_handle->outfile->Close();
		delete writer_handle.release();
		if (!status.ok())
			throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE
	}

	void close_parquet_writer(void *handle)
	{
		auto writer_handle = as_handle(handle);

		// in_row_group is included here (not just row_group_writer) so a row group that was
		// opened via parquet_new_row_group but never finished even once -- row_group_writer
		// itself is only ever set inside parquet_finish_row_group's first successful call --
		// still reaches close_streaming_writer's own dangling-row-group check below, instead of
		// silently falling through to the batch (WriteTable) path with a column that only has a
		// pending_chunk_arrays entry and no arrays[] entry at all (which previously crashed
		// arrow::Table::Make with a null Array).
		if (writer_handle->row_group_writer || writer_handle->in_row_group)
		{
			close_streaming_writer(writer_handle);
			return;
		}

		if (!writer_handle->column_metadata.empty())
		{
			// fields/arrays are always kept resized in lockstep with column_metadata by
			// parquet_add_column_metadata (its only growth site) -- no size check needed here
			// (same reasoning as append_column's own resize removal).
			for (size_t i = 0; i < writer_handle->column_metadata.size(); ++i)
			{
				// column_metadata is populated by parquet_add_column_metadata, a public extern "C"
				// entry point on the schema-less/dynamic-metadata writer path (distinct from the
				// parquet_schema/add_field flow, whose own "missing write" check lives entirely on
				// the Fortran side, parquet_write.f90's enabled_columns check) -- this guards the
				// same ABI boundary directly, against any C caller bypassing the Fortran wrapper.
				if (!writer_handle->fields[i] || !writer_handle->arrays[i])
				{ // GCOVR_EXCL_START
					auto missing = writer_handle->column_metadata[i].name;
					delete writer_handle.release();
					throw std::runtime_error("Missing column data before close: " + missing);
				}
				// GCOVR_EXCL_STOP
			}
		}

		auto metadata = build_file_metadata(writer_handle->column_metadata, writer_handle->table_metadata);
		auto schema = arrow::schema(writer_handle->fields, metadata);
		auto table = arrow::Table::Make(schema, writer_handle->arrays);

		// writer_handle->chunk_size <= 0 means the caller never passed
		// chunk_size to parquet_open_writer: auto-size it now that the
		// final table exists, targeting a row group size in BYTES rather
		// than a flat row count. A flat row-count cap doesn't know how wide
		// a row is: for a handful of int32 columns, a few hundred thousand
		// rows might be a few MB, while for a table with several vector
		// columns (large col_size) the same row count could be gigabytes --
		// sized this way, both end up with row groups in the same
		// ballpark of actual bytes, which is what Parquet's own row-group
		// size guidance (roughly 128MB-1GB) is actually about, and what
		// drives per-row-group compression efficiency and decode cost.
		// kMinAutoChunkSizeRows/kMaxAutoChunkSizeRows bound the result so
		// pathological row widths still produce something reasonable: an
		// extremely wide row (e.g. a huge vector column) is floored so a
		// table isn't fragmented into an absurd number of tiny row groups,
		// and an extremely narrow row is capped so a huge table doesn't
		// collapse into one single, enormous row group either.
		auto effective_chunk_size = writer_handle->chunk_size;
		if (effective_chunk_size <= 0)
		{
			static constexpr int64_t kTargetRowGroupBytes = 256LL * 1024 * 1024; // ~256 MiB
			static constexpr int64_t kMinAutoChunkSizeRows = 1000;
			// 10,000,000: high enough that the byte target above governs for
			// any realistically-shaped table (the row-count cap only starts
			// to bind below ~27 bytes/row -- e.g. a single narrow column --
			// see kTargetRowGroupBytes/kMaxAutoChunkSizeRows), while still
			// backstopping genuinely pathological cases (a handful of bytes
			// per row at billions of rows) from collapsing into one giant
			// row group spanning the whole file.
			static constexpr int64_t kMaxAutoChunkSizeRows = 10000000;
			// The kMinAutoChunkSizeRows floor exists to avoid fragmenting a
			// table into an excessive number of tiny row groups when rows are
			// moderately wide -- but blindly applying it regardless of row
			// width defeats the whole point of sizing by bytes: if a single
			// row is already close to (or bigger than) kTargetRowGroupBytes
			// (e.g. a vector column with a very large col_size), forcing
			// kMinAutoChunkSizeRows rows into one row group would produce a
			// row group many times the intended size. kMaxFloorOvershootFactor
			// bounds how far the floor is allowed to push things past the
			// target before it's abandoned in favor of a smaller-than-floor
			// (down to 1 row) row group instead -- an under-sized row group is
			// a much smaller problem than one that is unboundedly oversized.
			static constexpr double kMaxFloorOvershootFactor = 4.0;

			auto num_rows = table->num_rows();
			auto total_bytes = arrow::util::TotalBufferSize(*table);
			if (num_rows > 0 && total_bytes > 0)
			{
				double bytes_per_row = static_cast<double>(total_bytes) / static_cast<double>(num_rows);
				auto rows_for_target = static_cast<int64_t>(
					static_cast<double>(kTargetRowGroupBytes) / std::max(bytes_per_row, 1.0));

				if (rows_for_target >= kMinAutoChunkSizeRows)
				{
					effective_chunk_size = std::min<int64_t>(rows_for_target, kMaxAutoChunkSizeRows);
				}
				else if (static_cast<double>(kMinAutoChunkSizeRows) * bytes_per_row
					<= static_cast<double>(kTargetRowGroupBytes) * kMaxFloorOvershootFactor)
				{
					// Floor overshoots the target, but only by a bounded,
					// acceptable amount -- apply it as usual.
					effective_chunk_size = kMinAutoChunkSizeRows;
				}
				else
				{
					// Even the floor would blow far past the target (rows
					// this wide): accept a smaller-than-floor row group
					// (down to 1 row) instead of a wildly oversized one.
					effective_chunk_size = std::max<int64_t>(rows_for_target, 1);
				}
				effective_chunk_size = std::min(effective_chunk_size, num_rows);
			}
			else
			{
				effective_chunk_size = num_rows;
			}
			if (effective_chunk_size < 1) effective_chunk_size = 1;

			// Vector-column int32 per-row-group element-count ceiling (see
			// kArrowInt32ListElementCountLimit) -- always resolvable by clamping down, since
			// col_size alone is already separately bounded by check_col_size_fits_arrow_limit
			// (so even a 1-row row group can never itself overflow), meaning this auto-sized
			// path never needs to abort. An explicit caller-chosen chunk_size, below, is
			// validated instead of silently overridden.
			auto max_col_size = max_fixed_size_list_col_size(writer_handle->fields);
			if (max_col_size > 1)
			{
				int64_t limit = g_debug_list_element_count_limit > 0 ? g_debug_list_element_count_limit : kArrowInt32ListElementCountLimit;
				effective_chunk_size = std::min(effective_chunk_size, std::max<int64_t>(limit / max_col_size, 1));
			}
		}
		else
		{
			check_explicit_chunk_size_fits_arrow_limit(effective_chunk_size, writer_handle->fields, "close_parquet_writer");
		}

		parquet::ArrowWriterProperties::Builder arrow_writer_builder;
		arrow_writer_builder.store_schema();
		// See create_parquet_reader for why this defaults to true (Arrow's own
		// default is false); callers can opt out via
		// parquet_open_writer(..., use_threads=.false.).
		arrow_writer_builder.set_use_threads(writer_handle->use_threads);
		auto arrow_writer_properties = arrow_writer_builder.build();
		// WriterProperties has its own, separate max_row_group_length, defaulting
		// to parquet::DEFAULT_MAX_ROW_GROUP_LENGTH (1,048,576) -- independent of
		// and silently overriding the chunk_size passed to WriteTable below
		// whenever effective_chunk_size exceeds it. Set explicitly here so
		// effective_chunk_size (whether auto-sized above or given by the
		// caller) is the actual, sole authority on row group size.
		auto writer_properties = parquet::WriterProperties::Builder()
			.compression(writer_handle->compression_codec)
			->compression_level(writer_handle->compression_level)
			->max_row_group_length(effective_chunk_size)
			->build();

		auto status = parquet::arrow::WriteTable(
			*table,
			arrow::default_memory_pool(),
			writer_handle->outfile,
			effective_chunk_size,
			writer_properties,
			arrow_writer_properties);
		if (!status.ok())
		{ // GCOVR_EXCL_START -- file-I/O backstop, not fixture-triggerable
			delete writer_handle.release();
			throw std::runtime_error(status.ToString());
		}
		// GCOVR_EXCL_STOP

		status = writer_handle->outfile->Close();
		delete writer_handle.release();
		if (!status.ok())
			throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE
	}

}
