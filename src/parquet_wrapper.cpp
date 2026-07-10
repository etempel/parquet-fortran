#if __cplusplus < 202002L
#error "parquet-fortran requires C++20 (Arrow/Parquet headers use std::span unconditionally, regardless of Arrow version). " \
	"Set FPM_CXXFLAGS to include -std=c++20 (see README.md) before running fpm build/test."
#endif

#include <arrow/api.h>
#include <arrow/array/concatenate.h>
#include <arrow/array/util.h>
#include <arrow/compute/api.h>
#include <arrow/compute/initialize.h>
#include <arrow/io/api.h>
#include <arrow/util/byte_size.h>
#include <arrow/util/compression.h>
#include <arrow/util/thread_pool.h>
#include <parquet/arrow/reader.h>
#include <parquet/arrow/writer.h>

#include <algorithm>
#include <atomic>
#include <cctype>
#include <cerrno>
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
		{
			std::fprintf(stderr,
				"parquet-fortran: concurrent access to a single %s detected: each thread must use "
				"its own independent parquet_reader/parquet_writer instance (see the README's Thread "
				"safety section) -- do not call into the same one from more than one thread at a time. "
				"Aborting.\n", what);
			std::fflush(stderr);
			std::abort();
		}
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

	void parquet_maml_lock()
	{
		g_maml_mutex.lock();
	}

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
		std::string filename;
		int64_t nrows = 0; // effective row count: equal to total_nrows until a filter narrows it (see parquet_reader_set_filter).
		int64_t total_nrows = 0; // the file's true, unfiltered row count -- kept for parquet_reader_print_stat's "of N total".
		std::unordered_map<int, std::shared_ptr<arrow::Array>> column_cache;
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
		bool qc_enabled = false;
		bool qc_soft = false;
		std::unordered_map<int, QcRule> qc_rules;
		std::unordered_set<int> qc_null_warned;
		std::unordered_set<int> qc_range_warned;
		std::atomic<bool> busy{false}; // guards against two threads calling into the same reader at once; see ConcurrencyGuard.
	};

	static ConcurrencyGuard<ParquetWriterHandle> as_handle(void *handle)
	{
		return ConcurrencyGuard<ParquetWriterHandle>(static_cast<ParquetWriterHandle *>(handle), "parquet_writer");
	}

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
	[[noreturn]] static void report_fatal_error(const char *context, const std::string &message)
	{
		std::fprintf(stderr, "parquet-fortran: %s: %s\n", context, message.c_str());
		std::fflush(stderr);
		std::abort();
	}

	static int64_t get_column_index(const ParquetReaderHandle *reader_handle, const char *name)
	{
		auto idx = reader_handle->schema->GetFieldIndex(name);
		if (idx < 0)
		{
			throw std::runtime_error(std::string("Column not found: ") + name);
		}
		return static_cast<int64_t>(idx);
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
	}

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
		auto filtered = arrow::compute::Filter(array, reader_handle->filter_mask);
		if (!filtered.ok())
		{
			throw std::runtime_error(filtered.status().ToString());
		}
		return filtered.ValueOrDie().make_array();
	}

	// Reads (and caches) exactly one column's data from disk -- every other
	// column in the file is never touched, regardless of how many columns
	// the file has or how large they are. This is what makes reading a
	// large file with many columns, but only asking for a few of them,
	// cheap: nothing beyond the footer/schema is read until this is called.
	static std::shared_ptr<arrow::Array> get_single_chunk_array(ParquetReaderHandle *reader_handle, const char *name)
	{
		auto idx = get_column_index(reader_handle, name);
		auto cached = reader_handle->column_cache.find(static_cast<int>(idx));
		if (cached != reader_handle->column_cache.end())
		{
			return cached->second;
		}

		std::shared_ptr<arrow::ChunkedArray> chunked;
		auto status = reader_handle->reader->ReadColumn(static_cast<int>(idx), &chunked);
		if (!status.ok())
		{
			throw std::runtime_error(status.ToString());
		}

		auto array = apply_filter_mask(reader_handle, combine_column_chunks(chunked, name));
		reader_handle->column_cache.emplace(static_cast<int>(idx), array);
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

	static bool parse_double_strict(const std::string &s, double &out)
	{
		if (s.empty()) return false;
		char *end = nullptr;
		double v = std::strtod(s.c_str(), &end);
		if (end != s.c_str() + s.size()) return false;
		out = v;
		return true;
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
			if (array->type_id() == arrow::Type::INT32)
			{
				auto arr = std::static_pointer_cast<arrow::Int32Array>(array);
				for (int64_t i = 0; i < n; ++i) if (!arr->IsNull(i)) scan(arr->Value(i));
			}
			else
			{
				auto arr = std::static_pointer_cast<arrow::Int64Array>(array);
				for (int64_t i = 0; i < n; ++i) if (!arr->IsNull(i)) scan(arr->Value(i));
			}
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
			if (array->type_id() == arrow::Type::FLOAT)
			{
				auto arr = std::static_pointer_cast<arrow::FloatArray>(array);
				for (int64_t i = 0; i < n; ++i) if (!arr->IsNull(i)) scan(static_cast<double>(arr->Value(i)));
			}
			else
			{
				auto arr = std::static_pointer_cast<arrow::DoubleArray>(array);
				for (int64_t i = 0; i < n; ++i) if (!arr->IsNull(i)) scan(arr->Value(i));
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
		{
			auto arr = std::static_pointer_cast<arrow::StringArray>(array);
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
			for (int64_t i = 0; i < n; ++i) if (!arr->IsNull(i)) scan(std::string(arr->GetView(i)));
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
		default:
			return false;
		}

		// Core message only (no "WARNING: "/"parquet-fortran: " prefix) --
		// run_qc_checks adds whichever suits the soft vs hard mode.
		out_message = "qc violation for column '" + colname + "' (based on incomplete column information): declared " +
			bounds_desc + ", data range [" + data_min_s + ", " + data_max_s + "], " +
			std::to_string(n_violate) + " of " + std::to_string(n_valid) + " valid element(s) out of range";
		return true;
	}

	// Runs both read-time QC checks for column `idx`/`name` against
	// `array` (whatever was just decoded/cached for it -- already the
	// filtered version, if a filter is set), if this reader has qc enabled
	// and this column has a rule declared for it. In soft mode (qc_soft),
	// each of the two checks (Null-presence, range) prints a WARNING to
	// stdout at most once per column for the whole lifetime of the reader
	// -- qc_null_warned/qc_range_warned record that, so a column
	// read/prefetched/filtered more than once doesn't repeat the same
	// warning. In hard mode (the default), the first violation of either
	// check aborts the process via report_fatal_error, so the throttling
	// sets are never consulted. Called from mark_read/mark_read_string (an
	// actual typed read), parquet_reader_prefetch_columns, and
	// parquet_reader_set_filter (for a column the filter itself touches) --
	// i.e. every place this reader already tracks as "touched" for
	// parquet_reader_print_stat.
	static void run_qc_checks(ParquetReaderHandle *reader_handle, int idx, const std::string &name,
		const std::shared_ptr<arrow::Array> &array)
	{
		if (!reader_handle->qc_enabled) return;
		auto it = reader_handle->qc_rules.find(idx);
		if (it == reader_handle->qc_rules.end()) return;

		auto flat = flatten_for_stats(array);

		if (reader_handle->qc_null_warned.find(idx) == reader_handle->qc_null_warned.end())
		{
			std::string msg;
			if (run_qc_null_check(flat, it->second, name, msg))
			{
				if (!reader_handle->qc_soft)
				{
					report_fatal_error("qc hard check", msg);
				}
				std::fprintf(stdout, "WARNING: %s\n", msg.c_str());
				reader_handle->qc_null_warned.insert(idx);
			}
		}

		if (reader_handle->qc_range_warned.find(idx) == reader_handle->qc_range_warned.end())
		{
			std::string msg;
			if (run_qc_range_check(flat, it->second, name, msg))
			{
				if (!reader_handle->qc_soft)
				{
					report_fatal_error("qc hard check", msg);
				}
				std::fprintf(stdout, "WARNING: %s\n", msg.c_str());
				reader_handle->qc_range_warned.insert(idx);
			}
		}
	}

	// Records that a typed parquet_read_* entry point actually read `name`
	// (as opposed to it merely being cached via get_single_chunk_array's own
	// cache-fill or via parquet_reader_prefetch_columns) and what Fortran-side
	// output type it was read into -- used only for parquet_reader_print_stat.
	static void mark_read(ParquetReaderHandle *reader_handle, const char *name, const char *type_name)
	{
		auto idx = static_cast<int>(get_column_index(reader_handle, name));
		reader_handle->was_read.insert(idx);
		reader_handle->output_type_used[idx] = type_name;
		run_qc_checks(reader_handle, idx, name, reader_handle->column_cache.at(idx));
	}

	static void mark_read_string(ParquetReaderHandle *reader_handle, const char *name, int64_t item_len)
	{
		auto idx = static_cast<int>(get_column_index(reader_handle, name));
		reader_handle->was_read.insert(idx);
		reader_handle->output_type_used[idx] = "string";
		reader_handle->output_str_len_used[idx] = item_len;
		run_qc_checks(reader_handle, idx, name, reader_handle->column_cache.at(idx));
	}

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
		report_fatal_error(context, std::string("type mismatch for column: ") + name +
			" (expected fixed_size_list/list/large_list, got " + array->type()->ToString() + ")");
	}

	static void copy_string_with_padding(char *dst, int64_t item_len, const std::string_view &src)
	{
		std::memset(dst, ' ', static_cast<size_t>(item_len));
		auto ncopy = std::min<int64_t>(item_len, static_cast<int64_t>(src.size()));
		if (ncopy > 0)
		{
			std::memcpy(dst, src.data(), static_cast<size_t>(ncopy));
		}
	}

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
	}

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

	static std::string trim_right_spaces_and_nuls(const std::string &s)
	{
		size_t end = s.size();
		while (end > 0 && (s[end - 1] == ' ' || s[end - 1] == '\0'))
		{
			--end;
		}
		return s.substr(0, end);
	}

	static std::string build_votable_xml(const std::string &table_name,
									const std::vector<ColumnMetadata> &columns,
									const std::vector<TableMetadataEntry> &table_metadata,
									const std::string &date)
	{
		std::ostringstream xml;
		xml << "<?xml version='1.0'?>\n"
			<< "<VOTABLE version=\"1.4\" xmlns=\"http://www.ivoa.net/xml/VOTable/v1.3\">\n"
			<< "<RESOURCE>\n"
			<< "<TABLE name=\"" << xml_escape(table_name) << "\">\n"
			<< "<PARAM arraysize=\"19\" datatype=\"char\" name=\"DATE\" value=\""
			<< xml_escape(date) << "\">\n"
			<< "<DESCRIPTION>file creation date (YYYY-MM-DDThh:mm:ss UT)</DESCRIPTION>\n"
			<< "</PARAM>\n"
		;

		for (const auto &kv : table_metadata)
		{
			xml << "<PARAM datatype=\"char\" arraysize=\"*\" name=\""
				<< xml_escape(kv.key)
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
			xml << "<FIELD datatype=\"" << xml_escape(col.data_type)
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

	static bool has_any_null(const int8_t *valid_in, int64_t n)
	{
		if (valid_in == nullptr) return false;
		for (int64_t i = 0; i < n; ++i)
		{
			if (valid_in[i] == 0) return true;
		}
		return false;
	}

	static void append_column(
		ParquetWriterHandle *writer_handle,
		const std::string &name,
		const std::shared_ptr<arrow::Field> &field,
		const std::shared_ptr<arrow::Array> &array)
	{
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
			const auto target_size = writer_handle->column_metadata.size();
			if (writer_handle->fields.size() < target_size)
			{
				writer_handle->fields.resize(target_size);
			}
			if (writer_handle->arrays.size() < target_size)
			{
				writer_handle->arrays.resize(target_size);
			}

			auto idx = static_cast<size_t>(metadata_index);
			if (writer_handle->fields[idx] || writer_handle->arrays[idx])
			{
				throw std::runtime_error("Column written more than once: " + name);
			}

			writer_handle->fields[idx] = field;
			writer_handle->arrays[idx] = array;
			return;
		}

		writer_handle->fields.push_back(field);
		writer_handle->arrays.push_back(array);
	}

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

	void *create_parquet_writer(const char *filename)
	{
		auto *handle = new ParquetWriterHandle{};
		auto result = arrow::io::FileOutputStream::Open(filename);
		if (!result.ok())
		{
			delete handle;
			report_fatal_error("create_parquet_writer",
				std::string("failed to open '") + filename + "' for writing: " + result.status().ToString());
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
		if (compression_name == "lz4") return arrow::Compression::LZ4_FRAME;
		throw std::runtime_error("Unknown compression codec: " + compression_name);
	}

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
			throw std::runtime_error("parquet_set_max_threads: n must be >= 1");
		}
		auto status = arrow::SetCpuThreadPoolCapacity(n);
		if (!status.ok())
		{
			throw std::runtime_error(status.ToString());
		}
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
			delete handle;
			report_fatal_error("create_parquet_reader",
				std::string("failed to open '") + filename + "' for reading: " + infile_result.status().ToString());
		}
		auto infile = infile_result.ValueOrDie();
		parquet::arrow::FileReaderBuilder builder;
		auto status = builder.Open(infile);
		if (!status.ok())
		{
			delete handle;
			report_fatal_error("create_parquet_reader",
				std::string("failed to open '") + filename + "': " + status.ToString());
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
			delete handle;
			report_fatal_error("create_parquet_reader",
				std::string("failed to open '") + filename + "': " + status.ToString());
		}

		status = handle->reader->GetSchema(&handle->schema);
		if (!status.ok())
		{
			delete handle;
			report_fatal_error("create_parquet_reader",
				std::string("failed to open '") + filename + "': " + status.ToString());
		}

		handle->nrows = handle->reader->parquet_reader()->metadata()->num_rows();
		handle->total_nrows = handle->nrows;
		return handle;
	}

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
		// columns that are already in column_cache.
		std::vector<int> indices;
		std::vector<std::string> names;
		indices.reserve(static_cast<size_t>(n));
		names.reserve(static_cast<size_t>(n));
		for (int64_t i = 0; i < n; ++i)
		{
			std::string name(names_packed + i * item_len, static_cast<size_t>(item_len));
			name = trim_right_spaces_and_nuls(name);
			int idx = static_cast<int>(get_column_index(reader_handle, name.c_str()));
			reader_handle->was_prefetched.insert(idx);
			auto cached = reader_handle->column_cache.find(idx);
			if (cached != reader_handle->column_cache.end())
			{
				// Already decoded (by an earlier prefetch, read, or filter
				// evaluation) -- still counts as "touched" by this prefetch
				// call, so QC still runs for it here (it may not have, e.g.
				// if the only earlier touch was a plain get_col_size/
				// get_string_length query, which doesn't run QC itself).
				run_qc_checks(reader_handle, idx, name, cached->second);
				continue;
			}
			indices.push_back(idx);
			names.push_back(name);
		}

		if (indices.empty()) return;

		std::shared_ptr<arrow::Table> table;
		auto status = reader_handle->reader->ReadTable(indices, &table);
		if (!status.ok())
		{
			throw std::runtime_error(status.ToString());
		}

		for (size_t i = 0; i < indices.size(); ++i)
		{
			auto chunked = table->column(static_cast<int>(i));
			auto array = apply_filter_mask(reader_handle, combine_column_chunks(chunked, names[i]));
			reader_handle->column_cache[indices[i]] = array;
			run_qc_checks(reader_handle, indices[i], names[i], array);
		}
	}

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

			auto idx = reader_handle->schema->GetFieldIndex(name);
			if (idx < 0) continue; // qc-maml may declare columns not present in this file -- fine, just ignore them.

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

			reader_handle->qc_rules[static_cast<int>(idx)] = rule;
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
			else
			{
				auto arr = std::static_pointer_cast<arrow::Int64Array>(array);
				for (int64_t i = 0; i < n; ++i)
				{
					bool ok = !arr->IsNull(i) && compare_op<int64_t>(arr->Value(i), parsed, op);
					combined[static_cast<size_t>(i)] = combined[static_cast<size_t>(i)] && ok;
				}
			}
			return true;
		}
		case arrow::Type::FLOAT:
		case arrow::Type::DOUBLE:
		{
			double parsed;
			if (is_string || !parse_double_strict(value_text, parsed))
			{
				err = "value '" + value_text + "' is not a valid number for column '" + colname + "'";
				return false;
			}
			if (array->type_id() == arrow::Type::FLOAT)
			{
				auto arr = std::static_pointer_cast<arrow::FloatArray>(array);
				for (int64_t i = 0; i < n; ++i)
				{
					bool ok = !arr->IsNull(i) && compare_op<double>(static_cast<double>(arr->Value(i)), parsed, op);
					combined[static_cast<size_t>(i)] = combined[static_cast<size_t>(i)] && ok;
				}
			}
			else
			{
				auto arr = std::static_pointer_cast<arrow::DoubleArray>(array);
				for (int64_t i = 0; i < n; ++i)
				{
					bool ok = !arr->IsNull(i) && compare_op<double>(arr->Value(i), parsed, op);
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
		{
			if (!is_string)
			{
				err = "value for string column '" + colname + "' must be double-quoted";
				return false;
			}
			auto arr = std::static_pointer_cast<arrow::StringArray>(array);
			for (int64_t i = 0; i < n; ++i)
			{
				bool ok = !arr->IsNull(i) && compare_op<std::string>(std::string(arr->GetView(i)), value_text, op);
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

		for (int64_t i = 0; i < n; ++i)
		{
			std::string name(names_packed + i * name_len, static_cast<size_t>(name_len));
			name = trim_right_spaces_and_nuls(name);
			std::string op(ops_packed + i * op_len, static_cast<size_t>(op_len));
			op = trim_right_spaces_and_nuls(op);
			std::string value(values_packed + i * value_len, static_cast<size_t>(value_len));
			value = trim_right_spaces_and_nuls(value);
			bool is_string = is_string_flags[i] != 0;

			if (reader_handle->schema->GetFieldIndex(name) < 0)
			{
				std::snprintf(err_out, static_cast<size_t>(err_cap), "unknown column in filter: %s", name.c_str());
				return 1;
			}

			std::shared_ptr<arrow::Array> array;
			try
			{
				array = get_single_chunk_array(reader_handle, name.c_str());
			}
			catch (const std::exception &e)
			{
				std::snprintf(err_out, static_cast<size_t>(err_cap), "failed to read filter column '%s': %s", name.c_str(), e.what());
				return 1;
			}

			if (array->type_id() == arrow::Type::FIXED_SIZE_LIST || array->type_id() == arrow::Type::LIST)
			{
				std::snprintf(err_out, static_cast<size_t>(err_cap),
					"filter column '%s' is a vector column; filtering only supports scalar columns", name.c_str());
				return 1;
			}

			int idx = static_cast<int>(reader_handle->schema->GetFieldIndex(name));
			if (std::find(touched_indices.begin(), touched_indices.end(), idx) == touched_indices.end())
			{
				touched_indices.push_back(idx);
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
			// in order and joined with ", " at print time.
			reader_handle->filter_clauses[idx].push_back(value.empty() ? op : op + value);
		}

		arrow::BooleanBuilder mask_builder;
		auto append_status = mask_builder.AppendValues(combined.data(), static_cast<int64_t>(combined.size()));
		if (!append_status.ok())
		{
			std::snprintf(err_out, static_cast<size_t>(err_cap), "failed to build filter mask: %s", append_status.ToString().c_str());
			return 1;
		}
		std::shared_ptr<arrow::Array> mask_array;
		auto finish_status = mask_builder.Finish(&mask_array);
		if (!finish_status.ok())
		{
			std::snprintf(err_out, static_cast<size_t>(err_cap), "failed to build filter mask: %s", finish_status.ToString().c_str());
			return 1;
		}
		reader_handle->filter_mask = std::static_pointer_cast<arrow::BooleanArray>(mask_array);

		int64_t matched = 0;
		for (uint8_t v : combined) matched += (v != 0);
		reader_handle->nrows = matched;

		// Every filter column was decoded (and cached) above, before
		// filter_mask existed -- re-filter those specific cache entries now
		// so they're consistent with every other column, which will only
		// ever see the filtered version (via apply_filter_mask, from here on).
		ensure_compute_initialized();
		for (int idx : touched_indices)
		{
			auto it = reader_handle->column_cache.find(idx);
			if (it == reader_handle->column_cache.end()) continue;
			auto filtered = arrow::compute::Filter(it->second, reader_handle->filter_mask);
			if (!filtered.ok())
			{
				std::snprintf(err_out, static_cast<size_t>(err_cap), "failed to apply filter: %s", filtered.status().ToString().c_str());
				return 1;
			}
			it->second = filtered.ValueOrDie().make_array();
			reader_handle->was_prefetched.insert(idx);
			run_qc_checks(reader_handle, idx, reader_handle->schema->field(idx)->name(), it->second);
		}

		return 0;
	}

	// Non-throwing existence check, so callers (parquet_prefetch_columns) can
	// validate names up front and report a clean Fortran-side error stop,
	// instead of letting get_column_index's std::runtime_error escape
	// uncaught across the Fortran/C++ boundary.
	int64_t parquet_reader_has_column(void *handle, const char *name)
	{
		auto reader_handle = as_reader_handle(handle);
		return reader_handle->schema->GetFieldIndex(name) >= 0 ? 1 : 0;
	}

	int64_t parquet_reader_get_column_col_size(void *handle, const char *name)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		return get_col_size(array);
	}

	int64_t parquet_reader_get_column_total_elements(void *handle, const char *name)
	{
		auto reader_handle = as_reader_handle(handle);
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
		if (array->type_id() == arrow::Type::STRING)
		{
			auto sarr = std::static_pointer_cast<arrow::StringArray>(array);
			for (int64_t i = 0; i < sarr->length(); ++i)
			{
				if (sarr->IsNull(i)) continue;
				max_len = std::max(max_len, static_cast<int64_t>(sarr->GetView(i).size()));
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
		{
			throw std::runtime_error(std::string("Column is not string-like: ") + name);
		}

		auto vals = std::static_pointer_cast<arrow::StringArray>(list_values);
		for (int64_t i = 0; i < vals->length(); ++i)
		{
			if (vals->IsNull(i)) continue;
			max_len = std::max(max_len, static_cast<int64_t>(vals->GetView(i).size()));
		}
		return max_len;
	}

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
			"nulls", "min", "max", "qcmin", "qcmax", "qcmiss", "prefetc", "read", "filter"};
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
			if (flat->type_id() == arrow::Type::STRING)
			{
				auto sarr = std::static_pointer_cast<arrow::StringArray>(flat);
				int64_t max_len = 0;
				for (int64_t i = 0; i < sarr->length(); ++i)
				{
					if (sarr->IsNull(i)) continue;
					max_len = std::max(max_len, static_cast<int64_t>(sarr->GetView(i).size()));
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
			auto qc_it = reader_handle->qc_rules.find(idx);
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
			std::fprintf(stdout, "columns: %d   shown: %zu   rows: %lld (of %lld total)\n\n",
				reader_handle->schema->num_fields(), rows.size(),
				static_cast<long long>(reader_handle->nrows), static_cast<long long>(reader_handle->total_nrows));
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
		if (!any_null) return;

		if (valid_out == nullptr)
		{
			report_fatal_error(context, std::string("column contains Null value(s), which is not supported: ") + name);
		}

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
		if (!any_null) return;

		if (valid_out == nullptr)
		{
			report_fatal_error(context, std::string("column contains Null value(s), which is not supported: ") + name);
		}

		for (int64_t i = 0; i < nrows; ++i)
		{
			valid_out[i] = (list_array->IsValid(i) && vals_any->IsValid(i * col_size + offset)) ? 1 : 0;
		}
	}

	template <typename T>
	static void fill_null_default(T *data, const int8_t *valid_out, int64_t n)
	{
		if (!valid_out) return;
		for (int64_t k = 0; k < n; ++k)
		{
			if (!valid_out[k]) data[k] = T{};
		}
	}

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
		report_fatal_error(context, std::string("type mismatch for column: ") + name +
			" (expected fixed_size_list/list/large_list, got " + array->type()->ToString() + ")");
	}

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
		default:
			report_fatal_error(context, std::string("type mismatch for column: ") + name +
				" (expected int32/int64, got " + vals->type()->ToString() + ")");
		}
	}

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
		{
			auto arr = std::static_pointer_cast<arrow::Int32Array>(vals);
			for (int64_t i = 0; i < n; ++i) data[i] = static_cast<int64_t>(arr->Value(offset + i * stride));
			break;
		}
		default:
			report_fatal_error(context, std::string("type mismatch for column: ") + name +
				" (expected int64/int32, got " + vals->type()->ToString() + ")");
		}
	}

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
		{
			auto arr = std::static_pointer_cast<arrow::Int32Array>(vals);
			for (int64_t i = 0; i < n; ++i) data[i] = static_cast<float>(arr->Value(offset + i * stride));
			break;
		}
		case arrow::Type::INT64:
		{
			auto arr = std::static_pointer_cast<arrow::Int64Array>(vals);
			for (int64_t i = 0; i < n; ++i) data[i] = static_cast<float>(arr->Value(offset + i * stride));
			break;
		}
		default:
			report_fatal_error(context, std::string("type mismatch for column: ") + name +
				" (expected float32/float64/int32/int64, got " + vals->type()->ToString() + ")");
		}
	}

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
		{
			auto arr = std::static_pointer_cast<arrow::Int32Array>(vals);
			for (int64_t i = 0; i < n; ++i) data[i] = static_cast<double>(arr->Value(offset + i * stride));
			break;
		}
		case arrow::Type::INT64:
		{
			auto arr = std::static_pointer_cast<arrow::Int64Array>(vals);
			for (int64_t i = 0; i < n; ++i) data[i] = static_cast<double>(arr->Value(offset + i * stride));
			break;
		}
		default:
			report_fatal_error(context, std::string("type mismatch for column: ") + name +
				" (expected float64/float32/int32/int64, got " + vals->type()->ToString() + ")");
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

template <typename CType>
static void read_list_primitive_row(void *handle, const char *name, int64_t row_index, CType *data, int64_t col_size, int8_t *valid_out)
{
	auto reader_handle = as_reader_handle(handle);
	auto array = get_single_chunk_array(reader_handle, name);
	auto vals_any = get_row_list_values(array, name, row_index, col_size, "parquet_read_array_row_mode");
	report_nulls_list_full(array, vals_any, name, 1, col_size, row_index - 1, valid_out, "parquet_read_array_row_mode");

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
	mark_read(reader_handle, name, ctype_name<CType>());
}

template <typename CType>
static void read_list_primitive_element(void *handle, const char *name, int64_t col_index, CType *data, int64_t nrows, int8_t *valid_out)
{
	auto reader_handle = as_reader_handle(handle);
	auto array = get_single_chunk_array(reader_handle, name);
	auto col_size = get_col_size(array);
	if (col_index < 1 || col_index > col_size)
	{
		report_fatal_error("parquet_read_array_element_mode", "col_index out of bounds");
	}
	auto vals_any = get_uniform_list_values(array, name, nrows, col_size, "parquet_read_array_element_mode");
	auto offset = col_index - 1;
	report_nulls_list_element(array, vals_any, name, nrows, col_size, offset, valid_out, "parquet_read_array_element_mode");

	// vals_any holds every row's full col_size-element vector back to back,
	// so the value for row i at this fixed col_index sits at a stride of
	// col_size apart, starting at offset -- not contiguous, hence passing
	// stride/offset explicitly here (unlike the row-mode call above).
	if constexpr (std::is_same_v<CType, int32_t>)
		convert_values_to_int32(vals_any, data, nrows, name, "parquet_read_array_element_mode", col_size, offset);
	else if constexpr (std::is_same_v<CType, int64_t>)
		convert_values_to_int64(vals_any, data, nrows, name, "parquet_read_array_element_mode", col_size, offset);
	else if constexpr (std::is_same_v<CType, float>)
		convert_values_to_float32(vals_any, data, nrows, name, "parquet_read_array_element_mode", col_size, offset);
	else if constexpr (std::is_same_v<CType, double>)
		convert_values_to_float64(vals_any, data, nrows, name, "parquet_read_array_element_mode", col_size, offset);

	fill_null_default(data, valid_out, nrows);
	mark_read(reader_handle, name, ctype_name<CType>());
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
		mark_read(reader_handle, name, "int32");
	}

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
		mark_read(reader_handle, name, "int64");
	}

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
		mark_read(reader_handle, name, "float32");
	}

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
		mark_read(reader_handle, name, "float64");
	}

	void parquet_read_bool8_column(void *handle, const char *name, int8_t *data, int64_t nrows, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		if (array->type_id() != arrow::Type::BOOL)
		{
			report_fatal_error("parquet_read_bool8_column", std::string("type mismatch for column: ") + name +
				" (expected bool, got " + array->type()->ToString() + ")");
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
		mark_read(reader_handle, name, "bool8");
	}

	void parquet_read_string_column(void *handle, const char *name, char *data, int64_t item_len, int64_t nrows, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		if (array->type_id() != arrow::Type::STRING)
		{
			report_fatal_error("parquet_read_string_column", std::string("type mismatch for column: ") + name +
				" (expected string, got " + array->type()->ToString() + ")");
		}
		auto arr = std::static_pointer_cast<arrow::StringArray>(array);
		if (arr->length() != nrows)
		{
			report_fatal_error("parquet_read_string_column", std::string("nrows mismatch for column: ") + name);
		}
		check_or_report_nulls(arr, name, valid_out, "parquet_read_string_column");
		for (int64_t i = 0; i < nrows; ++i)
		{
			auto view = arr->GetView(i);
			copy_string_with_padding(data + i * item_len, item_len, view);
		}
		fill_null_default_string(data, item_len, valid_out, nrows);
		mark_read_string(reader_handle, name, item_len);
	}

	void parquet_read_int32_array_column(void *handle, const char *name, int32_t *data, int64_t nrows, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, "parquet_read_int32_array_column");
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out, "parquet_read_int32_array_column");
		int64_t total = nrows * col_size;
		convert_values_to_int32(vals_any, data, total, name, "parquet_read_int32_array_column");
		fill_null_default(data, valid_out, total);
		mark_read(reader_handle, name, "int32");
	}

	void parquet_read_int64_array_column(void *handle, const char *name, int64_t *data, int64_t nrows, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, "parquet_read_int64_array_column");
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out, "parquet_read_int64_array_column");
		int64_t total = nrows * col_size;
		convert_values_to_int64(vals_any, data, total, name, "parquet_read_int64_array_column");
		fill_null_default(data, valid_out, total);
		mark_read(reader_handle, name, "int64");
	}

	void parquet_read_float32_array_column(void *handle, const char *name, float *data, int64_t nrows, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, "parquet_read_float32_array_column");
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out, "parquet_read_float32_array_column");
		int64_t total = nrows * col_size;
		convert_values_to_float32(vals_any, data, total, name, "parquet_read_float32_array_column");
		fill_null_default(data, valid_out, total);
		mark_read(reader_handle, name, "float32");
	}

	void parquet_read_float64_array_column(void *handle, const char *name, double *data, int64_t nrows, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, "parquet_read_float64_array_column");
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out, "parquet_read_float64_array_column");
		int64_t total = nrows * col_size;
		convert_values_to_float64(vals_any, data, total, name, "parquet_read_float64_array_column");
		fill_null_default(data, valid_out, total);
		mark_read(reader_handle, name, "float64");
	}

	void parquet_read_bool8_array_column(void *handle, const char *name, int8_t *data, int64_t nrows, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, "parquet_read_bool8_array_column");
		if (vals_any->type_id() != arrow::Type::BOOL)
		{
			report_fatal_error("parquet_read_bool8_array_column", std::string("type mismatch for list values in column: ") + name +
				" (expected bool, got " + vals_any->type()->ToString() + ")");
		}
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out, "parquet_read_bool8_array_column");
		auto vals = std::static_pointer_cast<arrow::BooleanArray>(vals_any);
		for (int64_t i = 0; i < nrows * col_size; ++i)
		{
			data[i] = vals->Value(i) ? 1 : 0;
		}
		fill_null_default(data, valid_out, nrows * col_size);
		mark_read(reader_handle, name, "bool8");
	}

	void parquet_read_string_array_column(void *handle, const char *name, char *data, int64_t item_len, int64_t nrows, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, "parquet_read_string_array_column");
		if (vals_any->type_id() != arrow::Type::STRING)
		{
			report_fatal_error("parquet_read_string_array_column", std::string("type mismatch for list values in column: ") + name +
				" (expected string, got " + vals_any->type()->ToString() + ")");
		}
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out, "parquet_read_string_array_column");
		auto vals = std::static_pointer_cast<arrow::StringArray>(vals_any);
		for (int64_t i = 0; i < nrows * col_size; ++i)
		{
			copy_string_with_padding(data + i * item_len, item_len, vals->GetView(i));
		}
		fill_null_default_string(data, item_len, valid_out, nrows * col_size);
		mark_read_string(reader_handle, name, item_len);
	}

	void parquet_read_int32_array_row(void *handle, const char *name, int64_t row_index, int32_t *data, int64_t col_size, int8_t *valid_out)
	{
		read_list_primitive_row<int32_t>(handle, name, row_index, data, col_size, valid_out);
	}

	void parquet_read_int64_array_row(void *handle, const char *name, int64_t row_index, int64_t *data, int64_t col_size, int8_t *valid_out)
	{
		read_list_primitive_row<int64_t>(handle, name, row_index, data, col_size, valid_out);
	}

	void parquet_read_float32_array_row(void *handle, const char *name, int64_t row_index, float *data, int64_t col_size, int8_t *valid_out)
	{
		read_list_primitive_row<float>(handle, name, row_index, data, col_size, valid_out);
	}

	void parquet_read_float64_array_row(void *handle, const char *name, int64_t row_index, double *data, int64_t col_size, int8_t *valid_out)
	{
		read_list_primitive_row<double>(handle, name, row_index, data, col_size, valid_out);
	}

	void parquet_read_bool8_array_row(void *handle, const char *name, int64_t row_index, int8_t *data, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto vals_any = get_row_list_values(array, name, row_index, col_size, "parquet_read_bool8_array_row");
		if (vals_any->type_id() != arrow::Type::BOOL)
			report_fatal_error("parquet_read_bool8_array_row", std::string("type mismatch for list values in column: ") + name +
				" (expected bool, got " + vals_any->type()->ToString() + ")");
		report_nulls_list_full(array, vals_any, name, 1, col_size, row_index - 1, valid_out, "parquet_read_bool8_array_row");

		auto vals = std::static_pointer_cast<arrow::BooleanArray>(vals_any);
		for (int64_t j = 0; j < col_size; ++j)
		{
			data[j] = vals->Value(j) ? 1 : 0;
		}
		fill_null_default(data, valid_out, col_size);
		mark_read(reader_handle, name, "bool8");
	}

	void parquet_read_string_array_row(void *handle, const char *name, int64_t row_index, char *data, int64_t item_len, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto vals_any = get_row_list_values(array, name, row_index, col_size, "parquet_read_string_array_row");
		if (vals_any->type_id() != arrow::Type::STRING)
			report_fatal_error("parquet_read_string_array_row", std::string("type mismatch for list values in column: ") + name +
				" (expected string, got " + vals_any->type()->ToString() + ")");
		report_nulls_list_full(array, vals_any, name, 1, col_size, row_index - 1, valid_out, "parquet_read_string_array_row");

		auto vals = std::static_pointer_cast<arrow::StringArray>(vals_any);
		for (int64_t j = 0; j < col_size; ++j)
		{
			copy_string_with_padding(data + j * item_len, item_len, vals->GetView(j));
		}
		fill_null_default_string(data, item_len, valid_out, col_size);
		mark_read_string(reader_handle, name, item_len);
	}

	void parquet_read_int32_array_element(void *handle, const char *name, int64_t col_index, int32_t *data, int64_t nrows, int64_t unused_col_size, int8_t *valid_out)
	{
		read_list_primitive_element<int32_t>(handle, name, col_index, data, nrows, valid_out);
	}

	void parquet_read_int64_array_element(void *handle, const char *name, int64_t col_index, int64_t *data, int64_t nrows, int64_t unused_col_size, int8_t *valid_out)
	{
		read_list_primitive_element<int64_t>(handle, name, col_index, data, nrows, valid_out);
	}

	void parquet_read_float32_array_element(void *handle, const char *name, int64_t col_index, float *data, int64_t nrows, int64_t unused_col_size, int8_t *valid_out)
	{
		read_list_primitive_element<float>(handle, name, col_index, data, nrows, valid_out);
	}

	void parquet_read_float64_array_element(void *handle, const char *name, int64_t col_index, double *data, int64_t nrows, int64_t unused_col_size, int8_t *valid_out)
	{
		read_list_primitive_element<double>(handle, name, col_index, data, nrows, valid_out);
	}

	void parquet_read_bool8_array_element(void *handle, const char *name, int64_t col_index, int8_t *data, int64_t nrows, int64_t, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto col_size = get_col_size(array);
		if (col_index < 1 || col_index > col_size)
			report_fatal_error("parquet_read_bool8_array_element", "col_index out of bounds");
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, "parquet_read_bool8_array_element");
		if (vals_any->type_id() != arrow::Type::BOOL)
			report_fatal_error("parquet_read_bool8_array_element", std::string("type mismatch for list values in column: ") + name +
				" (expected bool, got " + vals_any->type()->ToString() + ")");
		auto offset = col_index - 1;
		report_nulls_list_element(array, vals_any, name, nrows, col_size, offset, valid_out, "parquet_read_bool8_array_element");

		auto vals = std::static_pointer_cast<arrow::BooleanArray>(vals_any);
		for (int64_t i = 0; i < nrows; ++i)
		{
			data[i] = vals->Value(i * col_size + offset) ? 1 : 0;
		}
		fill_null_default(data, valid_out, nrows);
		mark_read(reader_handle, name, "bool8");
	}

	void parquet_read_string_array_element(void *handle, const char *name, int64_t col_index, char *data, int64_t item_len, int64_t nrows, int64_t, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto col_size = get_col_size(array);
		if (col_index < 1 || col_index > col_size)
			report_fatal_error("parquet_read_string_array_element", "col_index out of bounds");
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, "parquet_read_string_array_element");
		if (vals_any->type_id() != arrow::Type::STRING)
			report_fatal_error("parquet_read_string_array_element", std::string("type mismatch for list values in column: ") + name +
				" (expected string, got " + vals_any->type()->ToString() + ")");
		auto offset = col_index - 1;
		report_nulls_list_element(array, vals_any, name, nrows, col_size, offset, valid_out, "parquet_read_string_array_element");

		auto vals = std::static_pointer_cast<arrow::StringArray>(vals_any);
		for (int64_t i = 0; i < nrows; ++i)
		{
			copy_string_with_padding(data + i * item_len, item_len, vals->GetView(i * col_size + offset));
		}
		fill_null_default_string(data, item_len, valid_out, nrows);
		mark_read_string(reader_handle, name, item_len);
	}

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
		auto value_builder = std::make_shared<BuilderType>();
		arrow::FixedSizeListBuilder list_builder(arrow::default_memory_pool(), value_builder, static_cast<int32_t>(col_size));
		auto status = list_builder.AppendValues(nrows);
		if (!status.ok())
			throw std::runtime_error(status.ToString());
		status = value_builder->AppendValues(data, nrows * col_size, valid_bytes);
		if (!status.ok())
			throw std::runtime_error(status.ToString());
		status = list_builder.Finish(&array);
		if (!status.ok())
			throw std::runtime_error(status.ToString());
	}
	else
	{
		BuilderType builder;
		auto status = builder.AppendValues(data, nrows, valid_bytes);
		if (!status.ok())
			throw std::runtime_error(status.ToString());
		status = builder.Finish(&array);
		if (!status.ok())
			throw std::runtime_error(status.ToString());
	}

	append_column(writer_handle, name, build_field(name, value_type, col_size, has_any_null(valid_in, nrows * col_size)), array);
}

extern "C"
{

	void parquet_append_int32_column(void *handle, const char *name, const int32_t *data, int64_t nrows, int64_t col_size, const int8_t *valid_in)
	{
		append_typed_column<arrow::Int32Builder>(handle, name, data, nrows, col_size, valid_in, arrow::int32());
	}

	void parquet_append_int64_column(void *handle, const char *name, const int64_t *data, int64_t nrows, int64_t col_size, const int8_t *valid_in)
	{
		append_typed_column<arrow::Int64Builder>(handle, name, data, nrows, col_size, valid_in, arrow::int64());
	}

	void parquet_append_float32_column(void *handle, const char *name, const float *data, int64_t nrows, int64_t col_size, const int8_t *valid_in)
	{
		append_typed_column<arrow::FloatBuilder>(handle, name, data, nrows, col_size, valid_in, arrow::float32());
	}

	void parquet_append_float64_column(void *handle, const char *name, const double *data, int64_t nrows, int64_t col_size, const int8_t *valid_in)
	{
		append_typed_column<arrow::DoubleBuilder>(handle, name, data, nrows, col_size, valid_in, arrow::float64());
	}

	void parquet_append_bool8_column(void *handle, const char *name, const int8_t *data, int64_t nrows, int64_t col_size, const int8_t *valid_in)
	{
		append_typed_column<arrow::BooleanBuilder>(
			handle, name, reinterpret_cast<const uint8_t *>(data), nrows, col_size, valid_in, arrow::boolean());
	}

	void parquet_append_string_column(void *handle, const char *name, const char *data, int64_t item_len, int64_t nrows, const int8_t *valid_in)
	{
		auto writer_handle = as_handle(handle);
		arrow::StringBuilder builder;

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
				throw std::runtime_error(status.ToString());
		}

		std::shared_ptr<arrow::Array> array;
		status = builder.Finish(&array);
		if (!status.ok())
			throw std::runtime_error(status.ToString());

		append_column(writer_handle, name, build_field(name, arrow::utf8(), 1, has_any_null(valid_in, nrows)), array);
	}

	void parquet_append_string_array_column(void *handle, const char *name, const char *data, int64_t item_len, int64_t nrows, int64_t col_size, const int8_t *valid_in)
	{
		auto writer_handle = as_handle(handle);
		auto value_builder = std::make_shared<arrow::StringBuilder>();
		arrow::FixedSizeListBuilder list_builder(arrow::default_memory_pool(), value_builder, static_cast<int32_t>(col_size));

		auto status = list_builder.AppendValues(nrows);
		if (!status.ok())
			throw std::runtime_error(status.ToString());

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
				throw std::runtime_error(status.ToString());
		}

		std::shared_ptr<arrow::Array> array;
		status = list_builder.Finish(&array);
		if (!status.ok())
			throw std::runtime_error(status.ToString());

		append_column(writer_handle, name, build_field(name, arrow::utf8(), col_size), array);
	}


	void close_parquet_writer(void *handle)
	{
		auto writer_handle = as_handle(handle);

		if (!writer_handle->column_metadata.empty())
		{
			if (writer_handle->fields.size() != writer_handle->column_metadata.size() ||
				writer_handle->arrays.size() != writer_handle->column_metadata.size())
			{
				delete writer_handle.release();
				throw std::runtime_error("Internal error: schema/data size mismatch before close");
			}

			for (size_t i = 0; i < writer_handle->column_metadata.size(); ++i)
			{
				if (!writer_handle->fields[i] || !writer_handle->arrays[i])
				{
					auto missing = writer_handle->column_metadata[i].name;
					delete writer_handle.release();
					throw std::runtime_error("Missing column data before close: " + missing);
				}
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
		{
			delete writer_handle.release();
			throw std::runtime_error(status.ToString());
		}

		status = writer_handle->outfile->Close();
		delete writer_handle.release();
		if (!status.ok())
			throw std::runtime_error(status.ToString());
	}

}
