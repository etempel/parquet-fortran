#if __cplusplus < 202002L
// Keep the message ONE string literal: #error prints its token sequence as-is and does NOT
// concatenate adjacent string literals, so a two-literal form reaches the user with a stray
// `" "` pair and the intervening whitespace in the middle of the sentence.
#error "parquet-fortran requires C++20: set FPM_CXXFLAGS to include -std=c++20 (see README.md) before running fpm build or fpm test. Arrow/Parquet headers use std::span unconditionally, regardless of Arrow version."
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
// For ColumnChunkMetaData::statistics()'s return type: parquet/metadata.h only forward-declares
// parquet::Statistics, so HasNullCount()/null_count() need the full definition. The typed
// subclasses (Int32Statistics/DoubleStatistics/...) the row-group statistics screen casts to, and
// is_min_value_exact()/is_max_value_exact(), also live here.
#include <parquet/statistics.h>
// For ColumnDescriptor::sort_order(), which gates every min/max use in the row-group
// statistics screen (screen_row_groups).
#include <parquet/schema.h>

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
#include <chrono>
#include <cerrno>
#include <cfenv>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <ctime>
#include <cstring>
#include <deque>
#include <memory>
#include <limits>
#include <numeric>
#include <mutex>
#include <sstream>
#include <stdexcept>
#include <string>
#include <thread>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <vector>

// ==== Handle types, concurrency guard, and process-global state ====
//
// Neither ParquetReaderHandle nor ParquetWriterHandle's own data structures
// (column_cache, the fields/arrays/*_metadata vectors, defined below) are
// synchronized -- concurrent calls into the *same* handle from more than one
// thread race on them (e.g. two threads' unordered_map::emplace on
// column_cache). Each library-facing entry point obtains its handle through
// as_reader_handle/as_handle below, which return this guard instead of a raw
// pointer: it atomically claims ownership of the handle for the duration of
// the call (RAII) and immediately aborts the process if a *different* thread
// is already inside a call on the same handle, rather than silently racing.
// This intentionally does NOT forbid handing a reader/writer off between
// threads sequentially (only true overlap is rejected), and it does not make
// it safe/meaningful to call into one reader/writer from many threads at once
// for speed -- see the README's Thread safety section: each thread must still
// use its own independent instance for that. Declared outside the extern "C"
// block below because templates cannot be given C language linkage.
//
// **The guard records WHICH thread owns the handle, not merely that someone
// does, and is re-entrant for that owner.** It used to be a plain
// `std::atomic<bool> busy`, which made any nested claim on one handle abort --
// including the library's own. That mattered once parquet_writer_enter/
// parquet_writer_leave (below) let the *Fortran* half of a write claim the
// same guard for the whole entry point: every C++ call it then makes is a
// nested claim from the owning thread and must be allowed through. Ownership
// is a per-thread token rather than std::thread::id because
// std::atomic<std::thread::id> is not guaranteed lock-free and its
// compare_exchange compares object representations, which a padded id type
// would break; a uint64_t has neither problem.
//
// Two consequences worth knowing. A missed parquet_writer_leave leaks depth
// rather than wedging the handle: the owning thread keeps working, and only
// the documented sequential hand-off to another thread would be refused. And
// the several `_impl` helpers elsewhere in this file that exist specifically
// to avoid re-entering an exported entry point are no longer load-bearing for
// correctness -- they are still the right shape (one atomic pair instead of
// two), so they stay, but their comments no longer claim an abort would
// follow.
//
// Deliberately terminates here instead of throwing: this guard is
// meant to be hit from worker threads inside a caller's own !$omp/#pragma omp
// parallel region (that's the whole misuse case it exists to catch), and the
// OpenMP specification does not guarantee well-defined behavior for a C++
// exception that escapes a parallel region uncaught -- different compiler/
// OpenMP-runtime combinations are free to handle that differently. Terminating
// directly sidesteps that entirely: it is well-defined from any thread,
// inside or outside any parallel construct, on every platform. See the
// fatal-path note below for why that termination is _Exit and not std::abort().

// ==== The fatal path ====
//
// A fatal error here can be reached by SEVERAL THREADS AT ONCE -- the concurrency guard below
// is meant to be, since its whole purpose is to catch a caller driving one handle from a
// parallel region, where every thread trips it at the same moment.
//
// std::abort() is not safe under that. It takes a lock inside glibc, and when many OpenMP
// threads reach it together they pile up on that lock and the process HANGS FOREVER instead of
// dying. Measured on machine B under ifx at -O0 -check all: 192 threads all parked in
// futex_wait_queue with `__lll_lock_wait_private <- abort` on every stack, surviving SIGTERM
// (the Fortran runtime catches it and its own handler deadlocks too) and killable only with
// SIGKILL. The same configuration also produced an occasional SIGSEGV inside that path. Neither
// is reproducible on demand -- both need enough threads to arrive together -- which is exactly
// what makes relying on abort() here a bad trade.
//
// So: exactly ONE thread reports and terminates. It ends the process with _Exit, a bare
// exit_group syscall -- no lock, no atexit handler, no static destructor, well defined from any
// thread inside or outside a parallel region. Every other thread parks and is reaped when the
// winner ends the process, because a losing thread has nothing useful left to do and any exit
// path it could take is the pile-up this exists to avoid.
//
// Exit code 134 is what a shell reports for a SIGABRT death, so this is indistinguishable from
// the previous behaviour to everything that observes it: the error scenarios, their
// `exitstat /= 0` checks, and the exit status the guide documents.
//
// Coverage is unaffected: _Exit skips the atexit-registered gcov flush exactly as abort() did,
// so every GCOVR_EXCL marker resting on that mechanism stays correct.
//
// NOTE for a future split of this file into several translation units (see CLAUDE.md): this
// flag must become a single `extern` definition, exactly like the g_debug_*/settings-mirror
// globals and the token counter below. Per-TU copies would each admit one thread, which is the
// multi-thread abort this prevents, reintroduced by the back door.
static std::atomic<bool> g_fatal_claimed{false};

// Returns only for the FIRST thread to reach a fatal path; every later one parks forever.
static void claim_fatal_path_or_park()
{
	if (!g_fatal_claimed.exchange(true, std::memory_order_acq_rel)) return;
	for (;;) std::this_thread::sleep_for(std::chrono::hours(1));
}

// Ends the process once a fatal message has been written. Never returns, takes no lock.
[[noreturn]] static void fatal_exit()
{
	std::fflush(stderr);
	std::_Exit(134);
}

// Monotonic source of the per-thread ownership tokens above. Never reused and
// never 0, so 0 unambiguously means "this handle is idle".
//
// NOTE for a future split of this file into several translation units (see
// CLAUDE.md): this counter must become a single `extern` definition, exactly
// like the g_debug_*/settings-mirror globals. Per-TU copies would hand two
// different threads the same token, and each would then be admitted through a
// guard the other holds.
static std::atomic<std::uint64_t> g_next_thread_token{1};

static std::uint64_t this_thread_token()
{
	static thread_local std::uint64_t token = g_next_thread_token.fetch_add(1, std::memory_order_relaxed);
	return token;
}

template <typename Handle>
class ConcurrencyGuard
{
public:
	ConcurrencyGuard(Handle *handle, const char *what) : handle_(handle)
	{
		const std::uint64_t me = this_thread_token();
		std::uint64_t expected = 0;
		if (!handle_->guard_owner.compare_exchange_strong(expected, me, std::memory_order_acq_rel,
				std::memory_order_acquire) &&
			expected != me)
		{ // GCOVR_EXCL_START -- same gcov-loss mechanism as report_fatal_error's own GCOVR_EXCL comment
			// This is the site that MUST tolerate many threads arriving together: see the
			// fatal-path comment above claim_fatal_path_or_park. Only the first gets past here.
			claim_fatal_path_or_park();
			std::fprintf(stderr,
				"parquet-fortran: concurrent access to a single %s detected: each thread must use "
				"its own independent parquet_reader/parquet_writer instance (see the README's Thread "
				"safety section) -- do not call into the same one from more than one thread at a time. "
				"Aborting.\n", what);
			fatal_exit();
		}
		// GCOVR_EXCL_STOP
		// Only ever incremented by the owning thread, so it needs no atomicity of its own.
		++handle_->guard_depth;
	}

	~ConcurrencyGuard()
	{
		if (handle_) guard_leave(handle_);
	}

	ConcurrencyGuard(const ConcurrencyGuard &) = delete;
	ConcurrencyGuard &operator=(const ConcurrencyGuard &) = delete;

	operator Handle *() const { return handle_; }
	Handle *operator->() const { return handle_; }

	// Disarms the guard (its destructor becomes a no-op) and returns the raw
	// pointer, for close_parquet_reader/close_parquet_writer, which delete
	// the underlying handle themselves -- without this, the guard's
	// destructor would touch already-freed memory afterwards. Also how
	// parquet_writer_enter hands a freshly claimed guard over to the Fortran
	// side, which releases it later via parquet_writer_leave.
	Handle *release()
	{
		auto *p = handle_;
		handle_ = nullptr;
		return p;
	}

	// Drops one level of ownership, releasing the handle at depth 0. Shared by
	// the destructor above and parquet_writer_leave, so the two can never
	// disagree about what "release" means.
	static void guard_leave(Handle *handle)
	{
		if (handle->guard_depth <= 0) return;
		if (--handle->guard_depth == 0) handle->guard_owner.store(0, std::memory_order_release);
	}

private:
	Handle *handle_;
};

// The six comparison operators, resolved from their text ONCE per clause rather than per row.
//
// **Honest about what this did and did not buy, because the story is instructive.** compare_op used
// to take the operator as a `const std::string &` and test it with up to five string comparisons,
// per row, for a value that is loop-invariant -- which looked like the obvious explanation for
// evaluate_nodes costing 13.6 ns per row on a plain `>` over doubles. Converting it to this enum
// changed the measurement by **nothing at all** (0.0272 s -> 0.0275 s over 2 M rows): the compiler
// was already hoisting it. The real cost was one line away, in real_family_value_at -- a
// `std::static_pointer_cast` PER ROW, i.e. two atomic refcount operations to read one double.
// Passing those helpers a raw `const arrow::Array *` instead took the same loop to 1.8 ns per row.
//
// The enum is kept because it is free and does not depend on a particular compiler noticing, but it
// is not what made this fast. **The lesson worth carrying: in this file, look for a shared_ptr copy
// in a row loop before looking at anything else.**
enum class CmpOp
{
	Gt,
	Ge,
	Lt,
	Le,
	Eq,
	Ne
};

// Every other operator string is rejected long before this is reached (parquet_parse_filter_rule
// and the qc validators both restrict it), so "/=" is the residual case rather than a guess.
static CmpOp cmp_op_of(const std::string &op)
{
	if (op == ">") return CmpOp::Gt;
	if (op == ">=") return CmpOp::Ge;
	if (op == "<") return CmpOp::Lt;
	if (op == "<=") return CmpOp::Le;
	if (op == "==") return CmpOp::Eq;
	return CmpOp::Ne;
}

// Used by eval_filter_clause (filter row-matching, further below) -- declared
// here, outside extern "C", since templates cannot have C language linkage
// (same reason read_list_primitive_row/ctype_name live between extern "C"
// blocks rather than inside one).
template <typename T>
static bool compare_op(const T &a, const T &b, CmpOp op)
{
	switch (op)
	{
	case CmpOp::Gt: return a > b;
	case CmpOp::Ge: return a >= b;
	case CmpOp::Lt: return a < b;
	case CmpOp::Le: return a <= b;
	case CmpOp::Eq: return a == b;
	default: return a != b;
	}
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
		// Type token the typed add_metadata overload recorded for `value` ("int32",
		// "float64[]", ...), or "" when the value is a string. A parquet key-value pair can
		// only hold text, so this is what build_file_metadata turns into a companion
		// "<key>.datatype" entry, and what build_votable_xml declares as the PARAM's real
		// datatype instead of char. Never an entry of its own on the Fortran side.
		std::string datatype;
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
		// Guards against two threads calling into the same writer at once; see ConcurrencyGuard.
		std::atomic<std::uint64_t> guard_owner{0}; // thread token of the thread currently inside a call; 0 when idle.
		int guard_depth = 0; // re-entry depth for that owner; only ever touched by the owning thread.

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

		// --- Nullability state for the streaming path (see resolve_chunk_nullability) ---
		//
		// A streamed column's Arrow field is fixed when the first row group locks the file's
		// schema, so its nullability has to be decided from that first chunk alone. The rule is
		// PRESENCE, not values: the field is nullable iff the first chunk carried an is_valid
		// mask, whatever that mask said. This map records that answer per column index, and every
		// later chunk for the same column is checked against it (Rule 2).
		std::unordered_map<size_t, bool> chunk_mask_present;
		// Output names of the columns this writer's schema declares protected
		// (extra: protected_cols:, or schema%set_protected). Pushed once by
		// parquet_writer_set_protected_column at open time, before any write. A protected column
		// may hold no Null at all -- enforced Fortran-side, before any of this -- so its field is
		// built NON-nullable on every path, including the two whose nulls live in the element
		// rather than in a mask (temporal, and a parquet_string_column) and which are otherwise
		// unconditionally nullable.
		std::unordered_set<std::string> protected_columns;

		// --- STRUCT column staging (see the "STRUCT column writes" section) ---
		//
		// A struct with M fields of arbitrary kinds cannot cross a fixed bind(C) signature in one
		// call, so a struct write is staged: parquet_struct_begin opens it, one
		// parquet_struct_field_<kind> call per field pushes that field's child array, and
		// parquet_append_struct_column[_chunk] assembles and stores the result. These three
		// members are that staging, and they are per WRITER -- which is what the writer's own
		// concurrency guard (writer_lock, src/parquet_core.f90) makes safe: the Fortran side
		// holds it across the whole begin/push/finish sequence, so two threads writing two struct
		// columns to one writer cannot interleave their pushes.
		std::string struct_staging_name;   // "" when nothing is staged.
		int64_t struct_staging_nrows = 0;  // rows the finisher will be checked against.
		int32_t struct_staging_nfields = 0; // fields the finisher will be checked against.
		std::vector<std::string> struct_staging_field_names;
		std::vector<std::shared_ptr<arrow::Array>> struct_staging_children;
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
		// Whether Nulls are expected for this field: true for a qc: miss: Null/NA AND for a field
		// declaring no qc: miss: at all (an undeclared miss: says nothing about Nulls), false only
		// for an explicit, EMPTY qc: miss:, which is how a maml asks for Null validation. Fortran
		// resolves this and pushes it per rule (parquet_reader_set_qc), so the initializer below is
		// documentation rather than policy -- but keep it matching parquet_qc_rule's own default.
		bool null_values_allowed = true;
	};

	// ---- The sort key, declared HERE rather than beside the comparator that reads it ----
	//
	// `ParquetReaderHandle` below holds one `SortKeyData` by value, so the type has to be complete
	// before that struct is declared. The comparator, the counting path and everything else that
	// reads a key live far below, next to sort_build_permutation; only these three declarations
	// moved up. A `std::unique_ptr<SortKeyData>` to an incomplete type was the alternative and was
	// rejected: it needs a destructor declared in the struct and defined where the type is complete,
	// which compiles until the day someone adds a second such member and does not.

	// Which comparison a bound key uses. Boolean and every temporal type bind as Integer (their
	// values are integers and their order is the integer order), which also lets them reach the
	// counting fast path.
	enum class SortValueKind { Integer, Real, Str };

	// One sort key. Exactly one of ints/reals/strs describes it, matching `kind` -- but the values
	// may either be OWNED (copied into the vector below) or BORROWED (ints_ptr/reals_ptr aimed at a
	// caller's array, with the vector left empty).
	//
	// The comparator always reads through ints_ptr/reals_ptr, never through the vectors, so
	// borrowing costs it nothing at all -- no extra branch in the hottest loop in the engine. Every
	// place that fills a key must therefore call sort_key_finalize() before the key is used;
	// sort_build_permutation checks that it happened rather than dereferencing a null pointer.
	//
	// Borrowing is only ever safe when the borrowed array outlives the sort, which is why it is
	// used by the one-shot parquet_sort_argsort_*/parquet_sort_is_sorted_* entry points (the
	// caller's array is live for the whole call and nothing survives it) and NOT by the
	// SortBuilderHandle adders, whose keys outlive the call that added them.
	struct SortKeyData
	{
		SortValueKind kind = SortValueKind::Integer;
		bool descending = false;
		bool nulls_first = false;
		std::vector<int64_t> ints;             //!< Integer kind, when owned (includes boolean and temporal).
		std::vector<double> reals;             //!< Real kind, when owned (float/double/half_float/uint64/decimal).
		std::vector<std::string_view> strs;    //!< Str kind; views into `owner`'s buffers, or into a caller's.
		std::vector<uint8_t> valid;            //!< 1 = valid; EMPTY means "no nulls at all".
		std::shared_ptr<void> owner;           //!< Keeps whatever backs `strs` alive. Unused when borrowing.
		const int64_t *ints_ptr = nullptr;     //!< What the comparator reads for Integer. Never null once finalized.
		const double *reals_ptr = nullptr;     //!< What the comparator reads for Real. Never null once finalized.
	};

	// Points a key's read pointers at whatever backs it. Call once, after the values are in place
	// and before the key is used; harmless to call on an already-borrowing key, whose pointer is
	// left alone. A vector's heap buffer survives the vector being moved, so a key may be moved
	// into a handle after this without invalidating anything.
	static inline void sort_key_finalize(SortKeyData &key)
	{
		if (key.ints_ptr == nullptr) key.ints_ptr = key.ints.data();
		if (key.reals_ptr == nullptr) key.reals_ptr = key.reals.data();
	}

	// Deliberately does NOT hold a materialized arrow::Table: opening a file
	// only parses the (small) footer/schema via FileReaderBuilder, so no
	// column's data is ever read from disk until that specific column is
	// actually requested (see get_single_chunk_array). column_cache holds
	// each column already read this way, keyed by its schema field index, so
	// asking for the same column twice (e.g. parquet_get_col_size followed
	// by parquet_read_column) doesn't re-read it from disk. Entries are always
	// post-transform (see sort_perm), and two paths drop them:
	// parquet_reader_release_column at the caller's request, and
	// parquet_reader_sort_install, which releases the key columns its own
	// bind decoded rather than re-ordering data nobody has asked to read.
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
		int64_t nrows = 0; // effective row count: equal to total_nrows until a filter or sample narrows it (see
		                   // parquet_reader_set_filter/parquet_reader_set_sample).
		int64_t total_nrows = 0; // the file's true, unfiltered row count -- kept for parquet_reader_print_stat's "of N total".
		std::unordered_map<int, std::shared_ptr<arrow::Array>> column_cache;
		// Flat key-value table metadata (parquet_add_table_metadata's own
		// key/value pairs, as written into the Arrow schema's KeyValueMetadata
		// by build_file_metadata -- NOT re-parsed from the embedded VOTable
		// XML). Populated once by create_parquet_reader, so parquet_get_metadata
		// (parquet_metadata.f90) never re-reads the file: every call just
		// scans this in-memory copy.
		std::vector<std::pair<std::string, std::string>> table_metadata_cache;
		// Every readable column of this file, as the (possibly dotted) leaf paths every
		// column-name argument in this library already accepts -- populated once by
		// create_parquet_reader (schema-only, no column data), so parquet_reader_get_column_count/
		// _name_length/_name are all O(1) lookups into it and can never disagree with each other.
		// See collect_column_leaf_paths for exactly which fields become an entry.
		std::vector<std::string> column_path_cache;
		// (The mask itself is declared further down, next to row_group_live_offsets -- see live_mask.)
		// Per-column filter clauses retained solely for parquet_reader_print_stat's
		// "filter" column: each entry is one clause's operator+value with the
		// column name stripped (e.g. ">=0.0"), in the order set_filter saw them.
		// A non-flat expression cannot be decomposed per column ("(a>1 and b<2) or c==3" belongs
		// to three columns jointly), so this stays a best-effort per-column listing and
		// filter_expr_text below carries the authoritative form.
		std::unordered_map<int, std::vector<std::string>> filter_clauses;
		// Set by parquet_reader_set_sort: the row order every column read hands back, as a 0-based
		// Int64Array permutation of length nrows (i.e. of the POST-filter row set -- the sort runs
		// after the mask, so it orders the surviving rows). Null when no sort is active, which is
		// the predicate reader_has_sort_permutation reports. apply_row_transform applies it with
		// arrow::compute::Take right after the mask, so every column ever handed back to Fortran is
		// in sorted order, transparently, once this is set -- and that ONE path is what makes it
		// true, which is why parquet_reader_sort_install can simply release whatever was decoded
		// before the permutation existed rather than re-ordering it in place. A cached entry is
		// therefore always post-transform: it was cached after this was set, or it is not there.
		//
		// A permutation, unlike a mask, destroys row-group locality: sorted row 5 may come from row
		// group 47 and row 6 from row group 3. Everything row-group-scoped is therefore refused
		// while this is set (chunked reads, parquet_get_chunk_size, a sliced parquet_table), and
		// row/element mode fall back to a whole-column read. See reader_has_sort_permutation.
		std::shared_ptr<arrow::Array> sort_perm;
		// The sort keys as re-rendered by the Fortran side ("ra asc, dec desc"), retained solely
		// for parquet_reader_print_stat's own "sort:" line -- never parsed here.
		std::string sort_key_text;
		// ---- One-slot cache of the key parquet_reader_sort_key_info most recently bound --------
		//
		// Installing a read-time sort makes TWO crossings per key (add_read_sort_key,
		// src/parquet_read.f90): _key_info binds the key to report its family and size so Fortran
		// can allocate buffers, then _key_fetch binds it AGAIN to copy the values out. Arrow's
		// decode is shared -- both reach the column through get_single_chunk_array, which caches on
		// column_cache -- so what repeated was sort_bind_arrow_key's O(rows) materialisation: a
		// vector<int64_t>/vector<double>, or for a string key a vector<string_view> plus the buffer
		// that owns it, plus a validity vector. Measured at 5-9% of a whole read-time open.
		//
		// ONE slot, not a map, because the two entry points are strictly paired: one _info
		// immediately followed by one _fetch, per key. The three identity fields are what make a
		// stale slot impossible to consume -- name alone is not enough, since the same column may
		// legitimately appear twice under different directions.
		//
		// The slot is a pure optimisation and _fetch keeps its from-scratch path: if the identity
		// does not match, or the slot is empty, it binds exactly as before. That is deliberate --
		// it is what stops the two entry points becoming secretly order-dependent, and it costs one
		// comparison. Do not replace it with an assertion.
		//
		// Memory is NEUTRAL despite appearances: the reduction used to be built and freed twice,
		// and is now built once and held across one Fortran allocation of comparable size. It does
		// not grow with the key count.
		bool sort_key_cached = false;
		std::string sort_key_cache_name;
		bool sort_key_cache_descending = false;
		bool sort_key_cache_nulls_first = false;
		SortKeyData sort_key_cache;
		// The whole filter expression re-rendered in canonical form by
		// parquet_render_filter_expr (parquet_read_filter.f90) and handed over by
		// parquet_reader_set_filter. Retained solely for parquet_reader_print_stat's own
		// "filter:" line -- never parsed here; the evaluator works from the node list.
		std::string filter_expr_text;
		// Set once by parquet_reader_set_sample when parquet_open_reader's sample_fraction < 1.0 --
		// retained solely for parquet_reader_print_stat's own "sample:" line. sample_seed_used is
		// always the seed the draw actually used, whether caller-supplied (sample_seed > 0) or
		// entropy-drawn, so a caller can read back a non-deterministic run's seed afterward and reuse
		// it for a reproducible repeat.
		bool has_sample = false;
		double sample_fraction = 0.0;
		int64_t sample_seed_used = 0;
		// Set when parquet_reader_set_sample was told a filter= will also be applied right after:
		// the caller-supplied keep mask is held HERE and folded in by parquet_reader_set_filter
		// rather than installed immediately. One reason, and it is about ordering alone:
		// installing a sample mask now would make every subsequent column read -- including the
		// filter's own referenced columns, read while parquet_reader_set_filter evaluates its
		// clauses -- come back already sample-compacted, breaking the row-index alignment clause
		// evaluation depends on (confirmed by a real Arrow "must all be the same length" crash
		// when this wasn't deferred).
		//
		// It is NOT about which rows survive the screen. The mask spans the file's physical rows
		// and each entry depends only on (seed, row), so a deferred fold and an immediate install
		// select identically; the screen cannot move a single decision. That was not true while
		// the draw was a sequential engine stepped once per row, and the difference is why this
		// vector can simply be indexed by physical row below.
		bool has_pending_sample = false;
		std::vector<uint8_t> pending_sample_keep;
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
		// Set by parquet_reader_release_column for every column whose cached array it dropped.
		// was_prefetched/was_read are never cleared, so without this parquet_reader_print_stat
		// would still list a released column as "touched" and then look it up in column_cache,
		// where it no longer is -- see its own handling of this.
		std::unordered_set<int> was_released;
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
		// (create_parquet_reader); like total_nrows, it always describes the physical file and is
		// never narrowed by a filter -- a filter changes how many rows each row group yields, not
		// how many row groups there are.
		// chunk_read_row_groups tracks, per column schema-field-index, which 1-based row groups
		// have been read via the chunk API -- used only by parquet_reader_check_complete, called
		// from parquet_close_reader(check_complete=.true.).
		int64_t num_row_groups = 0;
		std::unordered_map<int, std::unordered_set<int64_t>> chunk_read_row_groups;
		// Where each row group starts, in physical file rows: row_group_offsets[i] is the 0-based
		// first row of row group i+1, and the trailing entry is total_nrows, so row group `rg`
		// covers [offsets[rg-1], offsets[rg]). Built once at open time from the footer.
		//
		// This is what makes a filter mask row-group-addressable: the mask is built in physical file
		// order and row groups partition those same rows contiguously, so each row group's own mask
		// is a slice of it. The mask covers only the LIVE rows, though, so the offset that indexes
		// INTO it is row_group_live_offsets below, not this table; this one stays in physical file
		// rows and is what a row range (parquet_reader_set_filter's row_lo/row_hi) is expressed in.
		std::vector<int64_t> row_group_offsets;
		// Surviving (post-mask) row count per row group, 1-based-indexed as [rg-1], filled once
		// whenever a mask is installed and empty when no mask is active. Cached rather than
		// recomputed because parquet_get_chunk_size and every chunked read ask for it repeatedly,
		// and a popcount over one row group's mask segment is O(rows in that row group).
		std::vector<int64_t> row_group_surviving;
		// Row-group statistics pre-screen (screen_row_groups): which row groups the footer could
		// NOT rule out, 1-based-indexed as row_group_live[rg-1]. EMPTY means "no screen has run",
		// i.e. every row group is live -- which is also what it stays for a sample-only mask, for
		// a file whose statistics are missing, and for any expression the screen declines to
		// reason about. Never shrinks the answer: a pruned row group is one whose own mask segment
		// is provably all-false, so the rows it drops are exactly the rows the mask would have
		// dropped anyway (see live_mask).
		std::vector<uint8_t> row_group_live;
		// THE row mask: set by parquet_reader_set_sample and/or parquet_reader_set_filter, true for
		// rows that pass every filter clause (filter=), lie inside the requested row range, and
		// survive random downsampling (sample_fraction=). get_single_chunk_array and
		// parquet_reader_prefetch_columns apply it (via arrow::compute::Filter) to every column
		// right after decoding, so every column ever handed back to Fortran -- and every
		// column_cache entry -- reflects only the matching rows, transparently, once this is set.
		//
		// It covers the LIVE rows only -- every row of every row group that is neither
		// statistics-pruned nor outside a scoped filter's range -- in file order, which is exactly
		// what an array read via read_live_row_groups must be filtered with, since such an array
		// only contains those rows. An excluded row group therefore costs NOTHING here, which is
		// the whole point: a slice-scoped filter on a huge file holds a mask proportional to its
		// own slice rather than to total_nrows. When nothing is excluded the live rows are every
		// row, and this is byte-for-byte the full-length mask it replaced.
		//
		// Non-null exactly when a mask is active; a reader whose filter matched nothing still has
		// one (zero-length, or all-false), so testing this pointer is the "is a mask installed"
		// predicate. Its length is always row_group_live_offsets' own total.
		std::shared_ptr<arrow::BooleanArray> live_mask;
		// Where row group rg begins WITHIN live_mask (0-based), or -1 when rg contributes no rows
		// to it at all (pruned by statistics, or outside a scoped filter's row-group range). Empty
		// whenever live_mask is null. This is the live-space counterpart of row_group_offsets, and
		// the pair is what keeps every row-group-scoped operation a zero-copy slice rather than a
		// rebuild: row_group_mask_segment slices live_mask at row_group_live_offsets[rg-1].
		std::vector<int64_t> row_group_live_offsets;
		// How many row groups the last screen ruled out -- parquet_reader_print_stat's "screened:"
		// line and the test-only parquet_debug_get_row_groups_pruned() hook.
		int64_t row_groups_pruned = 0;
		// Pins the most recent array handed out (by pointer, not value) by
		// parquet_read_string_column_chunk_buffers, whose row-group-scoped array (from
		// get_row_group_chunk_array) is otherwise never retained anywhere -- unlike a whole-column
		// read's *unresolved* (pre-struct-path) array, which column_cache already keeps alive for
		// the reader's whole lifetime. Fortran (parquet_read.f90) always consumes the returned
		// buffer pointers immediately (append_buffers), before any other call on this same reader,
		// so a single slot -- next overwritten by the next such call -- is sufficient; it does not
		// need per-column tracking.
		std::shared_ptr<arrow::Array> last_chunk_buffers_array;
		// Same idea as last_chunk_buffers_array, above, but for parquet_read_string_column_buffers
		// (the whole-column counterpart): column_cache only ever stores the *pre*-unwrap_struct_path
		// array (keyed by the top-level column's physical index), so for a plain (non-struct)
		// column, get_single_chunk_array's returned array genuinely is the same object already
		// cached, and stays alive on its own. But for a dotted struct-field path,
		// unwrap_struct_path builds a brand-new Array (with a freshly allocated combined-validity
		// buffer) that is never placed into column_cache -- nothing kept it alive past the end of
		// parquet_read_string_column_buffers's own local variable, so the moment that function
		// returned, the buffer backing the just-exported validity pointer was freed, and Fortran's
		// append_buffers read freed memory (observed in practice as every row reading back null,
		// not a crash). Pinning here unconditionally (cheap: one extra shared_ptr assignment, even
		// for the already-safe non-struct case) closes this for every case uniformly.
		std::shared_ptr<arrow::Array> last_whole_column_buffers_array;
		// Guards against two threads calling into the same reader at once; see ConcurrencyGuard.
		std::atomic<std::uint64_t> guard_owner{0}; // thread token of the thread currently inside a call; 0 when idle.
		int guard_depth = 0; // re-entry depth for that owner; only ever touched by the owning thread.
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

	// One row group's physical row count, from the offset table built at open time.
	static inline int64_t row_group_rows(ParquetReaderHandle *reader_handle, int64_t row_group)
	{
		return reader_handle->row_group_offsets[static_cast<size_t>(row_group)] -
			reader_handle->row_group_offsets[static_cast<size_t>(row_group - 1)];
	}

	// An all-false BooleanArray of `length` rows. Built only for an EXCLUDED row group's mask
	// segment (see row_group_mask_segment): live_mask holds no bits for such a row group, but a
	// caller that reads its chunk from disk anyway still gets a physical-length array back and
	// needs a same-length mask to filter it with. One row group's worth of bits, allocated only on
	// that path.
	static std::shared_ptr<arrow::BooleanArray> all_false_mask(int64_t length)
	{
		arrow::BooleanBuilder builder;
		auto reserve_status = builder.AppendValues(static_cast<int64_t>(length), false);
		if (!reserve_status.ok())
		{ // GCOVR_EXCL_START -- BooleanBuilder allocation backstop, not fixture-triggerable
			throw std::runtime_error(reserve_status.ToString());
		}
		// GCOVR_EXCL_STOP
		std::shared_ptr<arrow::Array> array;
		auto finish_status = builder.Finish(&array);
		if (!finish_status.ok())
		{ // GCOVR_EXCL_START -- BooleanBuilder allocation backstop, not fixture-triggerable
			throw std::runtime_error(finish_status.ToString());
		}
		// GCOVR_EXCL_STOP
		return std::static_pointer_cast<arrow::BooleanArray>(array);
	}

	// Row group `row_group`'s own slice of the active mask, or nullptr when NO mask is active.
	//
	// THE TWO NULL CASES MUST STAY DISTINGUISHABLE. nullptr means "this reader has no mask at all",
	// and every caller reads it as "hand the chunk back unfiltered". An EXCLUDED row group -- one
	// pruned by statistics or outside a scoped filter's range -- is the opposite: none of its rows
	// survive. live_mask holds no bits for it (that is the memory saving), so this returns a
	// physical-length ALL-FALSE array for it rather than nullptr. Returning nullptr there instead
	// would make a chunked read of a pruned row group hand back every one of its rows unfiltered,
	// silently, with nothing to notice -- see this stage's own mutation test for exactly that.
	//
	// For a live row group the slice needs no copy: Arrow's Slice shares the underlying buffer.
	static std::shared_ptr<arrow::BooleanArray> row_group_mask_segment(
		ParquetReaderHandle *reader_handle, int64_t row_group)
	{
		if (!reader_handle->live_mask) return nullptr;
		int64_t length = row_group_rows(reader_handle, row_group);
		int64_t offset = reader_handle->row_group_live_offsets[static_cast<size_t>(row_group - 1)];
		if (offset < 0) return all_false_mask(length);
		return std::static_pointer_cast<arrow::BooleanArray>(reader_handle->live_mask->Slice(offset, length));
	}

	// Fills row_group_surviving from the mask just installed on the handle: one popcount per row
	// group, done once here rather than per query. Called by every path that installs a mask
	// (parquet_reader_set_filter, parquet_reader_set_sample). An excluded row group contributes 0
	// without materializing its all-false segment.
	static void refresh_row_group_surviving(ParquetReaderHandle *reader_handle)
	{
		reader_handle->row_group_surviving.clear();
		if (!reader_handle->live_mask) return;
		reader_handle->row_group_surviving.reserve(static_cast<size_t>(reader_handle->num_row_groups));
		for (int64_t rg = 1; rg <= reader_handle->num_row_groups; ++rg)
		{
			int64_t offset = reader_handle->row_group_live_offsets[static_cast<size_t>(rg - 1)];
			if (offset < 0)
			{
				reader_handle->row_group_surviving.push_back(0);
				continue;
			}
			int64_t rows = row_group_rows(reader_handle, rg);
			int64_t surviving = 0;
			for (int64_t i = 0; i < rows; ++i)
			{
				if (reader_handle->live_mask->Value(offset + i)) ++surviving;
			}
			reader_handle->row_group_surviving.push_back(surviving);
		}
	}

	// Fills row_group_live_offsets from row_group_live, and returns the total live row count (the
	// length live_mask must have). Called by every path that installs a mask, BEFORE the mask is
	// built, since the offsets are what say where each row group's bits go.
	static int64_t assign_row_group_live_offsets(ParquetReaderHandle *reader_handle)
	{
		reader_handle->row_group_live_offsets.assign(static_cast<size_t>(reader_handle->num_row_groups), -1);
		int64_t live_rows = 0;
		for (int64_t rg = 1; rg <= reader_handle->num_row_groups; ++rg)
		{
			if (!reader_handle->row_group_live.empty() &&
				reader_handle->row_group_live[static_cast<size_t>(rg - 1)] == 0)
			{
				continue;
			}
			reader_handle->row_group_live_offsets[static_cast<size_t>(rg - 1)] = live_rows;
			live_rows += row_group_rows(reader_handle, rg);
		}
		return live_rows;
	}

	// How many rows row group `row_group` yields to the caller: its physical row count, or its
	// surviving count when a filter/sample mask is active. This is what parquet_get_chunk_size
	// reports and what a chunked read returns, so a chunked loop's sizes always sum to
	// parquet_get_nrows -- the same "as if the file only contained the matching rows" contract
	// every other read path already follows.
	static int64_t row_group_effective_rows(ParquetReaderHandle *reader_handle, int64_t row_group)
	{
		if (reader_handle->row_group_surviving.empty()) return row_group_rows(reader_handle, row_group);
		return reader_handle->row_group_surviving[static_cast<size_t>(row_group - 1)];
	}

	// Whether the statistics screen ruled out at least one row group on this reader, i.e. whether
	// a whole-column read may skip anything. False whenever no screen has run, and false when one
	// ran and kept everything -- in both cases every read path takes exactly the same Arrow calls
	// it took before F4 existed.
	static bool reader_has_pruned_row_groups(ParquetReaderHandle *reader_handle)
	{
		return reader_handle->row_groups_pruned > 0;
	}

	// The live row groups as the 0-based indices Arrow's ReadRowGroups wants, in ascending file
	// order (which is what makes the concatenated result still physically ordered, and therefore
	// still alignable with live_mask).
	static std::vector<int> live_row_group_list(ParquetReaderHandle *reader_handle)
	{
		std::vector<int> live;
		live.reserve(static_cast<size_t>(reader_handle->num_row_groups));
		for (int64_t rg = 1; rg <= reader_handle->num_row_groups; ++rg)
		{
			if (reader_handle->row_group_live.empty() ||
				reader_handle->row_group_live[static_cast<size_t>(rg - 1)] != 0)
			{
				live.push_back(static_cast<int>(rg - 1));
			}
		}
		return live;
	}

	// Installs `combined` (one byte per LIVE row, in file order -- the layout
	// assign_row_group_live_offsets just laid out) as the reader's mask, and refreshes the derived
	// per-row-group survivor counts. The single place a mask becomes active, shared by
	// parquet_reader_set_filter and parquet_reader_set_sample.
	//
	// THE INVARIANT F4 RESTS ON: for every column, Filter(live_read, live_mask) yields exactly the
	// surviving rows. It holds by construction now -- an array read via read_live_row_groups spans
	// the live row groups' rows, and so does this mask, element for element -- where it previously
	// depended on a full-length mask being all-false over each pruned row group.
	static bool install_row_mask(ParquetReaderHandle *reader_handle, const std::vector<uint8_t> &combined,
		char *err_out, int64_t err_cap)
	{
		arrow::BooleanBuilder mask_builder;
		auto append_status = mask_builder.AppendValues(combined.data(), static_cast<int64_t>(combined.size()));
		if (!append_status.ok())
		{ // GCOVR_EXCL_START -- BooleanBuilder allocation backstop, not fixture-triggerable
			std::snprintf(err_out, static_cast<size_t>(err_cap), "failed to build filter mask: %s",
				append_status.ToString().c_str());
			return false;
		}
		// GCOVR_EXCL_STOP
		std::shared_ptr<arrow::Array> mask_array;
		auto finish_status = mask_builder.Finish(&mask_array);
		if (!finish_status.ok())
		{ // GCOVR_EXCL_START -- BooleanBuilder allocation backstop, not fixture-triggerable
			std::snprintf(err_out, static_cast<size_t>(err_cap), "failed to build filter mask: %s",
				finish_status.ToString().c_str());
			return false;
		}
		// GCOVR_EXCL_STOP
		reader_handle->live_mask = std::static_pointer_cast<arrow::BooleanArray>(mask_array);
		refresh_row_group_surviving(reader_handle);
		int64_t matched = 0;
		for (uint8_t v : combined) matched += (v != 0);
		reader_handle->nrows = matched;
		return true;
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
	// the fatal exit skips the atexit-registered gcov-flush handler a normal process exit relies
	// on, so any process that reaches this function loses that whole run's coverage data --
	// unobservable by gcov no matter how well-tested, not merely hard to trigger.
	[[noreturn]] static void report_fatal_error(const char *context, const std::string &message) // GCOVR_EXCL_START
	{
		claim_fatal_path_or_park();   // several threads can reach a fatal error at once
		std::fprintf(stderr, "parquet-fortran: %s: %s\n", context, message.c_str());
		fatal_exit();
	}
	// GCOVR_EXCL_STOP

	// ==== Output settings, mirrored from parquet_settings.f90 ====
	//
	// This side prints three warnings of its own (qc soft-mode on read, twice, and an incomplete
	// chunked read under check_hard=.false.) plus the whole parquet_reader_print_stat report, so it
	// needs its own copy of what Fortran decided. Both arrive as already-resolved integers: the
	// tokens are folded and validated once, in Fortran, and a second string parser here is exactly
	// the drift the arrangement avoids.
	//
	// There is deliberately no C++-side setter beyond this one entry point, so Fortran is the single
	// writer and this copy is derived rather than independent.
	static int g_verbosity = 0;      // 0 = normal, 1 = silent, 2 = errors_only
	static int g_message_stream = 0; // 0 = stdout, 1 = stderr

	void parquet_push_output_settings(int verbosity, int message_stream)
	{
		g_verbosity = verbosity;
		g_message_stream = message_stream;
	}

	// ==== File-metadata settings, mirrored from parquet_settings.f90 ====
	//
	// The creation timestamp this side stamps into the `DATE` key and the VOTable sidecar is the
	// ONLY thing that differs between two writes of the same data, so pinning it is the whole of
	// what byte-for-byte reproducibility needs. Fortran decides WHICH of the two behaviours applies
	// and sends it as a flag: there is no "empty means read the clock" rule on this side, because a
	// policy decided in two places is a policy that can disagree with itself.
	//
	// Initialisers equal parquet_settings' own defaults (blank, i.e. read the clock), since they are
	// what applies in the window before the first push -- feature_risks.md Risk-42.
	static int g_file_date_fixed = 0;
	static std::string g_file_date;

	void parquet_push_file_metadata_settings(int use_fixed, const char *date)
	{
		g_file_date_fixed = use_fixed;
		g_file_date = (use_fixed && date != nullptr) ? std::string(date) : std::string();
	}

	// True when output the caller explicitly asked for should be skipped -- the C++ counterpart of
	// parquet_settings' parquet_output_is_suppressed, asked by parquet_reader_print_stat.
	static bool output_is_suppressed(void)
	{
		return g_verbosity >= 1;
	}

	// The one place a C++-side warning is printed, so the three call sites cannot disagree about
	// either setting. Mirrors parquet_emit_warning: suppressed only at errors_only, prefixed here
	// rather than at each site, and routed by the shared stream selector.
	static void emit_warning_cpp(const std::string &msg)
	{
		if (g_verbosity >= 2) return;
		std::fprintf(g_message_stream == 1 ? stderr : stdout, "WARNING: %s\n", msg.c_str());
	}

	// ==== Performance settings, mirrored from parquet_settings.f90 ====
	//
	// Five values this file reads on hot paths: the sort's parallel threshold and its counting
	// fast-path pair (used by sort_counting_candidate and the four sort entry points), the
	// row-group byte target (chunk_size_from_bytes_per_row), and the row-group statistics
	// prescreen (screen_row_groups). Declared here, above every one of those, rather than beside
	// their push function at the bottom of the file.
	//
	// Everything arrives ALREADY RESOLVED, exactly as the output settings above do: Fortran's
	// "0 means the built-in default" sentinel is resolved on the Fortran side, so there is no
	// `g_x > 0 ? g_x : kBuiltIn` conditional anywhere below and no second place the default can be
	// spelled. The initializers here are what applies before Fortran has pushed anything at all,
	// which is why they MUST equal parquet_settings' own parameters of the same name -- the drift
	// feature_risks.md Risk-42 is about, and what test/test_settings.f90 asserts by reading each
	// default back through the C++-observable effect rather than through the Fortran getter alone.
	//
	// These replaced three test-only parquet_debug_* override hooks (sort_parallel_min_rows,
	// disable_sort_counting_path, disable_statistics_prescreen). **Do not reintroduce a debug
	// override for a value that is STILL a setting**: two writers for one behaviour is precisely
	// the drift parquet_settings exists to remove.
	//
	// `sort_parallel_min_rows` is the exception, and it is an exception because it is no longer a
	// setting at all -- it was retired once the Fortran engine stopped reading it, leaving the C++
	// floor an internal constant that no fixture a test can build could ever reach. Its override
	// (parquet_debug_set_sort_parallel_min_rows, far below) is therefore the ONLY writer, not a
	// second one. The rule is about competing writers, not about debug hooks.

	//! Rows below which threading is refused outright: spawning threads to sort a small array costs
	//! more than the sort saves.
	//!
	//! MEASURED, not guessed -- an 8-thread argsort of random real64 against the serial one, best of
	//! 15 rounds each, on an 8-core arm64 laptop: 2k rows 0.86x (threading LOSES), 8k 1.48x, 16k
	//! 2.18x, 32k 2.50x, 65k 2.60x, 1M 3.23x. Break-even sits between 2k and 8k, so 8192 is the
	//! first power of two that is reliably a win. An earlier provisional 65536 was four times too
	//! conservative and left most real sorts serial for no reason.
	//!
	//! Re-measure before changing it. This is a correctness-adjacent default rather than a tuning
	//! knob: set it too low and every trivial sort pays for threads it cannot use.
	static constexpr int64_t kSortParallelMinRows = 1 << 13;
	// Counting-sort ceiling: 4M buckets, i.e. at most 32 MB of int64 counters. Above this the
	// comparator sort is used instead, which is why the bound is on the key's value RANGE and not
	// on its row count.
	static constexpr int64_t kSortCountingBucketLimit = 1 << 22;
	// Row-group byte target -- see chunk_size_from_bytes_per_row, far below, for what it governs
	// and for the three bounds that are NOT settings and stay declared beside it.
	static constexpr int64_t kTargetRowGroupBytes = 256LL * 1024 * 1024; // ~256 MiB

	// Test-only override for kSortParallelMinRows, which is otherwise unreachable now that the
	// published `sort_parallel_min_rows` setting has been retired. Every fixture a test can
	// build is orders of magnitude below 8192 rows, so without a way down every C++-engine
	// threading test would assert "serial matches serial" -- feature_risks.md Risk-35's vacuous
	// shape, and Risk-49's unreachable-threshold shape at the same time. Negative = use the
	// real constant.
	static int64_t g_debug_sort_parallel_min_rows = -1;
	static bool g_sort_counting_path = true;
	static int64_t g_sort_counting_bucket_limit = kSortCountingBucketLimit;
	static int64_t g_target_row_group_bytes = kTargetRowGroupBytes;
	static bool g_statistics_prescreen = true;

	void parquet_push_performance_settings(int sort_counting_path,
		int64_t sort_counting_bucket_limit, int64_t target_row_group_bytes, int statistics_prescreen)
	{
		g_sort_counting_path = (sort_counting_path != 0);
		g_sort_counting_bucket_limit = sort_counting_bucket_limit;
		g_target_row_group_bytes = target_row_group_bytes;
		g_statistics_prescreen = (statistics_prescreen != 0);
	}

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
	// STRUCT field; the terminal segment must resolve to something other than STRUCT/MAP -- a
	// scalar, FIXED_SIZE_LIST or (variable-length) LIST/LARGE_LIST leaf is fine. Struct-of-struct
	// nesting to any depth is supported, but a path stopping at an intermediate struct, or
	// passing through/landing on a MAP, is not (see CLAUDE.md's nested-struct-field design
	// notes). Throws std::runtime_error, same as get_column_index, on any failure --
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
		if (leaf_id == arrow::Type::STRUCT || leaf_id == arrow::Type::MAP)
		{ // GCOVR_EXCL_START -- dead, see comment above.
			throw std::runtime_error(std::string("Column not found: ") + name +
				" (resolves to a " + field->type()->ToString() +
				" column; struct paths must resolve to a leaf scalar/vector/list column, and MAP is not supported)");
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
		// LIST/LARGE_LIST are ACCEPTED as a leaf: a variable-length list column is readable (into
		// a parquet_list_column, and into a 2-D array when its data happens to be uniform),
		// whether it sits at the top level or under a struct, so refusing it here would have made
		// this probe -- and every query built on it -- disagree with parquet_get_column_names,
		// which lists such a leaf under its dotted path. That disagreement was three separate
		// wrong answers about the same name: listed, `parquet_column_exists` .false., and
		// `parquet_get_column_type` ABORTING with "column not found" about a path the same reader
		// had just listed -- the last of those a defect against that query's own contract, which
		// is that an unreadable type is an ANSWER ("unknown") and not an error.
		//
		// MAP stays refused: nothing can read one yet, so answering .true. would move the failure
		// from a truthful "not found" to an abort further down the read.
		return leaf_id != arrow::Type::STRUCT && leaf_id != arrow::Type::MAP;
	}

	// Appends `field`'s addressable column name(s) to `out`, as the dotted leaf paths
	// resolve_struct_path/struct_path_exists accept: a STRUCT field contributes one entry per
	// leaf beneath it (recursively, to any depth) and no entry for itself, since a bare struct
	// name is not readable; every other field -- scalar, FIXED_SIZE_LIST (vector), and also
	// LIST/LARGE_LIST/MAP -- contributes exactly one entry under its own name.
	//
	// LIST/LARGE_LIST is a readable leaf (into a parquet_list_column, or into a 2-D array when its
	// data happens to be uniform) whether it is top-level or nested inside a struct, so it is
	// listed and every lookup on it answers about a name that really does resolve.
	//
	// MAP is deliberately INCLUDED too, even though nothing can read one: this powers
	// parquet_get_column_names, whose job is to report what the file actually contains, and a
	// caller that goes on to ask parquet_column_exists/parquet_get_column_type about such a name
	// gets a truthful "not a supported type" answer. Silently omitting it would instead make a
	// column simply vanish from a file listing, which is a much harder thing to diagnose.
	static void collect_column_leaf_paths(
		const std::shared_ptr<arrow::Field> &field, const std::string &prefix, std::vector<std::string> &out)
	{
		std::string path = prefix.empty() ? field->name() : prefix + "." + field->name();
		if (field->type()->id() == arrow::Type::STRUCT)
		{
			auto struct_type = std::static_pointer_cast<arrow::StructType>(field->type());
			for (int i = 0; i < struct_type->num_fields(); ++i)
			{
				collect_column_leaf_paths(struct_type->field(i), path, out);
			}
			return;
		}
		out.push_back(path);
	}

	// ==== Struct-path resolution ====
	//
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
	// The parquet leaf column indices a ROW-GROUP read of `child_path` under `top_level_idx` has to
	// ask for.
	//
	// Almost always exactly one, and that is deliberate: reading a single leaf is what makes a
	// chunked read of a scalar, vector or LIST column cheap, and a bare STRUCT name only ever
	// reaches here for its own row validity, which any one of its leaves carries.
	//
	// A MAP is the one shape that needs MORE THAN ONE, and getting it wrong is silent in the worst
	// way: ReadRowGroup with only the key leaf returns a perfectly well-formed map column whose
	// entries struct has ONE field instead of two, so nothing fails until something asks for the
	// values. Both leaves are collected here rather than at the call site so that a future
	// multi-leaf shape has one place to join.
	static void resolve_chunk_leaf_indices(const ParquetReaderHandle *reader_handle, int top_level_idx,
		const std::vector<std::string> &child_path, std::vector<int> &out)
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
		out.clear();
		if (current->field->type()->id() == arrow::Type::MAP)
		{
			// Every leaf beneath the map -- its key and its value, and for a nested value more
			// than two, which Phase 7 will need. Children are pushed in reverse so that popping
			// yields schema order, which is already ascending by column index.
			std::vector<const parquet::arrow::SchemaField *> stack{current};
			while (!stack.empty())
			{
				const parquet::arrow::SchemaField *node = stack.back();
				stack.pop_back();
				if (node->is_leaf())
				{
					out.push_back(static_cast<int>(node->column_index));
					continue;
				}
				for (size_t i = node->children.size(); i > 0; --i)
				{
					stack.push_back(&node->children[i - 1]);
				}
			}
			return;
		}
		while (!current->is_leaf())
		{
			current = &current->children[0];
		}
		out.push_back(static_cast<int>(current->column_index));
	}

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

	// Every no-argument arrow::<type>() factory (arrow::int32(), arrow::utf8(), ...) returns a
	// reference to a function-local static singleton -- normally safe to call concurrently for
	// the first time under C++11 "magic statics", but confirmed via ThreadSanitizer (see
	// CLAUDE.md's Arrow type-singleton race note) to actually race on this project's apt-installed
	// Arrow build in (at least) TWO separate, independently-racy ways: the singleton's OWN
	// construction (its shared_ptr control block), and each singleton's lazily-computed,
	// mutable-cached `fingerprint()`/`metadata_fingerprint()` (arrow::detail::Fingerprintable --
	// every DataType inherits it, and Arrow's own type/field/schema equality checks use it as a
	// fast path, so it is reachable from far more than just an explicit call to fingerprint()).
	// Both are the SAME underlying hazard: a process-wide singleton object with lazily-populated
	// mutable state that two threads can race to populate the first time they both touch it. The
	// corruption this causes doesn't crash where it happens -- it surfaces later, in whatever
	// unrelated code next touches the heap, which is what made the original symptom (a SIGSEGV
	// inside a trivial, unrelated boolean check) so misleading to trace back.
	//
	// Fixed the same way ensure_compute_initialized() above fixes a different lazy-registry race:
	// force every singleton this file uses, AND every lazy cache on it this project has found
	// racing so far, into existence exactly once, from a single thread, before any OpenMP-parallel
	// code path can reach Arrow at all. After that one call, every later concurrent read is just a
	// read of already-published state, which is safe. Keep the type list in sync with every bare
	// (no-argument) arrow::<type>() factory used anywhere in this file -- a parameterized factory
	// (arrow::timestamp(unit), arrow::decimal128(p, s), ...) is NOT affected, since those construct
	// a fresh, non-shared object per call rather than caching a singleton, so nothing to warm up.
	//
	// This is deliberately NOT an exhaustive fix for every possible lazy Arrow cache -- that is an
	// unwinnable fight against Arrow's own internals. Two are known and covered; if a THIRD
	// distinct race against one of these same ten singletons ever surfaces (see CLAUDE.md's note
	// for the "how to tell" signature), add whatever call reproduces it here rather than chasing
	// it as a one-off, and reconsider a broader warm-up (e.g. a full dummy write+close exercising
	// every type) if the individual-cache approach keeps growing.
	static void ensure_arrow_type_singletons_initialized()
	{
		static std::once_flag type_init_flag;
		std::call_once(type_init_flag, []() {
			std::vector<std::shared_ptr<arrow::DataType>> singletons{
				arrow::int32(), arrow::int64(), arrow::float32(), arrow::float64(),
				arrow::boolean(), arrow::utf8(), arrow::large_utf8(), arrow::utf8_view(),
				arrow::binary(), arrow::date32(),
			};
			for (const auto &t : singletons)
			{
				(void)t->fingerprint();
				(void)t->metadata_fingerprint();
			}
		});
	}

	// Casts a STRING_VIEW array to arrow::large_utf8(), returning every other type unchanged.
	// Two callers need it, for the same underlying reason -- a view array's values are inlined or
	// spread across a variable number of data buffers, so it exposes neither a single offsets
	// array nor a single contiguous data buffer:
	//
	//   the row filter, because arrow::compute::Filter (Arrow 24) has no "array_filter" kernel for
	//   arrow::Type::STRING_VIEW
	// at all -- confirmed via NotImplementedError ("Function 'array_filter' has no kernel
	// matching input types (string_view, bool)");
	//
	//   and the compact parquet_string_column read, whose whole point is handing those two buffers
	//   straight to Fortran (see extract_string_buffers/is_offset_string_type).
	//
	// Since every read-side call site already treats
	// STRING_VIEW identically to STRING/LARGE_STRING via is_string_like_type/
	// make_string_like_accessor, working around this Arrow gap by casting to arrow::large_utf8()
	// first (rather than teaching every filter call site about STRING_VIEW specifically) is
	// lossless for every consumer, and has NO visible side effect at all: parquet_reader_print_stat's
	// parquet_type cell reads reader_handle->schema (the file's own schema, populated once by
	// GetSchema and never rewritten), not the column_cache's decoded array, so a filtered
	// STRING_VIEW column still reports "string_view". An earlier version of this comment claimed
	// the cell became "large_string" once a filter was active; that was wrong, and was checked by
	// running print_stat on a filtered STRING_VIEW fixture. Returns `array` unchanged for every
	// other type.
	static arrow::Result<std::shared_ptr<arrow::Array>> coerce_string_view_to_offset_string(const std::shared_ptr<arrow::Array> &array)
	{
		if (array->type_id() != arrow::Type::STRING_VIEW) return array;
		ARROW_ASSIGN_OR_RAISE(auto cast_datum, arrow::compute::Cast(array, arrow::large_utf8()));
		return cast_datum.make_array();
	}

	// Whole-column half of the STRING_VIEW conversion: casts if needed, and REPLACES the cached
	// array with the result so a second compact read of the same column pays nothing. Safe against
	// every other cache consumer -- they reach a string column through make_string_like_accessor,
	// which handles LARGE_STRING identically, and the footer-based null screen does not read the
	// cache at all. parquet_get_column_type is unaffected either way: it resolves against the
	// file's own schema, so it still answers "string".
	//
	// Returns `array` untouched for every other type, and for a dotted struct path, whose array is
	// freshly built by unwrap_struct_path and deliberately not in the cache (see
	// get_single_chunk_array) -- there the caller's own pin is what keeps it alive.
	static std::shared_ptr<arrow::Array> recache_coerced_string_view(ParquetReaderHandle *reader_handle,
		const char *name, const std::shared_ptr<arrow::Array> &array, const char *context)
	{
		if (array->type_id() != arrow::Type::STRING_VIEW) return array;
		ensure_compute_initialized();
		auto coerced = coerce_string_view_to_offset_string(array);
		if (!coerced.ok())
		{ // GCOVR_EXCL_START -- Cast-kernel Status backstop on an already-decoded array.
			report_fatal_error(context, std::string("failed to convert a string_view column for reading: ") + name);
		}
		// GCOVR_EXCL_STOP
		auto result = coerced.ValueOrDie();
		auto idx = reader_handle->schema->GetFieldIndex(name);
		if (idx >= 0)
		{
			auto it = reader_handle->column_cache.find(idx);
			if (it != reader_handle->column_cache.end() && it->second == array) it->second = result;
		}
		return result;
	}

	// Reads `leaf_indices` (Parquet's flat leaf-schema indices, the convention ReadTable/
	// ReadRowGroups use -- NOT ReadColumn's top-level field index) over the LIVE row groups only,
	// or over the whole file when the statistics screen pruned nothing.
	//
	// This is the one primitive F4's I/O saving is built on, and it is the reason pruning saves
	// more than the filter columns: every whole-column read in this file goes through it, so the
	// payload columns a caller reads afterwards skip the pruned row groups too.
	//
	// The batched, thread-parallel single Arrow call survives pruning unchanged -- ReadRowGroups
	// takes a LIST of row groups, so the read is issued over the surviving subset rather than
	// becoming a per-row-group loop. Result rows stay in physical file order (the list is
	// ascending), which is what lets live_mask line up with them.
	static std::shared_ptr<arrow::Table> read_live_row_groups(
		ParquetReaderHandle *reader_handle, const std::vector<int> &leaf_indices)
	{
		arrow::Result<std::shared_ptr<arrow::Table>> table_result =
			reader_has_pruned_row_groups(reader_handle)
				? reader_handle->reader->ReadRowGroups(live_row_group_list(reader_handle), leaf_indices)
				: reader_handle->reader->ReadTable(leaf_indices);
		if (!table_result.ok())
		{ // GCOVR_EXCL_START -- file-I/O backstop, not fixture-triggerable
			throw std::runtime_error(table_result.status().ToString());
		}
		// GCOVR_EXCL_STOP
		return table_result.ValueOrDie();
	}

	// Applies a reader's row transforms to a just-decoded column array: the filter/sample mask
	// (if set -- see parquet_reader_set_filter), then the sort permutation (if set -- see
	// parquet_reader_set_sort). A no-op (returns `array` unchanged) if neither is set.
	// Called from every place a column is first decoded from disk
	// (get_single_chunk_array, parquet_reader_prefetch_columns), so every
	// column ever cached or handed back to Fortran reflects only the
	// matching rows, in the requested order, once either is in effect.
	//
	// The order of the two steps is the contract, not an implementation detail: filter FIRST, then
	// sort WITHIN the survivors. That is why sort_perm's length is the post-filter row count, and
	// why parquet_reader_set_sort must run after parquet_reader_set_filter.
	static std::shared_ptr<arrow::Array> apply_row_transform(ParquetReaderHandle *reader_handle, const std::shared_ptr<arrow::Array> &array)
	{
		if (!reader_handle->live_mask && !reader_handle->sort_perm) return array;
		ensure_compute_initialized();
		auto coerced = coerce_string_view_to_offset_string(array);
		if (!coerced.ok())
		{ // GCOVR_EXCL_START -- Cast-kernel Status backstop on already-validated input
			throw std::runtime_error(coerced.status().ToString());
		}
		// GCOVR_EXCL_STOP
		arrow::Datum current = coerced.ValueOrDie();
		if (reader_handle->live_mask)
		{
			// live_mask covers the LIVE row groups' rows: every whole-column decode goes through
			// read_live_row_groups, so `array` spans the LIVE row groups' rows rather than the
			// whole file. The two are the same object whenever nothing was pruned.
			auto filtered = arrow::compute::Filter(current, reader_handle->live_mask);
			if (!filtered.ok())
			{ // GCOVR_EXCL_START -- Filter-kernel Status backstop on already-validated input
				throw std::runtime_error(filtered.status().ToString());
			}
			// GCOVR_EXCL_STOP
			current = filtered.ValueOrDie();
		}
		if (reader_handle->sort_perm)
		{
			auto taken = arrow::compute::Take(current, arrow::Datum(reader_handle->sort_perm));
			if (!taken.ok())
			{ // GCOVR_EXCL_START -- Take-kernel Status backstop on an already-validated permutation
				throw std::runtime_error(taken.status().ToString());
			}
			// GCOVR_EXCL_STOP
			current = taken.ValueOrDie();
		}
		return current.make_array();
	}

	// The one predicate every "this cannot be done under a row transform" guard keys on. It reports
	// a SORT PERMUTATION only, never a filter/sample mask -- deliberately, and the distinction is
	// load-bearing: a mask only ever REMOVES rows, so row groups stay contiguous and chunked reads,
	// parquet_get_chunk_size and row/element mode all work under one (they are scoped to each row
	// group's surviving rows). A permutation REORDERS rows, which destroys that correspondence
	// entirely. Widening this to "any transform" would silently re-ban everything filtering
	// supports; narrowing it to nothing would silently return physically ordered rows from a sorted
	// reader. Route every new guard through this rather than testing sort_perm directly.
	static bool reader_has_sort_permutation(ParquetReaderHandle *reader_handle)
	{
		return reader_handle->sort_perm != nullptr;
	}

	// Test-only: forces the next genuine whole-column decode to abort via report_fatal_error --
	// get_single_chunk_array's own ReadColumn (below) and both batched prefetch paths' ReadTable
	// (parquet_reader_prefetch_columns / parquet_reader_prefetch_columns_by_index). A cache hit is
	// unaffected -- see get_single_chunk_array's own comment. Covering the prefetch paths too is
	// load-bearing rather than thorough: prefetching is a different Arrow call (ReadTable, not
	// ReadColumn) that reads whole columns just the same, so a hook watching only
	// get_single_chunk_array reports "no whole-column read" for a path that prefetched the whole
	// file. That gap let a real mutation survive undetected, back when the unscoped path's filter
	// columns were warmed from Fortran: removing the guard that kept that warm-up off the SCOPED
	// path made it read every filter column whole-file, and only the prefetch-side hook noticed.
	// The read now lives in parquet_reader_set_filter itself, which carries the same hook. Lets
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
	//
	// UNLIKE every other g_debug_* global in this file, this one is incremented on the ordinary,
	// always-taken cache-miss path of get_single_chunk_array -- not only from within an isolated,
	// single-process error-scenario subprocess -- so it is live and incremented by every OpenMP
	// thread reading any column, all the time, not just while a scenario is deliberately exercising
	// it. A plain (non-atomic) `int64_t` there is a genuine, ThreadSanitizer-confirmed data race
	// under this project's own concurrent test suite (two threads' unsynchronized `++` on the same
	// word): each `++` is a non-atomic read-modify-write, so concurrent increments can silently
	// lose updates. `std::atomic` is the fix, not a scoping change -- every other g_debug_* global
	// stays a plain flag/limit precisely because it is only ever touched from one isolated
	// subprocess at a time (see this file's own notes on that isolation, and CLAUDE.md's "If
	// src/parquet_wrapper.cpp is ever split into multiple translation units").
	static std::atomic<int64_t> g_debug_physical_column_read_count{0};

	// ==== Whole-column reads (caching, filter/sample application, qc dispatch) ====
	//
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

			// A whole-column read must skip the row groups the statistics screen ruled out --
			// that is where F4's payload saving comes from, and it is the whole reason this is
			// not simply ReadColumn any more.
			//
			// But ReadColumn is KEPT for the unpruned case, rather than routing everything through
			// read_live_row_groups for symmetry's sake. Measured, on a 4M-row 9-column file with a
			// filter that prunes nothing: going through ReadTable here costs 3.4% on the whole
			// filtered read (0.412 s -> 0.426 s, reproducible to 0.1% over three runs), because
			// ReadTable reconstructs a Table and its schema per call where ReadColumn hands back
			// the ChunkedArray directly. Paying that on every unfiltered and every unpruned read,
			// to tidy up an asymmetry no caller can observe, is the wrong trade -- so the default
			// path stays byte-for-byte what it was before F4.
			std::shared_ptr<arrow::ChunkedArray> chunked;
			if (reader_has_pruned_row_groups(reader_handle))
			{
				std::vector<int> leaf_indices;
				collect_leaf_indices(reader_handle->manifest.schema_fields[static_cast<size_t>(idx)], leaf_indices);
				auto table = read_live_row_groups(reader_handle, leaf_indices);
				chunked = table->column(table->schema()->GetFieldIndex(resolved.top_level_name));
			}
			else
			{
				auto status = reader_handle->reader->ReadColumn(static_cast<int>(idx), &chunked);
				if (!status.ok())
				{ // GCOVR_EXCL_START -- file-I/O backstop, not fixture-triggerable
					throw std::runtime_error(status.ToString());
				}
				// GCOVR_EXCL_STOP
			}

			array = apply_row_transform(reader_handle, combine_column_chunks(chunked, resolved.top_level_name));
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

	// ==== String-like accessors and buffer extraction ====
	//
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
	// `array`'s own offset() is not always 0: every caller reaches `array` via
	// combine_column_chunks (either directly, for a whole-column read, or through
	// get_row_group_chunk_array, for a chunked read) for a *plain* (non-struct) column, which only
	// ever returns a freshly ReadColumn/ReadRowGroup-decoded chunk or the result of
	// arrow::Concatenate -- never a genuine Slice() -- so offset() is 0 there. But for a dotted
	// struct-field path, `array` instead comes from unwrap_struct_path, whose own
	// StructArray::field() calls *can* return a genuinely sliced (nonzero-offset) child array (see
	// unwrap_struct_path's own comment) -- offset() has been observed to stay 0 in every fixture
	// exercised so far, but nothing about the type signature here guarantees that, so it is
	// reported rather than assumed. data_out/*nchars_out are always correct regardless of offset()
	// (raw_value_offsets()[0]/total_values_length() already account for it -- a single pointer add,
	// free to do regardless). The offsets values themselves still rely on offset()==0 (or,
	// equivalently, that the sliced-away leading elements contributed 0 bytes) -- append_buffers'
	// own precondition guard (offsets(1) must be 0) aborts loudly rather than silently misplacing
	// bytes if that ever fails, see its doc comment. *validity_offset_out reports the element
	// offset separately so append_buffers can correctly align the validity bitmap (whose bits are
	// never pre-rebased by Arrow, unlike the two buffers above) even when offset() is nonzero.
	static void extract_string_buffers(const std::shared_ptr<arrow::Array> &array,
		int64_t *nrows_out, int64_t *nchars_out,
		const void **offsets_out, const void **data_out, const void **validity_out,
		int8_t *offsets_int32_out, int64_t *validity_offset_out)
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
		*validity_offset_out = array->data()->offset;
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

	// Test-only override of kArrowInt32OffsetLimit for a variable-length LIST column's own offsets
	// buffer -- the list counterpart of g_debug_string_offset_limit above, and a process-global for
	// exactly the same reason. Kept SEPARATE from the string one rather than shared: the scenario
	// that forces a list column onto large_list must not also push every string column in the same
	// process onto large_utf8, or it would be testing two things and reporting one. <= 0 (the
	// default) means "use the real production limit". See effective_list_offset_limit.
	static int64_t g_debug_list_offset_limit = -1;

	// Test-only override of kArrowInt32OffsetLimit for a MAP column's own offsets buffer -- the map
	// counterpart of g_debug_list_offset_limit above, and a process-global for the same reason.
	//
	// It exists for a HARDER limit than either of those two, and the difference is the whole point:
	// a string column that will not fit an int32 offsets buffer is written as large_utf8 and a list
	// column as large_list, but ARROW HAS NO large_map -- MapArray::FromArrays' own contract
	// requires int32 offsets and no wider map type exists. So for a map the ceiling is a refusal
	// rather than a representation choice, and this override is what lets an error scenario reach
	// that refusal with a tiny fixture. <= 0 (the default) means "use the real production limit".
	static int64_t g_debug_map_offset_limit = -1;

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

	// ==== Hard int32-only Arrow limit guards (col_size, chunk_size, column count) ====
	//
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

	// One row of a LIST/LARGE_LIST array's element count, by its own offsets. A variable-length
	// list column has no col_size to reason about, so everything below has to read the data.
	//
	// Takes a raw pointer rather than a shared_ptr: max_list_row_length calls it twice per ROW, and
	// a shared_ptr parameter on a per-element helper costs two atomic refcount operations per call
	// (CLAUDE.md, "A `shared_ptr` parameter on a per-row helper"). The caller owns the array for
	// the whole walk, so there is nothing to share.
	static int64_t list_array_value_offset(const arrow::Array *array, int64_t i)
	{
		if (array->type_id() == arrow::Type::LARGE_LIST)
		{
			return static_cast<const arrow::LargeListArray *>(array)->value_offset(i);
		}
		return static_cast<const arrow::ListArray *>(array)->value_offset(i);
	}

	// The longest single row across every variable-length LIST column in `arrays` (0 if there are
	// none) -- the list column's analogue of max_fixed_size_list_col_size above, and used the same
	// way: close_parquet_writer clamps an AUTO-SIZED row group down by it, so that a row group's
	// element count cannot exceed kArrowInt32ListElementCountLimit.
	//
	// Conservative by construction (chunk_size * longest_row is an upper bound on any window's
	// element count, usually a loose one), which is the safe direction for a clamp: it can only
	// produce smaller row groups than strictly necessary, never a row group that overflows. An
	// EXPLICIT chunk_size is validated exactly instead, by check_explicit_chunk_size_fits_list_limit
	// below -- clamping is silent and must not be wrong, while aborting is loud and must not be
	// wrong EITHER WAY, so it cannot use an over-estimate.
	static int64_t max_list_row_length(const std::vector<std::shared_ptr<arrow::Array>> &arrays)
	{
		int64_t longest = 0;
		for (const auto &array : arrays)
		{
			if (!array) continue;
			auto id = array->type_id();
			if (id != arrow::Type::LIST && id != arrow::Type::LARGE_LIST) continue;
			for (int64_t i = 0; i < array->length(); ++i)
			{
				int64_t len = list_array_value_offset(array.get(), i + 1) - list_array_value_offset(array.get(), i);
				if (len > longest) longest = len;
			}
		}
		return longest;
	}

	// Aborts if any row group an EXPLICIT chunk_size would produce holds more list elements than
	// kArrowInt32ListElementCountLimit. Walks each list column's actual offsets at chunk_size
	// stride, so it is exact: a caller whose chunk_size really does fit is never refused, however
	// ragged the column. The vector-column counterpart is check_explicit_chunk_size_fits_arrow_limit,
	// which needs no data because col_size is a constant.
	static void check_explicit_chunk_size_fits_list_limit(int64_t chunk_size,
		const std::vector<std::shared_ptr<arrow::Field>> &fields,
		const std::vector<std::shared_ptr<arrow::Array>> &arrays, const char *context)
	{
		int64_t limit = g_debug_list_element_count_limit > 0 ? g_debug_list_element_count_limit : kArrowInt32ListElementCountLimit;
		for (size_t c = 0; c < arrays.size(); ++c)
		{
			const auto &array = arrays[c];
			if (!array) continue;
			auto id = array->type_id();
			if (id != arrow::Type::LIST && id != arrow::Type::LARGE_LIST) continue;
			int64_t n = array->length();
			for (int64_t lo = 0; lo < n; lo += chunk_size)
			{
				int64_t hi = std::min(lo + chunk_size, n);
				int64_t elems = list_array_value_offset(array.get(), hi) - list_array_value_offset(array.get(), lo);
				if (elems <= limit) continue;
				std::string name = c < fields.size() && fields[c] ? fields[c]->name() : std::string("(unnamed)");
				report_fatal_error(context, "column '" + name + "': chunk_size (" + std::to_string(chunk_size) + // GCOVR_EXCL_LINE
					") would put " + std::to_string(elems) + " list elements in one row group, exceeding " + // GCOVR_EXCL_LINE
					std::to_string(kArrowInt32ListElementCountLimit) + ", the maximum per-row-group element count " // GCOVR_EXCL_LINE
					"Arrow/Parquet's list-column level generation supports -- pass a smaller chunk_size to " // GCOVR_EXCL_LINE
					"parquet_open_writer, or omit it to auto-size safely"); // GCOVR_EXCL_LINE
			}
		}
	}

	// Applies BYTE_STREAM_SPLIT to every float32/float64 column in `fields`, on `builder` --
	// automatic, type-based, and not exposed as a public argument (see CLAUDE.md's "Element-domain
	// modules" / writer-compression notes for the rationale: floats are effectively always
	// near-unique, so dictionary encoding never pays off for them, and byte-stream-splitting each
	// value's bytes across separate per-position streams compresses substantially better under
	// zstd than plain floats do). Every other column type is left at the writer's normal defaults
	// (dictionary enabled).
	//
	// Parquet's WriterProperties::Builder::encoding(path, type) is only honored when dictionary
	// encoding is disabled for that same column (see parquet/properties.h) -- requesting
	// BYTE_STREAM_SPLIT while dictionary stays enabled is a silent no-op, not an error. So
	// disable_dictionary must be paired with encoding(..., BYTE_STREAM_SPLIT) for exactly the same
	// set of columns, never applied on its own.
	static void apply_float_byte_stream_split(parquet::WriterProperties::Builder &builder,
		const std::vector<std::shared_ptr<arrow::Field>> &fields)
	{
		for (const auto &field : fields)
		{
			if (!field) continue;
			auto type_id = field->type()->id();
			if (type_id != arrow::Type::FLOAT && type_id != arrow::Type::DOUBLE) continue;
			builder.disable_dictionary(field->name());
			builder.encoding(field->name(), parquet::Encoding::BYTE_STREAM_SPLIT);
		}
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
		// Names parquet_open_writer only: parquet_set_writer_options is a bind(C) binding
		// (parquet_bindings.f90), never re-exported by the parquet facade, so advising a user to
		// call it points at something they cannot reach from `use parquet`.
		const char *advice = "pass a smaller chunk_size to parquet_open_writer, or "
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
		// See check_chunk_size_fits_limit_for_col_size above for why the advice names only
		// parquet_open_writer.
		const char *advice = "pass a smaller chunk_size to parquet_open_writer, or "
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
	// The byte target itself is a SETTING (parquet_set_target_row_group_bytes) and so lives with the
	// other mirrored values near the top of this file, as g_target_row_group_bytes; the three bounds
	// below are not settable and stay here.
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
			static_cast<double>(g_target_row_group_bytes) / std::max(bytes_per_row, 1.0));

		if (rows_for_target >= kMinAutoChunkSizeRows)
		{
			return std::min<int64_t>(rows_for_target, kMaxAutoChunkSizeRows);
		}
		if (static_cast<double>(kMinAutoChunkSizeRows) * bytes_per_row
			<= static_cast<double>(g_target_row_group_bytes) * kMaxFloorOvershootFactor)
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
	// extended_real_*_int64 error scenarios, all of which end in the fatal exit -- and that exit
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
	// **Takes a RAW POINTER, not a `shared_ptr`, and that is the whole performance story of the
	// filter path.** These helpers are called once per ROW; taking the array by
	// `const std::shared_ptr<arrow::Array> &` meant every `std::static_pointer_cast` inside them
	// built a new shared_ptr, i.e. an atomic increment and an atomic decrement, to read one value.
	// Measured on a 16-column x 2 M-row file, one filter clause: 13.6 ns per row before, 1.8 ns
	// after -- a 7.6x improvement in evaluate_nodes and 2.9x-4.1x in the whole cost of installing a
	// filter, with no threading involved at all.
	//
	// So: **never widen one of these back to a `shared_ptr` parameter, and never add a new per-row
	// helper that takes one.** The caller already owns a reference for the duration of the loop;
	// the loop needs the pointer, not a share of the ownership. Nothing fails if this is undone --
	// the answers stay identical and every test still passes, which is exactly why it is written
	// down here rather than left to be rediscovered.
	static double real_family_value_at(const arrow::Array *vals, int64_t idx)
	{
		switch (vals->type_id())
		{
		case arrow::Type::FLOAT:
			return static_cast<double>(static_cast<const arrow::FloatArray *>(vals)->Value(idx));
		case arrow::Type::HALF_FLOAT:
		{
			auto arr = static_cast<const arrow::HalfFloatArray *>(vals);
			return static_cast<double>(arrow::util::Float16::FromBits(arr->Value(idx)).ToFloat());
		}
		default: // arrow::Type::DOUBLE
			return static_cast<const arrow::DoubleArray *>(vals)->Value(idx);
		}
	}

	// Returns this decimal column's declared scale (digits after the point) --
	// shared by every DECIMAL32/64/128/256 case below. DecimalType is the
	// common base every decimal width's concrete type class derives from, so
	// this one accessor works regardless of which width `vals` actually is.
	static int32_t decimal_scale_of(const arrow::Array *vals)
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
	static NumericConvertStatus decimal_to_int64_checked(const arrow::Array *vals, int64_t idx, int64_t &out)
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
			auto arr = static_cast<const arrow::Decimal32Array *>(vals);
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
			auto arr = static_cast<const arrow::Decimal64Array *>(vals);
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
			auto arr = static_cast<const arrow::Decimal128Array *>(vals);
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
			auto arr = static_cast<const arrow::Decimal256Array *>(vals);
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
	static double decimal_value_at(const arrow::Array *vals, int64_t idx)
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
			return arrow::Decimal32(static_cast<const arrow::Decimal32Array *>(vals)->GetValue(idx)).ToDouble(scale);
		case arrow::Type::DECIMAL64:
			return arrow::Decimal64(static_cast<const arrow::Decimal64Array *>(vals)->GetValue(idx)).ToDouble(scale);
		// GCOVR_EXCL_STOP
		case arrow::Type::DECIMAL128:
			return arrow::Decimal128(static_cast<const arrow::Decimal128Array *>(vals)->GetValue(idx)).ToDouble(scale);
		default: // arrow::Type::DECIMAL256
			return arrow::Decimal256(static_cast<const arrow::Decimal256Array *>(vals)->GetValue(idx)).ToDouble(scale);
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
	static int64_t small_integer_value_at(const arrow::Array *vals, int64_t idx)
	{
		switch (vals->type_id())
		{
		case arrow::Type::INT8:
			return static_cast<int64_t>(static_cast<const arrow::Int8Array *>(vals)->Value(idx));
		case arrow::Type::INT16:
			return static_cast<int64_t>(static_cast<const arrow::Int16Array *>(vals)->Value(idx));
		case arrow::Type::UINT8:
			return static_cast<int64_t>(static_cast<const arrow::UInt8Array *>(vals)->Value(idx));
		case arrow::Type::UINT16:
			return static_cast<int64_t>(static_cast<const arrow::UInt16Array *>(vals)->Value(idx));
		case arrow::Type::UINT32:
			return static_cast<int64_t>(static_cast<const arrow::UInt32Array *>(vals)->Value(idx));
		case arrow::Type::INT32:
			return static_cast<int64_t>(static_cast<const arrow::Int32Array *>(vals)->Value(idx));
		default: // arrow::Type::INT64
			return static_cast<const arrow::Int64Array *>(vals)->Value(idx);
		}
	}

	// Read-time QC (see the QcRule struct and parquet_reader_set_qc further
	// below): checks `array` (already the filtered version, if a filter is
	// set -- see apply_row_transform) against `rule`'s declared Null policy.
	// Fires (returns true, filling `out_message`) only if Nulls are found AND
	// the maml declares an explicit, EMPTY qc: miss: for this field -- the one
	// form that asks for Null validation. A field declaring qc: miss: Null/NA,
	// and a field declaring no qc: miss: at all, both allow Nulls and are never
	// reported here (see QcRule::null_values_allowed) --
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
			std::to_string(nulls) + " unexpected Null value(s) found (qc: miss: is declared empty, so Nulls are not expected here)";
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

		// Resolved once per rule rather than per element, for the reason CmpOp's own comment gives.
		const CmpOp min_cmp = cmp_op_of(rule.min_op);
		const CmpOp max_cmp = cmp_op_of(rule.max_op);
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
				if (have_min) ok = ok && compare_op<int64_t>(v, min_bound, min_cmp);
				if (have_max) ok = ok && compare_op<int64_t>(v, max_bound, max_cmp);
				if (!ok) n_violate++;
			};
			for (int64_t i = 0; i < n; ++i) if (!array->IsNull(i)) scan(small_integer_value_at(array.get(), i));
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
				if (have_min) ok = ok && compare_op<double>(v, min_bound, min_cmp);
				if (have_max) ok = ok && compare_op<double>(v, max_bound, max_cmp);
				if (!ok) n_violate++;
			};
			auto type_id = array->type_id();
			bool is_decimal = type_id == arrow::Type::DECIMAL32 || type_id == arrow::Type::DECIMAL64 ||
				type_id == arrow::Type::DECIMAL128 || type_id == arrow::Type::DECIMAL256;
			for (int64_t i = 0; i < n; ++i)
			{
				if (array->IsNull(i)) continue;
				if (is_decimal) scan(decimal_value_at(array.get(), i));
				else if (type_id == arrow::Type::UINT64) scan(static_cast<double>(std::static_pointer_cast<arrow::UInt64Array>(array)->Value(i)));
				else scan(real_family_value_at(array.get(), i));
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
			// data_min/data_max stay std::string -- they outlive the loop, and the views they
			// would otherwise hold point into an array this function does not own. Everything
			// else takes the view directly: `scan` used to be handed a std::string built from it
			// per row, i.e. an allocator round trip per element to compare a few bytes.
			std::string data_min, data_max;
			auto scan = [&](std::string_view v)
			{
				n_valid++;
				if (!any_valid) { data_min = v; data_max = v; any_valid = true; }
				else
				{
					if (v < data_min) data_min = v;
					if (v > data_max) data_max = v;
				}
				bool ok = true;
				if (rule.has_min) ok = ok && compare_op<std::string_view>(v, rule.min_raw, min_cmp);
				if (rule.has_max) ok = ok && compare_op<std::string_view>(v, rule.max_raw, max_cmp);
				if (!ok) n_violate++;
			};
			for (int64_t i = 0; i < n; ++i) if (!acc.is_null(i)) scan(acc.get_view(i));
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
				emit_warning_cpp(msg);
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
				emit_warning_cpp(msg);
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

	// The schema-only half of parquet_reader_get_column_col_size, factored out so the sibling entry
	// points below can reuse it. Deliberately NOT the exported function: calling that would
	// re-enter as_reader_handle while the caller's own guard is still held. That is merely wasteful
	// now (ConcurrencyGuard admits its own owner re-entrantly, so it costs one redundant atomic
	// pair) rather than fatal as it once was, but the factoring is still the right shape -- the same
	// reasoning parquet_reader_get_column_total_elements already records for its own helper calls.
	static int64_t parquet_reader_get_column_col_size_impl(ParquetReaderHandle *reader_handle, const char *name)
	{
		auto resolved = resolve_struct_path(reader_handle->schema, name);
		if (resolved.leaf_field->type()->id() == arrow::Type::FIXED_SIZE_LIST)
		{
			return static_cast<int64_t>(std::static_pointer_cast<arrow::FixedSizeListType>(resolved.leaf_field->type())->list_size());
		}
		return 1;
	}

	// Whether column `name` contains any Null in the 1-based inclusive row-group range
	// [rg_lo, rg_hi] (rg_lo <= 0 meaning every row group), answered from the file footer alone.
	//
	// Parquet column-chunk statistics carry a null count, so this reads NO column data. That makes
	// it worth having: every materialize in parquet_tables used to request a per-row validity mask
	// unconditionally, which costs an O(n) IsValid() scan here, an int8 buffer plus a 4-byte-per-row
	// Fortran LOGICAL mask, a conversion pass between them, and a per-row replay loop on the Fortran
	// side -- all to describe a column that, in the overwhelmingly common case, has no Nulls at all.
	// Asking first lets that entire pipeline be skipped.
	//
	// Returns 1 for "has nulls, or cannot be sure". The conservative direction matters: statistics
	// are optional in the format, so a missing chunk statistic, or one without a null count, must
	// read as "might have nulls" -- claiming otherwise would make parquet_read_column abort on a
	// column it was told was clean. A filter or sample being active is safe to ignore, because both
	// only ever REMOVE rows: a column with no Nulls in the file has none in the filtered result.
	static int column_has_nulls_from_footer(
		ParquetReaderHandle *reader_handle, const char *name, int64_t rg_lo, int64_t rg_hi)
	{
		auto resolved = resolve_struct_path(reader_handle->schema, name);
		// A struct leaf's own validity is combined with every ancestor struct's on read
		// (unwrap_struct_path), so the leaf chunk's own null count does not describe the result.
		// Rather than reason about each level's statistics, decline to answer for a nested path.
		if (!resolved.child_path.empty())
		{
			return 1;
		}
		auto idx = get_column_index(reader_handle, resolved.top_level_name.c_str());
		auto leaf_idx = resolve_single_leaf_index(reader_handle, static_cast<int>(idx), resolved.child_path);
		auto *file_metadata = reader_handle->reader->parquet_reader()->metadata().get();
		int64_t lo = rg_lo > 0 ? rg_lo : 1;
		int64_t hi = rg_hi > 0 ? rg_hi : reader_handle->num_row_groups;
		for (int64_t rg = lo; rg <= hi; ++rg)
		{
			auto chunk = file_metadata->RowGroup(static_cast<int>(rg - 1))->ColumnChunk(static_cast<int>(leaf_idx));
			if (!chunk->is_stats_set())
			{
				return 1;
			}
			// Both this and the is_stats_set() test above are load-bearing as a PAIR, and each masks
			// the other individually: removing either alone changes nothing (verified by mutation --
			// every list/null test still passes), while removing both segfaults on
			// test/fixtures/no_stats.parquet, because statistics() returns null there. Do not drop
			// one as dead code on the strength of a coverage report. HasNullCount() specifically is
			// defensive: Arrow always writes a null count when it writes statistics at all, so no
			// fixture this repository can build reaches it with stats set but no count.
			auto stats = chunk->statistics();
			// See the comment above: kept as a load-bearing defensive pair with is_stats_set()
			// (removing both segfaults on test/fixtures/no_stats.parquet), but is_stats_set()
			// already catches that fixture's own missing statistics first, so no fixture this
			// repository can build reaches `stats` non-null with HasNullCount() false, nor `stats`
			// itself null once is_stats_set() is true.
			if (!stats || !stats->HasNullCount())
			{ // GCOVR_EXCL_START
				return 1;
			}
			// GCOVR_EXCL_STOP
			if (stats->null_count() > 0)
			{
				return 1;
			}
		}
		return 0;
	}

	// Whether measuring a leaf field's col_size requires reading its data, given that the caller
	// has already handled FIXED_SIZE_LIST (whose width is a schema constant).
	//
	// Only a plain LIST/LARGE_LIST does. Arrow lets such a column carry a DIFFERENT length in
	// every row, so there is no schema-level width to read -- get_col_size has to walk the whole
	// offsets buffer to find out whether one uniform width even exists. Nothing else needs the
	// data: get_col_size's own fallthrough returns 1 for every non-list array, so for a scalar
	// column (and this is the overwhelmingly common case -- every column of an ordinary file)
	// decoding it answers a question whose answer was fixed by the schema alone.
	//
	// This predicate is what keeps parquet_open_table's classification pass schema-only.
	// parquet_table asks for col_size on EVERY column at open time to tell a scalar column from
	// a vector one -- the type name cannot distinguish them, since
	// parquet_reader_get_column_type_name unwraps a list to its value type and reports
	// "float64" for both a double and a LIST<double>. Without this test that pass decoded, and
	// immediately discarded, the entire file, which made a "lazy" open cost as much as reading
	// everything (measured: 0.163 s -> 0.001 s on a 0.4 GB 8-column file).
	//
	// STRUCT and MAP never reach here at all: collect_column_leaf_paths expands a struct into
	// one path per leaf, so a struct is never itself measured, and a MAP (like decimal, binary,
	// and every other unsupported type) fails parquet_column_exists's types= probe in
	// table_classify, which returns before asking for col_size.
	static bool needs_data_to_measure_col_size(const std::shared_ptr<arrow::DataType> &type)
	{
		return type->id() == arrow::Type::LIST || type->id() == arrow::Type::LARGE_LIST;
	}

	// Returns the vector-column element count of a fixed-size-list or list array (1 for a scalar
	// column; 0 only for an EMPTY list column).
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

	// ==== Measuring a plain LIST column's width without materializing it ====
	//
	// A FIXED_SIZE_LIST carries its width in the schema, so none of this applies to it (nor to a
	// scalar column, whose width is 1 by construction) -- see needs_data_to_measure_col_size.
	// A plain LIST/LARGE_LIST may hold a DIFFERENT length in every row, so whether one uniform
	// width exists at all is a property of the data, and the naive way to find out is to
	// materialize the whole column and compare every row. On a large column that is exactly the
	// read this library works hardest to avoid.
	//
	// Two cheaper tiers replace it:
	//
	//   1. list_width_candidate -- a screen from the file footer alone, reading NO column data.
	//      Per row group, `num_values / num_rows` is the mean elements per row. If that is not a
	//      whole number, or two row groups disagree, no single uniform width can exist and the
	//      answer is settled for free. Only when every row group agrees on the same integer does
	//      a candidate survive, and a candidate is NOT a proof: rows [3,1,3,1] average to exactly
	//      2 (verified empirically -- see the tests over test/fixtures/list_widths.parquet).
	//   2. list_width_verified -- runs the screen first, then, only for a surviving candidate,
	//      walks the covered row groups one at a time and bails at the first disagreement. Peak
	//      memory is one row group rather than the whole column.
	//
	// Both take a 1-based INCLUSIVE row-group range, with rg_lo <= 0 meaning "every row group".
	// The range is what lets a sliced parquet_table measure only the row groups it actually
	// covers; a slice's width is therefore a property of the slice's own rows, so a file that is
	// ragged overall can legitimately present a uniform width within one slice.

	// Whether every row of `array` holds the same number of elements, and if so how many.
	// A non-list array is trivially uniform with width 1. A zero-length array carries no
	// information at all and reports width -1, which callers skip rather than count as a
	// disagreement.
	static bool list_uniform_width(const std::shared_ptr<arrow::Array> &array, int64_t &width)
	{
		width = 1;
		// Defensive completeness, unreachable through the public API: list_uniform_width's only two
		// call sites (both in list_width_verified) are reached exclusively through the
		// needs_data_to_measure_col_size gate, which is true only for LIST/LARGE_LIST -- a
		// FIXED_SIZE_LIST column is always answered from its own schema-level list_size() before
		// ever reaching this function. Kept for this function's own generality (its name and doc
		// comment promise an answer for "every row of `array`", not just LIST/LARGE_LIST).
		if (array->type_id() == arrow::Type::FIXED_SIZE_LIST)
		{ // GCOVR_EXCL_START
			auto list_arr = std::static_pointer_cast<arrow::FixedSizeListArray>(array);
			width = static_cast<int64_t>(list_arr->value_length());
			return true;
		}
		// GCOVR_EXCL_STOP
		if (array->type_id() == arrow::Type::LIST || array->type_id() == arrow::Type::LARGE_LIST)
		{
			if (array->length() == 0)
			{
				width = -1;
				return true;
			}
			// A null or empty row has length 0, which disagrees with any width >= 1 -- so a LIST
			// column carrying either is reported non-uniform, matching what get_uniform_list_values
			// would have rejected on the read path anyway.
			int64_t first = -1;
			for (int64_t i = 0; i < array->length(); ++i)
			{
				int64_t len = 0;
				if (array->type_id() == arrow::Type::LIST)
				{
					len = static_cast<int64_t>(std::static_pointer_cast<arrow::ListArray>(array)->value_length(i));
				}
				else
				{
					len = std::static_pointer_cast<arrow::LargeListArray>(array)->value_length(i);
				}
				// A null row must count as length 0 either way, whether or not Arrow's own
				// ListBuilder::AppendNull already left value_length(i) at 0 for it -- reached by
				// list_widths.parquet's null_avg column under a proven (row-group-scanned) width
				// measurement (test_list_width_screen_and_proof).
				if (array->IsNull(i))
				{
					len = 0;
				}
				if (first < 0)
				{
					first = len;
				}
				else if (len != first)
				{
					width = 1;
					return false;
				}
			}
			width = first;
			return true;
		}
		// Defensive completeness, unreachable for the same reason as the FIXED_SIZE_LIST branch
		// above: every array list_uniform_width is ever called with comes from a column already
		// known to be LIST/LARGE_LIST (list_width_verified's only caller path), so this catch-all
		// for "anything else" never actually executes.
		return true; // GCOVR_EXCL_LINE
	}

	// Footer-only screen: see the section comment above. Returns a candidate uniform width, 1 when
	// no uniform width above 1 can exist, or 0 for a column with no rows at all -- which is what
	// get_col_size has always reported for an empty list column, and is preserved here rather than
	// collapsed into 1 so parquet_get_col_size's answer does not change. Reads no column data.
	static int64_t list_width_candidate(
		ParquetReaderHandle *reader_handle, const char *name, int64_t rg_lo, int64_t rg_hi)
	{
		auto resolved = resolve_struct_path(reader_handle->schema, name);
		auto idx = get_column_index(reader_handle, resolved.top_level_name.c_str());
		auto leaf_idx = resolve_single_leaf_index(reader_handle, static_cast<int>(idx), resolved.child_path);
		auto *file_metadata = reader_handle->reader->parquet_reader()->metadata().get();
		int64_t lo = rg_lo > 0 ? rg_lo : 1;
		int64_t hi = rg_hi > 0 ? rg_hi : reader_handle->num_row_groups;
		int64_t candidate = -1;
		for (int64_t rg = lo; rg <= hi; ++rg)
		{
			auto row_group = file_metadata->RowGroup(static_cast<int>(rg - 1));
			int64_t nr = row_group->num_rows();
			if (nr <= 0)
			{
				continue;
			}
			// num_values counts LEAF slots, and a null or empty list occupies exactly one slot
			// (confirmed empirically, not assumed) -- so such a row inflates the mean and the
			// column gets screened out here, which is the right answer for a vector column.
			int64_t nv = row_group->ColumnChunk(static_cast<int>(leaf_idx))->num_values();
			if (nv % nr != 0)
			{
				return 1;
			}
			int64_t w = nv / nr;
			if (candidate < 0)
			{
				candidate = w;
			}
			else if (w != candidate)
			{
				return 1;
			}
		}
		// candidate < 0 means every row group in range was empty, i.e. the column has no rows.
		return candidate >= 0 ? candidate : 0;
	}

	// One row group's leaf array, deliberately NOT via get_row_group_chunk_array: that one also
	// records the row group as read (parquet_reader_check_complete) and runs per-row-group qc,
	// neither of which a width measurement should cause -- measuring a column must not make the
	// reader think it was read, nor emit qc warnings for data the caller never asked for.
	static std::shared_ptr<arrow::Array> read_row_group_array_for_measuring(
		ParquetReaderHandle *reader_handle, const char *name, int64_t row_group)
	{
		auto resolved = resolve_struct_path(reader_handle->schema, name);
		auto idx = get_column_index(reader_handle, resolved.top_level_name.c_str());
		auto leaf_idx = resolve_single_leaf_index(reader_handle, static_cast<int>(idx), resolved.child_path);
		auto result = reader_handle->reader->ReadRowGroup(static_cast<int>(row_group - 1), {static_cast<int>(leaf_idx)});
		if (!result.ok())
		{ // GCOVR_EXCL_START -- I/O backstop: the row-group range is derived from the footer.
			throw std::runtime_error(result.status().ToString());
		}
		// GCOVR_EXCL_STOP
		std::shared_ptr<arrow::Table> table = result.ValueOrDie();
		auto array = combine_column_chunks(table->column(0), resolved.top_level_name);
		// Defensive completeness, unreachable through the public API: this function is only ever
		// called (via list_width_verified) for a LIST/LARGE_LIST leaf, and resolve_struct_path
		// itself refuses to resolve a dotted struct path onto a LIST/LARGE_LIST leaf (see its own
		// comment -- "MAP/LIST are not supported" through a struct path), so `name` can never
		// actually be a dotted path here. child_path is therefore always empty in practice.
		if (!resolved.child_path.empty())
		{ // GCOVR_EXCL_START
			array = unwrap_struct_path(array, resolved.child_path);
		} // GCOVR_EXCL_STOP
		return array;
	}

	// Screen, then prove -- see the section comment above. Returns the proven uniform width, or 1
	// when the column is not a uniform vector column. Never holds more than one row group.
	static int64_t list_width_verified(
		ParquetReaderHandle *reader_handle, const char *name, int64_t rg_lo, int64_t rg_hi)
	{
		int64_t candidate = list_width_candidate(reader_handle, name, rg_lo, rg_hi);
		if (candidate <= 1)
		{
			// 0 (no rows) is passed through untouched; 1 is already the settled answer.
			return candidate;
		}
		// With a mask active, "the rows" a width measurement is about are the surviving ones, and
		// those are what the whole-column path yields (get_single_chunk_array applies the mask).
		// Measuring per row group would answer a different question -- the width of rows the
		// caller has filtered away -- so this deliberately stays a whole-column read. A sort takes
		// the same path for the stronger reason that row groups mean nothing under a permutation.
		if (reader_handle->live_mask || reader_has_sort_permutation(reader_handle))
		{
			int64_t width = 1;
			auto array = get_single_chunk_array(reader_handle, name);
			if (!list_uniform_width(array, width) || width < 0)
			{
				return 1;
			}
			return width == candidate ? width : 1;
		}
		int64_t lo = rg_lo > 0 ? rg_lo : 1;
		int64_t hi = rg_hi > 0 ? rg_hi : reader_handle->num_row_groups;
		for (int64_t rg = lo; rg <= hi; ++rg)
		{
			auto array = read_row_group_array_for_measuring(reader_handle, name, rg);
			int64_t width = 1;
			if (!list_uniform_width(array, width))
			{
				return 1;
			}
			// Both branches below are defensive completeness, not reachable via any real Parquet
			// footer + Arrow list data: list_width_candidate already required every row group in
			// [lo, hi] to agree on nv/nr == candidate before this loop ever runs, and for a row
			// group array_uniform_width finds genuinely uniform (list_uniform_width returned true
			// above), num_values == num_rows * width necessarily -- so width cannot come out
			// negative (that needs num_rows == 0, but list_width_candidate already skips a
			// zero-row row group with `continue`, so it can never have contributed to a >1
			// candidate) or different from candidate (candidate for this exact row group was
			// already nv/nr == width). Kept for robustness against a future change to
			// list_width_candidate's own invariants.
			if (width < 0)
			{ // GCOVR_EXCL_START
				continue;
			} // GCOVR_EXCL_STOP
			if (width != candidate)
			{ // GCOVR_EXCL_START
				return 1;
			} // GCOVR_EXCL_STOP
		}
		return candidate;
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

	// ==== VOTable-style metadata XML sidecar ====
	//
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

	// Maps a parquet-fortran metadata type token (TableMetadataEntry::datatype) to the VOTable 1.4
	// datatype attribute a scalar PARAM should declare, plus the arraysize attribute it needs ("" for
	// none). An empty/unrecognised token, and every array-valued ("...[]") token, stays
	// char/arraysize="*": an array's stored text is a bracketed list, which is NOT a VOTable array
	// serialisation (those are whitespace-separated), so declaring it as one would be a false claim
	// about the value. Unrecognised falling back to char means a token added later without touching
	// this function degrades to today's output rather than emitting an invalid attribute.
	static void votable_type_attrs(const std::string &token, std::string &datatype, std::string &arraysize)
	{
		datatype = "char";
		arraysize = "*";
		if (token == "int32")
		{
			datatype = "int";
		}
		else if (token == "int64")
		{
			datatype = "long";
		}
		else if (token == "float32")
		{
			datatype = "float";
		}
		else if (token == "float64")
		{
			datatype = "double";
		}
		else if (token == "boolean")
		{
			datatype = "boolean";
		}
		else
		{
			return;
		}
		arraysize.clear();
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
			std::string param_type;
			std::string param_arraysize;
			votable_type_attrs(kv.datatype, param_type, param_arraysize);
			// VOTable 1.4 serialises a boolean as T/F, so the PARAM says T while the native
			// key-value entry this was built from still says true/false. This is the ONE place
			// the two representations differ textually, and it is deliberate -- each spelling is
			// correct in its own convention. Keep the rewrite here: applied one level up, in
			// build_file_metadata, it would break every reader of the key-value entry (this
			// library's own parquet_metadata_parse_logical included).
			std::string param_value = kv.value;
			if (kv.datatype == "boolean")
			{
				param_value = (kv.value == "true") ? "T" : "F";
			}
			xml << "<PARAM datatype=\"" << param_type << "\"";
			if (!param_arraysize.empty())
			{
				xml << " arraysize=\"" << param_arraysize << "\"";
			}
			xml << " name=\""
				// GCOVR_EXCL_START -- gcov attribution artifact under GCC: same chained-statement
				// artifact as build_votable_xml's other continuation-line exclusions above; this
				// table_metadata loop is exercised by test_metadata.f90's VOTable generation tests.
				<< xml_escape(kv.key)
				// GCOVR_EXCL_STOP
				<< "\" value=\""
				<< xml_escape(param_value)
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

	// `nullable` says whether a NULL CAN LAND in this column's values, and it is applied at
	// whichever level can actually hold one -- which differs between a scalar and a vector column:
	//
	//   scalar (col_size <= 1): the outer field itself, the only place a Null can be.
	//   vector (col_size >  1): the CHILD ("item") field. Nulls in a vector column are always
	//                           element-level; a row's vector is never itself missing, by design
	//                           (see parquet_append_*_column's valid_in handling below), so the
	//                           outer list field stays nullable=false always.
	//
	// The child field is built EXPLICITLY rather than through
	// arrow::fixed_size_list(value_type, size), whose convenience constructor hard-codes a
	// nullable=true child (arrow/type.h's FixedSizeListType(DataType) constructor) and so ignored
	// this argument entirely until now -- meaning every vector column was written with a nullable
	// child however it was produced. Two things about that hand-built child are load-bearing:
	// its name must stay "item", which is the name that same Arrow constructor supplies (renaming
	// it changes the Parquet schema's leaf paths, e.g. "vec.list.item"), and passing `nullable`
	// through is what makes a mask-free vector write produce a genuinely non-nullable element
	// field.
	//
	// SAFETY INVARIANT, and it is the one that makes this whole scheme correct rather than a
	// silent-corruption hazard: a field declared non-nullable must NEVER receive an array that
	// contains nulls. It holds by construction -- an absent is_valid mask reaches the builder as a
	// null valid_bytes, which cannot produce a null -- and the two column kinds whose nulls come
	// from somewhere other than a mask (temporal, and a parquet_string_column) are excluded from
	// the presence rule for exactly that reason (see resolve_chunk_nullability). Break it and
	// Parquet emits definition levels that disagree with the schema it wrote.
	// Restamps `array`'s own type with `field`'s, when the two differ only in metadata Arrow still
	// validates. This exists for exactly one case, and it is not optional: a FIXED_SIZE_LIST array
	// comes out of arrow::FixedSizeListBuilder carrying a type whose child field is *nullable*
	// (the builder derives its type from the value builder, which has no say in the matter), while
	// build_field may now declare that child non-nullable. arrow::Table::Validate() compares the
	// two and rejects the write with "Column data for field N ... is inconsistent with schema",
	// which is an abort at close time, far from the cause.
	//
	// Restamping is safe because the difference is pure metadata: the buffers, the child data and
	// the null counts are identical, and only the child field's `nullable` flag differs. It is
	// deliberately a no-op when the types already match, so every non-vector path pays nothing.
	// The SAFETY INVARIANT in build_field's comment is what makes it correct to restamp rather
	// than to widen the field: a non-nullable field only ever reaches here with a null-free array.
	static std::shared_ptr<arrow::Array> align_array_to_field(
		const std::shared_ptr<arrow::Field> &field,
		const std::shared_ptr<arrow::Array> &array)
	{
		if (!array || !field || array->type()->Equals(*field->type())) return array;
		auto data = array->data()->Copy();
		data->type = field->type();
		return arrow::MakeArray(data);
	}

	static std::shared_ptr<arrow::Field> build_field(
		const std::string &name,
		const std::shared_ptr<arrow::DataType> &value_type,
		int64_t col_size,
		bool nullable = false)
	{
		if (col_size > 1)
		{
			auto item = arrow::field("item", value_type, nullable);
			return arrow::field(name, arrow::fixed_size_list(item, static_cast<int32_t>(col_size)), false);
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
		  // process's gcov coverage just like the fatal exit does; tested via
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
			writer_handle->arrays[idx] = align_array_to_field(field, array);
			return;
		}

		check_column_count_fits_arrow_limit(writer_handle->fields.size(), name, "parquet_append_column");
		writer_handle->fields.push_back(field);
		writer_handle->arrays.push_back(align_array_to_field(field, array));
	}

	// Builds the flat key-value file metadata: the VOTable XML sidecar (if any columns are
	// declared), the DATE/name/version keys, every table_metadata entry, and per-column
	// unit/description/ucd/datatype keys.
	static std::shared_ptr<arrow::KeyValueMetadata> build_file_metadata(
		const std::vector<ColumnMetadata> &column_metadata,
		const std::vector<TableMetadataEntry> &table_metadata)
	{
		// One `date` for the whole function, deliberately: it reaches the `DATE` key AND the
		// VOTable sidecar's own DATE PARAM, and Arrow's store_schema() then duplicates both into
		// the base64 ARROW:schema blob -- four appearances of one value. Reading the clock twice
		// here would let a file disagree with itself across a second boundary.
		auto date = g_file_date_fixed ? g_file_date : current_utc_timestamp();

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
			// A parquet key-value pair is text-only by specification, so a typed scalar's type
			// is recorded alongside it rather than in it -- mirroring the column.<name>.<attr>
			// convention already used below. Emitted immediately after the key it describes
			// (not as a trailing block) so a raw metadata dump stays readable. The empty case
			// covers a string value and every entry the caller's own "<key>.datatype" already
			// speaks for (see parquet_open_writer's collision guard).
			if (!kv.datatype.empty())
			{
				keys.push_back(kv.key + ".datatype");
				values.push_back(kv.datatype);
			}
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

	// ==== Writer lifecycle (create/options) ====
	//
	// Claims this writer's concurrency guard for the calling thread and KEEPS it claimed after
	// returning, until a matching parquet_writer_leave. Aborts with the guard's usual diagnostic
	// if a different thread already holds it.
	//
	// **This exists because the C++ guard alone was claimed far too late to keep its own promise.**
	// A parquet_write_column call spends its whole first half in Fortran, mutating writer state
	// that no C++ guard can see -- above all writer%written_names, which parquet_write.f90's
	// parquet_check_and_mark_written_name grows with allocate/copy/move_alloc on every call. Two
	// threads sharing a writer therefore raced on a Fortran allocatable and corrupted the heap
	// BEFORE either reached the guarded append at the end, so the documented diagnostic lost a race
	// it was supposed to win: measured on a 384-core machine, every single run of
	// test/error_scenarios.f90's concurrent_calls_into_shared_writer segfaulted, and the intended
	// message survived to stderr only some of the time. The read path never had this problem --
	// its first statement after the open check is already a guarded C++ call (see
	// check_column_exists) and nothing on the Fortran side mutates a reader.
	//
	// The Fortran half now claims the guard for the whole entry point via writer_lock
	// (parquet_core.f90), whose FINAL releases it on every exit path including an early RETURN.
	// A NULL handle is ignored so the Fortran side does not have to special-case an unopened
	// writer; check_writer_open has already rejected that case with a better message anyway.
	void parquet_writer_enter(void *handle)
	{
		if (handle == nullptr) return; // GCOVR_EXCL_LINE -- check_writer_open rejects this first
		ConcurrencyGuard<ParquetWriterHandle> guard(static_cast<ParquetWriterHandle *>(handle), "parquet_writer");
		guard.release(); // ownership now belongs to the Fortran-side writer_lock, not to this scope
	}

	// Drops one level of the ownership parquet_writer_enter claimed. Idempotent on an unclaimed
	// handle: a writer_lock that was never claimed (or was already released) leaves the guard
	// exactly as it found it, which is what lets writer_lock's FINAL run unconditionally.
	void parquet_writer_leave(void *handle)
	{
		if (handle == nullptr) return; // GCOVR_EXCL_LINE -- writer_lock never releases a null handle
		ConcurrencyGuard<ParquetWriterHandle>::guard_leave(static_cast<ParquetWriterHandle *>(handle));
	}

	// Creates filename and returns an opaque handle to a new parquet writer for it.
	void *create_parquet_writer(const char *filename)
	{
		// See ensure_arrow_type_singletons_initialized's own comment: this is one of the two
		// entry points (with create_parquet_reader) any OpenMP thread can reach FIRST, so this is
		// where the one-time, single-threaded warm-up has to happen.
		ensure_arrow_type_singletons_initialized();
		auto *handle = new ParquetWriterHandle{};
		auto result = arrow::io::FileOutputStream::Open(filename);
		if (!result.ok())
		{
			delete handle; // GCOVR_EXCL_LINE -- runs in open_writer_bad_path, but the report_fatal_error()
			                // below takes the fatal exit, which discards that whole process's gcov data,
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
	// Test-only: the resolved use_threads value the most recently opened reader or writer was
	// given. parquet_set_default_use_threads' effect is otherwise unobservable -- the value
	// disappears into a handle with no getter -- so without this the setting could be stored and
	// never acted on while passing every set/get test (feature_risks.md Risk-41).
	static int g_debug_last_use_threads = -1;

	int parquet_debug_get_last_use_threads(void)
	{
		return g_debug_last_use_threads;
	}

	// Test-only: how many threads the last parquet_table parallel prefetch was given. Written from
	// Fortran (parquet_debug_note_prefetch_threads, src/parquet_tables_read.f90) because the number
	// is Fortran-side state; kept here rather than in a public Fortran procedure so the hook stays
	// out of the library's own interface.
	static int64_t g_debug_prefetch_threads_used = 0;

	void parquet_debug_set_prefetch_threads_used(int64_t n)
	{
		g_debug_prefetch_threads_used = n;
	}

	int64_t parquet_debug_get_prefetch_threads_used(void)
	{
		return g_debug_prefetch_threads_used;
	}

	// Maintainer-only phase timers for parquet_reader_set_filter, in nanoseconds, accumulated since
	// the last reset. Three phases, because "the filter is expensive" is not actionable until it is
	// known WHICH part is:
	//
	//   decode  -- reading the filter's key columns (read_live_row_groups, or the per-row-group
	//              reads on the scoped path). Already batched into one Arrow call with use_threads
	//              enabled, so improving it means going around Arrow rather than through it.
	//   eval    -- evaluate_nodes: one Kleene value per row per clause, plus the and/or/not folds.
	//              This library's own code, O(rows x clauses), and independent per row.
	//   mask    -- the final walk that collapses unknown to false, applies the row range, folds in
	//              the sample draw and writes the BooleanArray.
	//
	// Not part of the library's behaviour in any way: nothing reads these but a benchmark, and they
	// are plain non-atomic int64 because set_filter runs once per reader open, on one thread.
	static int64_t g_debug_filter_decode_nanos = 0;
	static int64_t g_debug_filter_eval_nanos = 0;
	static int64_t g_debug_filter_mask_nanos = 0;

	void parquet_debug_reset_filter_phase_nanos(void)
	{
		g_debug_filter_decode_nanos = 0;
		g_debug_filter_eval_nanos = 0;
		g_debug_filter_mask_nanos = 0;
	}

	int64_t parquet_debug_get_filter_decode_nanos(void) { return g_debug_filter_decode_nanos; }
	int64_t parquet_debug_get_filter_eval_nanos(void) { return g_debug_filter_eval_nanos; }
	int64_t parquet_debug_get_filter_mask_nanos(void) { return g_debug_filter_mask_nanos; }

	// The same idea for the READ-TIME SORT -- parquet_open_reader(..., sort_by=) and
	// parquet_reader_set_sort. Added for feature_sort.md's P13, whose whole first step is that the
	// post-R1 phase shares had been DERIVED from a pre-R1 measurement rather than measured.
	//
	// The phases, in the order one sort walks them, and why each is separate:
	//
	//   info  -- parquet_reader_sort_key_info: binds the key to report its family and size to
	//            Fortran, which then allocates the buffers. Separate from `bind` below because
	//            bind_one_sort_key runs in BOTH, so this counter is what makes the second run
	//            visible instead of hiding inside one total.
	//   bind  -- the same reduction again inside parquet_reader_sort_key_fetch. Arrow's own decode
	//            is shared (get_single_chunk_array caches), so what repeats is sort_bind_arrow_key's
	//            O(rows) materialisation -- for a string key, a vector<string> of one string a row.
	//   copy  -- the memcpy/offset walk handing that reduction to Fortran-owned buffers.
	//   perm  -- building the arrow::Int64Array from the permutation Fortran computed.
	//   take  -- what parquet_reader_sort_install does with column_cache once the permutation
	//            exists. This is the phase P13 was about, and it is now a RELEASE on every path
	//            except an open that also prefetches: the entries are the sort's own key columns,
	//            and dropping them leaves a later read to decode them again through
	//            apply_row_transform. The counter is kept, and reading ~0 is the point -- it is
	//            what shows the Take is gone, and what would show it coming back.
	//
	// take_columns counts Take calls, released_columns the entries dropped instead, and they are
	// mutually exclusive per install. Both are reported because only the pair distinguishes "the
	// install ran and released the key" from "the install never ran at all" -- a lone
	// take_columns == 0 passes just as happily against a reader that was never sorted.
	//
	// Not part of the library's behaviour: nothing reads these but a benchmark, and they are plain
	// non-atomic int64 because a sort installs once per reader open, on one thread.
	static int64_t g_debug_sort_info_nanos = 0;
	static int64_t g_debug_sort_bind_nanos = 0;
	static int64_t g_debug_sort_copy_nanos = 0;
	static int64_t g_debug_sort_perm_nanos = 0;
	static int64_t g_debug_sort_take_nanos = 0;
	static int64_t g_debug_sort_take_columns = 0;
	static int64_t g_debug_sort_released_columns = 0;

	void parquet_debug_reset_sort_phase_nanos(void)
	{
		g_debug_sort_info_nanos = 0;
		g_debug_sort_bind_nanos = 0;
		g_debug_sort_copy_nanos = 0;
		g_debug_sort_perm_nanos = 0;
		g_debug_sort_take_nanos = 0;
		g_debug_sort_take_columns = 0;
		g_debug_sort_released_columns = 0;
	}

	int64_t parquet_debug_get_sort_info_nanos(void) { return g_debug_sort_info_nanos; }
	int64_t parquet_debug_get_sort_bind_nanos(void) { return g_debug_sort_bind_nanos; }
	int64_t parquet_debug_get_sort_copy_nanos(void) { return g_debug_sort_copy_nanos; }
	int64_t parquet_debug_get_sort_perm_nanos(void) { return g_debug_sort_perm_nanos; }
	int64_t parquet_debug_get_sort_take_nanos(void) { return g_debug_sort_take_nanos; }
	int64_t parquet_debug_get_sort_take_columns(void) { return g_debug_sort_take_columns; }
	int64_t parquet_debug_get_sort_released_columns(void) { return g_debug_sort_released_columns; }

	// Same idea, for the fixed-width space-padded string read (parquet_read_string_column). Two
	// phases, because the only question anyone asks about that path is whether its per-element work
	// is worth optimising, and that cannot be answered without knowing what share of the read it is:
	//
	//   decode -- get_single_chunk_array: Arrow reads and materialises the column. Not this
	//             library's code, and not reachable by any change to the accessor.
	//   copy   -- the per-row loop: one StringLikeAccessor::get_view (a std::function, so an
	//             indirect call that cannot inline) plus copy_string_with_padding.
	//
	// The `copy` share is the CEILING on replacing those std::function members with a
	// dispatch-once/templated visitor: even driving the indirect call to zero cannot beat it.
	// Nothing in the library reads these; plain non-atomic int64, as above.
	//
	// THAT CEILING HAS BEEN MEASURED, AND THE ANSWER WAS "NOT WORTH IT" -- do not re-derive it.
	// On machine A, 2M rows x character(len=24): decode 29.6 ms, copy loop 13.3 ms, whole read
	// 43.0 ms, so the ceiling is 31%. But an A/B running the SAME loop with a direct
	// arrow::StringArray::GetView instead of the std::function put it at 11.5 ms against 13.3
	// (stable to +-0.02 ms over four runs) -- so the indirect call is only 1.8 ms of it, i.e.
	// **4.2% of the read**, and the other 11.5 ms is copy_string_with_padding's memcpy and blank
	// fill, which no accessor change touches. That is below this project's ~5% keep-or-drop line,
	// so A.4's second half was dropped rather than implemented. See feature_optimise_A7.md's S7-7
	// outcome. Re-measure before reopening it; do not re-open it on the strength of the 31%.
	static int64_t g_debug_string_read_decode_nanos = 0;
	static int64_t g_debug_string_read_copy_nanos = 0;

	void parquet_debug_reset_string_read_phase_nanos(void)
	{
		g_debug_string_read_decode_nanos = 0;
		g_debug_string_read_copy_nanos = 0;
	}

	int64_t parquet_debug_get_string_read_decode_nanos(void) { return g_debug_string_read_decode_nanos; }
	int64_t parquet_debug_get_string_read_copy_nanos(void) { return g_debug_string_read_copy_nanos; }

	// Test-only: how many threads the last parquet_table SINGLE-COLUMN read was spread across, by
	// row group (materialize_column_parallel, src/parquet_tables_read.f90); 0 when that read took
	// the ordinary whole-column path. A separate counter from the prefetch one above on purpose --
	// the two paths are alternatives, so one counter could never say which of them ran.
	static int64_t g_debug_colread_threads_used = 0;

	void parquet_debug_set_colread_threads_used(int64_t n)
	{
		g_debug_colread_threads_used = n;
	}

	int64_t parquet_debug_get_colread_threads_used(void)
	{
		return g_debug_colread_threads_used;
	}

	// Test-only override of the work floor that same read is gated on, in elements (rows * width);
	// <= 0 restores the real one. Same reason as the colwork pair below: no fixture a test suite can
	// afford reaches a floor set where the parallel path starts paying for itself, so the only way
	// to exercise both sides of it is to move the floor rather than the input.
	static int64_t g_debug_colread_min_elements = -1;

	void parquet_debug_set_colread_min_elements(int64_t n)
	{
		g_debug_colread_min_elements = (n > 0) ? n : -1;
	}

	int64_t parquet_debug_get_colread_min_elements(void)
	{
		return g_debug_colread_min_elements;
	}

	// Test-only: how many threads the last parquet_table row-structural mutation (%sort_by,
	// %filter_rows, %top_n) resolved to. Written from Fortran (parquet_debug_note_table_threads,
	// src/parquet_tables_parallel.f90) for the same reason the prefetch counter above is: the number
	// is Fortran-side state, and keeping the hook here rather than in a public Fortran procedure
	// keeps it out of the library's own interface. Always written, including the value 1 for a
	// mutation that ran serially -- a gate that silently declines is exactly what the negative
	// control in test/test_settings.f90 exists to catch, and it needs to see the 1.
	static int64_t g_debug_table_threads_used = 0;

	void parquet_debug_set_table_threads_used(int64_t n)
	{
		g_debug_table_threads_used = n;
	}

	int64_t parquet_debug_get_table_threads_used(void)
	{
		return g_debug_table_threads_used;
	}

	// Test-only overrides of the two constants colwork_threads (src/parquet_tables_parallel.f90) gates
	// a row-structural mutation on: the work floor in elements, and the minimum number of mutable
	// columns. <= 0 restores the real one. Unlike the two counters above -- which Fortran only writes
	// -- these are written from a test and READ from Fortran, which is why each has a getter.
	//
	// **They exist because neither constant can be tuned by a benchmark.** Every table size a
	// benchmark can afford sits orders of magnitude above the work floor, so no sweep over rows or
	// columns ever visits its break-even; the only way to find it is to move the constant instead of
	// the input. That is the mirror image of parquet_debug_set_sort_merge_min_segment's problem (a
	// constant no test-sized input can cross) and takes the same shape for the same reason. See
	// feature_table_parallel.md section 14.8 and section 17.1.
	//
	// Kept here rather than as public Fortran procedures for the reason CLAUDE.md gives: a
	// Fortran-side hook would have to be public, and visible to every `use parquet`.
	static int64_t g_debug_colwork_min_elements = -1;
	static int64_t g_debug_colwork_min_columns = -1;

	void parquet_debug_set_colwork_min_elements(int64_t n)
	{
		g_debug_colwork_min_elements = (n > 0) ? n : -1;
	}

	int64_t parquet_debug_get_colwork_min_elements(void)
	{
		return g_debug_colwork_min_elements;
	}

	void parquet_debug_set_colwork_min_columns(int64_t n)
	{
		g_debug_colwork_min_columns = (n > 0) ? n : -1;
	}

	int64_t parquet_debug_get_colwork_min_columns(void)
	{
		return g_debug_colwork_min_columns;
	}

	void parquet_set_writer_options(void *handle, const char *compression_name, int compression_level, int64_t chunk_size, int use_threads)
	{
		g_debug_last_use_threads = use_threads;
		auto writer_handle = as_handle(handle);
		writer_handle->compression_codec = parse_compression_name(compression_name);
		writer_handle->compression_level = compression_level;
		writer_handle->chunk_size = chunk_size;
		writer_handle->use_threads = (use_threads != 0);
	}

	// Declares `name` (the column's OUTPUT name, i.e. what reaches the file) as protected on this
	// writer. Pushed once per protected column by parquet_open_writer, before any write, from the
	// schema's own is_protected flags -- extra: protected_cols: or schema%set_protected.
	//
	// A protected column may hold no Null at all, which parquet_check_protected enforces
	// Fortran-side with a message naming the column. This side needs to know only so that the
	// column's Arrow field can be built NON-nullable: for the mask-carrying kinds Fortran already
	// erases an all-.true. mask, but a temporal column and a parquet_string_column carry their
	// null state inside the element with no mask to erase, so protection is the only signal that
	// can make their fields non-nullable. See resolve_chunk_nullability.
	void parquet_writer_set_protected_column(void *handle, const char *name)
	{
		auto writer_handle = as_handle(handle);
		writer_handle->protected_columns.insert(std::string(name));
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
			// Dead: parquet_settings.f90's parquet_set_max_threads already does the identical
			// "n < 1" check itself (error stop "parquet_set_max_threads: n must be >= 1") before
			// ever calling into C, so this throw has no reachable caller through the public API.
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

	// Reports Arrow's current global CPU thread-pool capacity -- the counterpart to
	// parquet_set_max_threads above, exposed as parquet_settings.f90's parquet_get_max_threads.
	// arrow::GetCpuThreadPoolCapacity() returns a plain int with no Status, so there is no failure
	// path to report and nothing here can abort.
	int parquet_get_max_threads(void)
	{
		return arrow::GetCpuThreadPoolCapacity();
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

	// ==== Sort engine: the REFERENCE implementation (std::sort comparator + counting fast path) ====
	//
	// **Nothing in the shipped library sorts with this engine any more, and that is not a reason to
	// delete it.** Every ordering a user can reach -- `parquet_open_reader(..., sort_by=)`,
	// `parquet_reader_set_sort`, `parquet_table%sort_by`, and every `pf_sort`/`pf_argsort` call --
	// is produced by the Fortran radix engine in `src/parquet_argsort_engine.f90`. What this code
	// is now is the **independent oracle that engine is tested against**: `pf_sort_keys`' seven
	// operations (argsort, partial argsort, nth element, is_sorted, build_runs, search, merge) each
	// keep a branch selecting it, reached only through the test-only
	// `parquet_debug_use_fortran_sort_engine(.false.)`, and roughly thirty test call sites across
	// test_sort.f90 / test_sorting.f90 / test_diagnostics.f90 / test_settings.f90 compare the two
	// answer for answer.
	//
	// That is worth more than the lines cost. A radix sort and a comparator sort share no code and
	// fail in different ways, so an A/B between them catches a class of defect no single-engine
	// test can: a comparator that is subtly wrong about a tie, a null tier, or a NaN would have to
	// be wrong *identically* in both to survive. **Deleting this engine would silently convert
	// those thirty-odd tests from "two implementations agree" into "one implementation is
	// self-consistent"** -- with every one of them still passing on the day it happened, which is
	// exactly the shape of regression this project's own testing rules exist to refuse.
	//
	// Two consequences for anyone maintaining it. It must keep answering *correctly*, so a change
	// here is as load-bearing as a change to the shipped engine even though no user reaches it --
	// and it does NOT need to keep being fast, so nothing below this banner should be optimised
	// again, and no performance claim about the library should be measured on it.
	//
	// Deliberately self-contained: the core below knows nothing about ParquetReaderHandle, reads no
	// reader state, and receives its keys as plain typed vectors. Do not reach for reader state
	// from anything below this banner -- that independence is what lets it serve as an oracle for
	// a Fortran-side sort whose Arrow buffers are long gone.
	//
	// Ordering semantics reproduce arrow::compute::SortIndices EXACTLY. This is deliberate, not
	// incidental: it is what anyone cross-checking against pyarrow will see, and it was verified
	// against Arrow over 5,000,000-row fixtures of every key family (identical permutations in all
	// of them -- ties, nulls and descending included) plus a dedicated NaN/null-tier conformance
	// check. The rule, from arrow/compute/ordering.h and confirmed empirically:
	//
	//   * Null/NaN placement is ABSOLUTE -- `descending` reverses the VALUES, never the tiers.
	//   * nulls last (the default): values ... NaNs ... nulls
	//   * nulls first:              nulls ... NaNs ... values
	//   * Within any tier, ties keep their original row order.
	//
	// Stability comes from the comparator's final tiebreaker on the row index itself, so plain
	// std::sort is enough and std::stable_sort's temporary buffer is never allocated.
	//
	// Measured against Arrow at 5M rows: faster on float64 (0.64x), high-cardinality int64 (0.66x)
	// and multi-key (0.77x); 1.24x slower on strings; and -- with the counting fast path below --
	// within 3 ms on low-cardinality integers, where the plain comparator was 86x slower.

	// `enum class SortValueKind`, `struct SortKeyData` and `sort_key_finalize` USED TO BE DECLARED
	// HERE, next to the comparator that reads them. They now sit just above `ParquetReaderHandle`,
	// because the handle holds a `SortKeyData` by value (see `sort_key_cache` there) and a member of
	// incomplete type is not a thing C++ allows. It is a pure relocation -- no line of any of the
	// three changed -- and it is recorded rather than silently done because the comparator below is
	// where a reader looks for them.

	// Output tier of row `i` under this key: 0 sorts first, 2 last. Absolute -- `descending` never
	// reaches this, which is exactly Arrow's rule (a descending sort still puts nulls last by
	// default, it does not flip them to the front).
	static inline int sort_tier_of(const SortKeyData &key, int64_t i)
	{
		bool is_null = !key.valid.empty() && key.valid[static_cast<size_t>(i)] == 0;
		bool is_nan = !is_null && key.kind == SortValueKind::Real && std::isnan(key.reals_ptr[static_cast<size_t>(i)]);
		if (key.nulls_first) return is_null ? 0 : (is_nan ? 1 : 2);
		return is_null ? 2 : (is_nan ? 1 : 0);
	}

	// -1/0/+1 for rows a and b under one key, with the key's own order and null placement applied.
	// Two rows in the same non-value tier (both null, or both NaN) compare equal, so the caller's
	// index tiebreaker keeps them in file order -- again matching Arrow.
	static inline int sort_compare_key(const SortKeyData &key, int64_t a, int64_t b)
	{
		int ta = sort_tier_of(key, a);
		int tb = sort_tier_of(key, b);
		if (ta != tb) return ta < tb ? -1 : 1;
		if (ta != (key.nulls_first ? 2 : 0)) return 0;
		int c = 0;
		if (key.kind == SortValueKind::Integer)
		{
			int64_t va = key.ints_ptr[static_cast<size_t>(a)], vb = key.ints_ptr[static_cast<size_t>(b)];
			c = (va < vb) ? -1 : (va > vb) ? 1 : 0;
		}
		else if (key.kind == SortValueKind::Real)
		{
			double va = key.reals_ptr[static_cast<size_t>(a)], vb = key.reals_ptr[static_cast<size_t>(b)];
			c = (va < vb) ? -1 : (va > vb) ? 1 : 0;
		}
		else
		{
			int raw = key.strs[static_cast<size_t>(a)].compare(key.strs[static_cast<size_t>(b)]);
			c = (raw < 0) ? -1 : (raw > 0) ? 1 : 0;
		}
		return key.descending ? -c : c;
	}

	// True when the single-key integer case can be counting-sorted, filling lo/hi with the key's
	// value range over its VALID rows.
	//
	// A null-bearing key is accepted. It used to be declined, and that one clause cost the entire
	// fast path to a single null anywhere in the column -- measured as a 4.6-5.6x cliff at 0.1%
	// null density on a 4M-row int32 column, i.e. a step function of WHETHER a null exists rather
	// than of how many. What makes nulls tractable is that they are a TIER in this engine, never a
	// value (see sort_tier_of): they never interleave with values, so they form one contiguous
	// block that the permutation can place directly.
	//
	// THE RANGE SCAN MUST SKIP NULLS, and that is not an optimisation. A null row's key slot holds
	// whatever the buffer happened to contain -- Arrow promises nothing there -- so including it
	// can widen the range past g_sort_counting_bucket_limit and decline the fast path for no
	// reason, or admit a bucket domain sized from garbage.
	static bool sort_counting_candidate(const std::vector<SortKeyData> &keys, int64_t n, int64_t &lo, int64_t &hi)
	{
		if (keys.size() != 1 || n < 2) return false;
		const SortKeyData &key = keys[0];
		if (key.kind != SortValueKind::Integer) return false;
		const bool has_nulls = !key.valid.empty();
		bool seen = false;
		lo = 0;
		hi = 0;
		for (int64_t i = 0; i < n; ++i)
		{
			if (has_nulls && key.valid[static_cast<size_t>(i)] == 0) continue;
			int64_t v = key.ints_ptr[static_cast<size_t>(i)];
			if (!seen)
			{
				lo = v;
				hi = v;
				seen = true;
			}
			else
			{
				if (v < lo) lo = v;
				if (v > hi) hi = v;
			}
		}
		// Every row null: there is no range to bound, and the answer is file order because all
		// nulls tie. lo == hi == 0 leaves one empty bucket, and the permutation below then emits
		// the identity -- which is correct, and cheaper than letting the comparator path discover
		// the same thing through n log n comparisons that all return 0.
		if (!seen) return true;
		// Unsigned subtraction, so a range spanning both signs cannot overflow the check itself.
		uint64_t range = static_cast<uint64_t>(hi) - static_cast<uint64_t>(lo);
		return range < static_cast<uint64_t>(g_sort_counting_bucket_limit);
	}

	// The fast path Arrow also takes for small-range integers, and the reason a low-cardinality
	// integer key does not cost 86x what Arrow charges. One counting pass and one placement pass,
	// both O(n); stable by construction, because the placement pass walks the input in index order
	// and so emits equal values in their original order -- the same result the comparator path's
	// index tiebreaker produces.
	//
	// NULLS ARE A BLOCK, NOT A BUCKET. They are a tier in this engine (sort_tier_of), so they never
	// interleave with values; the permutation places them as one contiguous run and counting-sorts
	// the values into what is left. Four properties have to survive, each of them already true of
	// the comparator path and each independently breakable here:
	//
	//   1. The null block sits at ONE END -- last by default, first under `nulls_first`.
	//   2. `descending` NEVER MOVES IT. sort_compare_key applies the tier test before the
	//      descending negation, so a descending sort still puts nulls last. This is Arrow's own
	//      rule and is the single most likely thing to get wrong here, because it is invisible to
	//      any ascending test. Note below that value_base/null_base do not mention `descending`.
	//   3. Nulls hold FILE ORDER among themselves -- they tie, so they are emitted in increasing
	//      row index, which the single forward pass gives for free.
	//   4. The value block keeps its existing behaviour exactly, including descending and
	//      stability. With no nulls, nn is 0, value_base is 0 and this is the original code.
	static std::vector<int64_t> sort_counting_permutation(const SortKeyData &key, int64_t n, int64_t lo, int64_t hi)
	{
		size_t nbuckets = static_cast<size_t>(static_cast<uint64_t>(hi) - static_cast<uint64_t>(lo)) + 1;
		std::vector<int64_t> counts(nbuckets, 0);
		const bool has_nulls = !key.valid.empty();
		int64_t nn = 0;
		for (int64_t i = 0; i < n; ++i)
		{
			if (has_nulls && key.valid[static_cast<size_t>(i)] == 0)
			{
				++nn;
				continue;
			}
			++counts[static_cast<size_t>(static_cast<uint64_t>(key.ints_ptr[static_cast<size_t>(i)]) - static_cast<uint64_t>(lo))];
		}
		// Where each block starts. Deliberately free of `descending` -- see property 2 above.
		const int64_t value_base = (has_nulls && key.nulls_first) ? nn : 0;
		int64_t null_pos = (has_nulls && key.nulls_first) ? 0 : (n - nn);
		// Turn counts into each bucket's first output offset: bottom-up ascending, top-down
		// descending (so the largest value lands at the front of the VALUE BLOCK while keeping
		// ties in file order).
		std::vector<int64_t> offsets(nbuckets, 0);
		int64_t running = value_base;
		if (key.descending)
		{
			for (size_t b = nbuckets; b-- > 0;)
			{
				offsets[b] = running;
				running += counts[b];
			}
		}
		else
		{
			for (size_t b = 0; b < nbuckets; ++b)
			{
				offsets[b] = running;
				running += counts[b];
			}
		}
		std::vector<int64_t> perm(static_cast<size_t>(n));
		for (int64_t i = 0; i < n; ++i)
		{
			if (has_nulls && key.valid[static_cast<size_t>(i)] == 0)
			{
				perm[static_cast<size_t>(null_pos++)] = i;
				continue;
			}
			size_t b = static_cast<size_t>(static_cast<uint64_t>(key.ints_ptr[static_cast<size_t>(i)]) - static_cast<uint64_t>(lo));
			perm[static_cast<size_t>(offsets[b]++)] = i;
		}
		return perm;
	}

	// Whether the counting fast path may be taken at all is a SETTING
	// (parquet_set_sort_counting_path), read as g_sort_counting_path in the four guards below.
	// Turning it off is how a test proves the two paths agree on the same fixture rather than
	// trusting that they do.

	// Test-only: counts calls to SortRowLess, so a test can prove a partial sort really is partial.
	// Returning the first n of a FULL sort is correct and only slower, so every correctness test
	// passes against both -- a comparison count is what distinguishes them, and unlike a wall-clock
	// benchmark it is deterministic and needs no warm-up. Read via parquet_debug_get_sort_comparisons.
	//
	// NOT atomic, and deliberately so: an atomic increment in the engine's hottest loop would cost
	// more than the feature is worth. It is therefore only meaningful single-threaded, which is why
	// the `sorting` suite is excluded from test-drive's per-test parallelism (run_tester.f90).
	static bool g_debug_count_sort_comparisons = false;
	static int64_t g_debug_sort_comparison_count = 0;

	// THE comparator. Every ordering entry point in this file routes through this one object --
	// full sort, partial sort, nth_element -- which is what feature_risks.md Risk-34 requires: three
	// public entry points agree on null placement, NaN placement and tie order only because none of
	// them owns a comparison of its own.
	//
	// The final `a < b` on the row index is what makes this a TOTAL order, and two separate
	// contracts rest on it. Stability: a full tie falls back to original file order, so plain
	// std::sort is stable here and std::stable_sort's temporary buffer is never allocated.
	// Determinism of nth_element: std::nth_element normally leaves an ARBITRARY element of an
	// equal-comparing run at the nth position, but under a total order no two elements compare
	// equal, so it lands on exactly the element a full stable sort would put there -- which is the
	// index pf_nth_element is documented to report. Remove this line and both break silently.
	struct SortRowLess
	{
		const std::vector<SortKeyData> *keys; //!< Borrowed; must outlive the sort.

		bool operator()(int64_t a, int64_t b) const
		{
			// One predictable branch on a global that is false in every non-test process. Measured
			// as noise against sort_compare_key's own tier arithmetic on the same call.
			if (g_debug_count_sort_comparisons) ++g_debug_sort_comparison_count;
			for (const auto &key : *keys)
			{
				int c = sort_compare_key(key, a, b);
				if (c != 0) return c < 0;
			}
			return a < b;
		}
	};

	// The same ordering as SortRowLess, as a THREE-WAY answer and WITHOUT its index tiebreaker --
	// which is what every operation that has to recognize "these two rows are equal" needs, since
	// under the tiebreaker no two rows ever are. Binary search, run detection (pf_unique/pf_rank),
	// merging and is_sorted all key on that distinction; sorting is the only caller that must not.
	//
	// Keep this beside SortRowLess. The two are one decision expressed twice, and feature_risks.md
	// Risk-34 is about them never drifting apart: both walk the keys in precedence order and both
	// delegate every actual comparison to sort_compare_key.
	// `nkeys` is how many LEADING keys take part. Every caller but the run detection below passes
	// keys.size(); that one passes a prefix, because grouping and ordering are different questions
	// -- "sort by field then magnitude, but group by field alone" needs every key to order and only
	// the first to decide where a group ends. Fortran resolves the count (a caller's key is not
	// always one engine key) and passes it already resolved, so there is nothing to interpret here.
	static inline int sort_keys_compare(const std::vector<SortKeyData> &keys, int64_t a, int64_t b,
		size_t nkeys)
	{
		if (nkeys > keys.size()) nkeys = keys.size();
		for (size_t k = 0; k < nkeys; ++k)
		{
			int c = sort_compare_key(keys[k], a, b);
			if (c != 0) return c;
		}
		return 0;
	}

	// Once per build, not per comparison: turns a forgotten sort_key_finalize() into a named abort
	// rather than a null dereference somewhere inside std::sort's comparator. Only the pointer the
	// key's own kind reads is checked, since the other is legitimately null for a borrowed key.
	// n == 0 is exempt: nothing is ever dereferenced, and an empty owned vector may report
	// data() == nullptr.
	static void sort_check_keys_finalized(const std::vector<SortKeyData> &keys, int64_t n, const char *who)
	{
		if (n <= 0) return;
		for (const auto &key : keys)
		{
			bool ok = (key.kind == SortValueKind::Integer)  ? key.ints_ptr != nullptr
				: (key.kind == SortValueKind::Real) ? key.reals_ptr != nullptr
				: true;
			if (!ok)
			{
				report_fatal_error(who,
					"internal error: a sort key was used without being finalized"); // GCOVR_EXCL_LINE
			}
		}
	}

	// The plain comparison sort, factored out so the parallel builder below can fall back to it
	// without re-running sort_counting_candidate's O(n) range scan a second time.
	static std::vector<int64_t> sort_comparison_permutation(const std::vector<SortKeyData> &keys, int64_t n)
	{
		std::vector<int64_t> perm(static_cast<size_t>(n));
		std::iota(perm.begin(), perm.end(), static_cast<int64_t>(0));
		std::sort(perm.begin(), perm.end(), SortRowLess{&keys});
		return perm;
	}

	// The engine's entry point: 0-based permutation of [0, n) putting the rows in key order.
	static std::vector<int64_t> sort_build_permutation(const std::vector<SortKeyData> &keys, int64_t n)
	{
		sort_check_keys_finalized(keys, n, "sort_build_permutation");
		int64_t lo = 0, hi = 0;
		if (g_sort_counting_path && sort_counting_candidate(keys, n, lo, hi))
		{
			return sort_counting_permutation(keys[0], n, lo, hi);
		}
		return sort_comparison_permutation(keys, n);
	}

	// ---- Parallel sorting ----
	//
	// A task-parallel merge sort over the PERMUTATION: each thread std::sorts one contiguous chunk
	// of it, then the chunks are merged pairwise in log(T) rounds. Hand-rolled on build cost --
	// std::execution::par needs TBB, and OpenMP tasks would need -fopenmp on the C++ compile, which
	// this project's CI deliberately does not set.
	//
	// **The answer is bit-identical to the serial sort, and that is structural rather than lucky.**
	// SortRowLess ends with a tiebreaker on the row index, so it is a TOTAL order under which no two
	// rows compare equal; every correct sorting algorithm therefore produces the same permutation.
	// The merge below uses that same object, taking from the left run when neither side is strictly
	// less, which is std::merge's own stability rule.
	//
	// **Nothing here decides HOW MANY threads to use.** The count arrives already resolved from the
	// Fortran side, which is the only side compiled with OpenMP and so the only one that can ask
	// omp_get_max_threads()/omp_in_parallel(). This function only declines a count it cannot use.

	// The row threshold below which threading is refused is kSortParallelMinRows, an internal
	// constant declared with the other engine defaults near the top of this file. It used to be the
	// published `sort_parallel_min_rows` setting; that knob was retired once the Fortran engine
	// stopped reading it, since it then governed only this engine and a setting whose scope is "one
	// of two engines, depending on which entry point you called" is worse than no setting at all.
	// Both floors are now internal, and this one governs an engine no user-facing path reaches --
	// see this section's banner. It still matters that the threshold behaves, because an A/B that
	// silently ran both arms serially would compare nothing.
	//
	// **A test that needs the parallel path at a small row count therefore cannot lower a setting
	// any more** -- it has to use parquet_debug_set_sort_engine_min_rows, which is what
	// force_parallel_threshold now drives. Without a way in, a test asserting "parallel matches
	// serial" would be asserting "serial matches serial", the vacuous shape feature_risks.md
	// Risk-35 exists to warn about.

	// Test-only: how many threads the last threaded build actually put to work, counting the
	// calling thread. 1 means the sort ran serially, whatever was asked for.
	static int64_t g_debug_sort_threads_used = 1;

	// Maintainer diagnostic: where the last threaded build spent its time, in nanoseconds.
	//   [0] phase 1 -- the per-chunk std::sorts.
	//   [1] phase 2 -- every merge round EXCEPT the last.
	//   [2] phase 2 -- the last round alone, which merges two runs on ONE thread and is the O(n)
	//       serial tail that caps the achievable speedup (see the phase 2 comment below).
	// Split that way because [2] is the quantity that decides whether a co-ranked parallel merge is
	// worth building: it is the only part of the sort that does not get faster as threads are added.
	//
	// Recorded unconditionally -- a handful of clock reads per sort, at ROUND granularity, never per
	// element -- so there is no arming flag to forget. Zeroed on entry, so a serial or counting-path
	// sort reports zeros and cannot be mistaken for a threaded one. Read via
	// parquet_debug_get_sort_phase_ns; process-global and non-atomic, like every counter here, so it
	// is only meaningful from a suite excluded from test-drive's per-test parallelism.
	static int64_t g_debug_sort_phase_ns[3] = {0, 0, 0};

	// Test-only: how many threads worked the FINAL merge round -- the round with two runs left -- the
	// calling thread included. 1 means that round ran on one thread, i.e. the serial tail is back.
	//
	// **The final round specifically, not a maximum over rounds.** A co-ranked merge that silently
	// degraded to one segment per pair everywhere except the first round would still report a high
	// maximum while leaving the whole tail in place -- and it would return the correct permutation
	// while doing so, because every thread count returns the same answer. This is Risk-39's shape one
	// level deeper: without an assertion on this counter, a merge that co-ranks nothing passes every
	// correctness test ever written for it. g_debug_sort_threads_used is NOT reused for this; it
	// keeps its own meaning (phase 1's count), which four existing tests assert against.
	static int64_t g_debug_sort_merge_threads_used = 1;

	// Nanoseconds since an arbitrary origin, for the phase timers above.
	static inline int64_t sort_now_ns()
	{
		return std::chrono::duration_cast<std::chrono::nanoseconds>(
			std::chrono::steady_clock::now().time_since_epoch()).count();
	}

	// Spawns f(k) for k in [lo, hi), returning the first index it could NOT spawn so the caller runs
	// the remainder on its own thread.
	//
	// A refused thread is a slowdown, never a failure. std::thread's constructor throws
	// std::system_error when the OS declines, and an exception reaching the extern "C" boundary
	// would call std::terminate and take the process down with it -- so this catches and reports
	// through the return value instead. `reserve` up front means a vector reallocation can never
	// drop an already-created thread on the floor.
	//
	// `extern "C++"` because everything in this file sits inside one big `extern "C"` block, and a
	// template cannot have C linkage ("templates must have C++ linkage"). The alternative -- taking
	// a std::function instead of a template parameter -- would work equally well here (the call
	// happens once per chunk, not once per comparison), but this keeps the lambda inlined.
	extern "C++" {
	template <typename F>
	static int64_t sort_spawn(std::vector<std::thread> &workers, int64_t lo, int64_t hi, F f)
	{
		int64_t k = lo;
		try
		{
			workers.reserve(static_cast<size_t>(hi - lo));
			for (; k < hi; ++k) workers.emplace_back(f, k);
		}
		catch (const std::system_error &) {}  // GCOVR_EXCL_LINE -- the OS refused a thread
		catch (const std::bad_alloc &) {}     // GCOVR_EXCL_LINE -- no room for the thread list
		return k;
	}  // GCOVR_EXCL_LINE -- exception-cleanup epilogue for the two handlers above, never entered
	}

	// ---- Co-ranked merge partitioning ----
	//
	// What lets a round's TWO runs be merged by many threads at once instead of one. Co-ranking asks:
	// for an output offset k, how many elements of the merged run came from the left run and how many
	// from the right? Answer that at T+1 offsets and the merge splits into T independent segments
	// writing disjoint output ranges.
	//
	// Everything below is about the LEFT-WINS-TIES rule, which is the only thing here that is easy to
	// get subtly wrong. See sort_corank's own comment.

	// One merge segment: two half-open source ranges in `from`, and where their merge starts in `to`.
	// Absolute indices, so the body needs no base to add.
	struct SortMergeSeg
	{
		int64_t li, le; //!< Left source range [li, le) in `from`.
		int64_t ri, re; //!< Right source range [ri, re) in `from`.
		int64_t out;    //!< First output position in `to`.
	};

	// Splits the merge of A = f[a, m) and B = f[m, b) at output offset k, returning i (elements taken
	// from A) via `i_out`; j is k - i by construction. 0 <= k <= (m - a) + (b - m).
	//
	// **The characterisation.** Writing nA = m - a and nB = b - m, the split (i, j = k - i) is the one
	// where merging A[0, i) with B[0, j) yields exactly the first k merged elements, i.e.
	//
	//   (1)  i == 0 || j == nB || !less(B[j],   A[i-1])     -- A[i-1] is not preceded by B[j]
	//   (2)  j == 0 || i == nA ||  less(B[j-1], A[i])       -- B[j-1] does precede A[i]
	//
	// **(1) is the tie rule in disguise, and is the line to read twice.** The merge emits A[i-1]
	// before B[j] exactly when `!less(B[j], A[i-1])` -- NOT when `less(A[i-1], B[j])`, which would
	// hand ties to the right run and break the stability rule the whole engine rests on. Under
	// SortRowLess the two forms happen to coincide, because its index tiebreaker makes it a total
	// order in which no two rows compare equal, so the wrong form would pass every test written here.
	// The correct form is written anyway, so this is right by derivation rather than by accident --
	// and so that anyone reusing it for pf_merge (which has real ties, and takes from the first input
	// on equal) starts from the right expression. See feature_risks.md Risk-37.
	//
	// **The search.** P(i) := "condition (1) holds at i" is monotone decreasing in i, and the answer
	// is the LARGEST i in [max(0, k - nB), min(k, nA)] with P(i) true. Two facts make that exact:
	//
	//   * P at the lower bound always holds -- there either i == 0 or j == nB, and (1) is trivial --
	//     so the search never returns an i that fails (1).
	//   * The maximal i satisfies (2) for free. If i < hi then P(i+1) is false, which unpacks to
	//     `less(B[k-i-1], A[i])`, i.e. (2) with j = k - i. If i == hi then i == nA or j == 0, and (2)
	//     is trivial.
	//
	// O(log min(nA, nB)) comparisons, and no allocation, so it is safe to call from a worker -- though
	// sort_merge_boundaries below calls it only on the spawning thread.
	static inline void sort_corank(const std::vector<int64_t> &f, int64_t a, int64_t m, int64_t b,
		int64_t k, const SortRowLess &less, int64_t &i_out)
	{
		int64_t nA = m - a, nB = b - m;
		int64_t lo = k - nB > 0 ? k - nB : 0;
		int64_t hi = k < nA ? k : nA;
		while (lo < hi)
		{
			// Upper mid, so a passing probe can keep `lo` without the loop standing still.
			int64_t i = lo + (hi - lo + 1) / 2;
			int64_t j = k - i;
			if (i > 0 && j < nB && less(f[static_cast<size_t>(m + j)], f[static_cast<size_t>(a + i - 1)]))
			{
				hi = i - 1; // too many taken from A: B[j] belongs before A[i-1]
			}
			else
			{
				lo = i;
			}
		}
		i_out = lo;
	}

	// Smallest output range worth giving a thread of its own. A merge step is far cheaper per element
	// than a sort comparison, so this floor has to be well above phase 1's own min_chunk
	// (kSortParallelMinRows / 4, i.e. 2048 -- a 16 KB segment, less work than creating
	// the thread to run it).
	//
	// **Measured basis**, so this is a number someone can argue with rather than a magic one: one
	// merge pass over 20 M elements costs 0.575 s on an 8-core M1 Pro, i.e. ~29 ns per element -- the
	// comparator's scattered `reals_ptr[perm[i]]` read, one cache miss per output element, not
	// bandwidth on the permutation stream (which is ~1% of it). So 16384 elements is ~0.5 ms of work
	// against a std::thread construction of perhaps 20-50 us: comfortably worth spawning, with room
	// to lower the floor if a workload ever wants finer segments. Deliberately NOT a setting -- it
	// has no meaning a caller can reason about. Nothing is user-facing here any more: the
	// "does this sort thread at all" floor is kSortParallelMinRows, also not a setting.
	static constexpr int64_t kSortMergeMinSegment = 1 << 14;

	// Test-only override of the floor above. <= 0 restores the real one.
	//
	// **Without this the co-rank is untestable at test sizes, and the gap is invisible.** A pair
	// shorter than 2 * 16384 is never split, so every array a unit test can afford to sort in a
	// dense sweep would take the unsegmented path and call sort_corank ZERO times -- while passing,
	// because an unsegmented merge is exactly the old correct one. Found by mutation: two deliberate
	// co-rank defects survived the entire suite until the sweep was made to lower this. Same
	// process-global, subprocess-free convention as parquet_debug_set_col_size_limit and the other
	// ceiling overrides, and the same reason the `sorting` suite is excluded from test-drive's
	// per-test parallelism. See feature_risks.md Risk-49.
	static int64_t g_debug_sort_merge_min_segment = -1;

	// The floor actually in force: the debug override when one is set, otherwise the real constant.
	static inline int64_t sort_merge_min_segment()
	{
		return g_debug_sort_merge_min_segment > 0 ? g_debug_sort_merge_min_segment : kSortMergeMinSegment;
	}

	// How many segments to split one pair's merge into. `len` is the pair's output length, `n` the
	// round's total, `threads` what phase 1 was given -- so segments are handed out in proportion to
	// how much of the round each pair actually is, and the round's task count lands near `threads`.
	//
	// Proportional rather than one-size-fits-all because a round's runs are only near-equal: phase 1's
	// bounds differ by at most one element, but an odd-run carry propagates a shorter run through
	// every later round. In the LAST round there is one pair and it takes all `threads` segments,
	// which is precisely the serial tail this work exists to remove.
	static inline int64_t sort_merge_segments(int64_t len, int64_t n, int64_t threads)
	{
		int64_t floor_len = sort_merge_min_segment();
		if (len < 2 * floor_len) return 1;
		int64_t want = (threads * len + n / 2) / n;   // round(threads * len / n)
		int64_t cap = len / floor_len;
		if (want > cap) want = cap;
		return want < 1 ? 1 : want;
	}

	// Appends `nseg` segments covering the merge of f[a, m) with f[m, b) into `to` starting at `a`.
	// `nseg >= 1`; nseg == 1 appends the whole pair as one segment and calls sort_corank not at all,
	// which is what makes an unsegmented pair cost exactly what it did before.
	//
	// Boundaries are computed HERE, on the spawning thread, rather than by each worker for its own two
	// ends. That is one co-rank per boundary instead of two, it removes any question of two threads
	// deriving different splits for the same k, and it leaves the partition inspectable -- which is
	// what the check below can then be written against.
	static void sort_merge_boundaries(const std::vector<int64_t> &f, int64_t a, int64_t m, int64_t b,
		int64_t nseg, const SortRowLess &less, std::vector<SortMergeSeg> &segs)
	{
		int64_t total = b - a;
		int64_t i_prev = 0, k_prev = 0;
		for (int64_t s = 1; s <= nseg; ++s)
		{
			int64_t k = total * s / nseg;
			int64_t i = m - a;
			if (s < nseg) sort_corank(f, a, m, b, k, less, i);
			// The invariant every later reader depends on: i and j = k - i are each non-decreasing
			// across s, i + j == k exactly, and the last boundary is (nA, nB). Break any of those and
			// the segments stop being a partition -- some rows are merged twice and others not at all,
			// and the result is silently no longer a permutation (feature_risks.md Risk-50). O(nseg)
			// against the round's O(n), so this costs nothing worth measuring.
			if (i < i_prev || i > m - a || k - i < k_prev - i_prev || k - i > b - m)
			{
				report_fatal_error("sort_merge_boundaries",             // GCOVR_EXCL_LINE
					"internal error: co-ranked merge boundaries are not a partition"); // GCOVR_EXCL_LINE
			}
			segs.push_back(SortMergeSeg{a + i_prev, a + i, m + (k_prev - i_prev), m + (k - i), a + k_prev});
			i_prev = i;
			k_prev = k;
		}
	}

	// 0-based permutation of [0, n), using up to `threads` threads. Identical to
	// sort_build_permutation's result in every case.
	static std::vector<int64_t> sort_build_permutation_threaded(const std::vector<SortKeyData> &keys,
		int64_t n, int64_t threads)
	{
		sort_check_keys_finalized(keys, n, "sort_build_permutation_threaded");
		g_debug_sort_threads_used = 1;
		g_debug_sort_merge_threads_used = 1;
		g_debug_sort_phase_ns[0] = g_debug_sort_phase_ns[1] = g_debug_sort_phase_ns[2] = 0;
		// The counting path is already O(n) and already produces this exact permutation, so it wins
		// over any number of threads: `threads` is a hint, not a command.
		int64_t lo = 0, hi = 0;
		if (g_sort_counting_path && sort_counting_candidate(keys, n, lo, hi))
		{
			return sort_counting_permutation(keys[0], n, lo, hi);
		}
		int64_t min_rows = (g_debug_sort_parallel_min_rows > 0) ? g_debug_sort_parallel_min_rows
		                                                       : kSortParallelMinRows;
		int64_t min_chunk = min_rows / 4;
		if (min_chunk < 1) min_chunk = 1;
		int64_t nchunks = threads;
		if (nchunks > n / min_chunk) nchunks = n / min_chunk;
		// g_debug_count_sort_comparisons disqualifies threading deliberately: that counter is NOT
		// atomic (by design -- an atomic increment in the engine's hottest loop would cost more than
		// the feature is worth), so counting across threads would be both a data race and a
		// meaningless number. The one test that enables it therefore always measures a serial sort.
		if (threads < 2 || n < min_rows || nchunks < 2 || g_debug_count_sort_comparisons)
		{
			return sort_comparison_permutation(keys, n);
		}

		std::vector<int64_t> perm(static_cast<size_t>(n));
		std::iota(perm.begin(), perm.end(), static_cast<int64_t>(0));
		std::vector<int64_t> bounds(static_cast<size_t>(nchunks) + 1);
		for (int64_t k = 0; k <= nchunks; ++k) bounds[static_cast<size_t>(k)] = n * k / nchunks;
		SortRowLess less{&keys};

		// Phase 1: one std::sort per chunk, on disjoint ranges of `perm`.
		//
		// A worker body must not throw: an exception escaping a std::thread's callable calls
		// std::terminate immediately rather than propagating to the joining thread. std::sort does
		// not allocate, and SortRowLess/sort_compare_key do no allocation and no I/O on any of the
		// three key families (std::string_view::compare cannot throw), so nothing here can. A future
		// key family whose comparison allocates would break that and must add its own guard.
		{
			int64_t t_phase1 = sort_now_ns();
			auto sort_chunk = [&perm, &bounds, less](int64_t k) {
				std::sort(perm.begin() + static_cast<ptrdiff_t>(bounds[static_cast<size_t>(k)]),
					perm.begin() + static_cast<ptrdiff_t>(bounds[static_cast<size_t>(k) + 1]), less);
			};
			std::vector<std::thread> workers;
			int64_t unspawned = sort_spawn(workers, 1, nchunks, sort_chunk);
			sort_chunk(0);
			for (int64_t k = unspawned; k < nchunks; ++k) sort_chunk(k); // GCOVR_EXCL_LINE -- only after a refusal
			for (auto &w : workers) w.join();
			g_debug_sort_threads_used = static_cast<int64_t>(workers.size()) + 1;
			g_debug_sort_phase_ns[0] = sort_now_ns() - t_phase1;
		}

		// Phase 2: merge the runs pairwise, ping-ponging between `perm` and one scratch buffer. This
		// is the 8n bytes of extra peak memory a threaded sort costs over a serial one, and it is
		// explicit rather than std::inplace_merge's internal allocation, whose failure mode is a
		// silent O(n log n) degradation.
		//
		// **Every round is CO-RANKED, so no round is limited by how many pairs it happens to have.**
		// A pairwise round has T/2, T/4, ..., 1 pairs, so the last one merged two runs on a single
		// thread: an O(n) pass whose cost does not fall as threads are added, and which measured 29.5%
		// of a 20 M-row sort at 8 threads (0.57 s, the SAME 0.57 s at 2, 4 and 8 threads). Co-ranking
		// splits each pair's output into segments instead, so a round's whole n elements are shared
		// over all T threads however few pairs it has. The phase goes from ~2n wall time, independent
		// of T, to n*log2(T)/T.
		//
		// The ANSWER is untouched: every segment merges disjoint source ranges into a disjoint output
		// range with the same left-wins-ties rule, so the permutation is bit-identical to the serial
		// one at every thread count, as it has always been.
		std::vector<int64_t> scratch(static_cast<size_t>(n));
		std::vector<int64_t> *from = &perm, *to = &scratch;
		std::vector<SortMergeSeg> segs;
		while (bounds.size() > 2)
		{
			int64_t t_round = sort_now_ns();
			size_t nruns = bounds.size() - 1;
			int64_t npairs = static_cast<int64_t>(nruns / 2);
			// An odd run count leaves one run unpaired. It becomes a pair whose RIGHT run is empty
			// rather than a special case: the merge body below then runs its "drain the left run" loop
			// and copies it through, never calling `less` at all, so there is no ordering decision to
			// get wrong and the carry can be segmented like any other pair.
			int64_t ntasks = static_cast<int64_t>((nruns + 1) / 2);
			segs.clear();
			for (int64_t p = 0; p < ntasks; ++p)
			{
				size_t pi = static_cast<size_t>(p);
				bool carry = (2 * pi + 2 > nruns);
				int64_t a = bounds[2 * pi];
				int64_t m = carry ? bounds[nruns] : bounds[2 * pi + 1];
				int64_t b = carry ? bounds[nruns] : bounds[2 * pi + 2];
				sort_merge_boundaries(*from, a, m, b,
					sort_merge_segments(b - a, n, nchunks), less, segs);
			}
			int64_t nseg = static_cast<int64_t>(segs.size());
			// One merge segment. The body is what merge_pair always was, with the pair's own bounds
			// replaced by the segment's -- which is the entire behavioural change in this phase.
			//
			// A worker body must not throw, for the reason phase 1 states; a segment allocates
			// nothing and neither does SortRowLess, and the boundaries it works from were computed
			// and checked before any thread was spawned.
			auto merge_seg = [&from, &to, &segs, less](int64_t s) {
				const SortMergeSeg &g = segs[static_cast<size_t>(s)];
				const std::vector<int64_t> &f = *from;
				std::vector<int64_t> &t = *to;
				int64_t i = g.li, j = g.ri, k = g.out;
				// `less(f[j], f[i]) ? right : left` takes from the LEFT run unless the right is
				// strictly smaller -- std::merge's stability rule, under a total order.
				while (i < g.le && j < g.re) t[static_cast<size_t>(k++)] = less(f[static_cast<size_t>(j)], f[static_cast<size_t>(i)])
					? f[static_cast<size_t>(j++)] : f[static_cast<size_t>(i++)];
				while (i < g.le) t[static_cast<size_t>(k++)] = f[static_cast<size_t>(i++)];
				while (j < g.re) t[static_cast<size_t>(k++)] = f[static_cast<size_t>(j++)];
			};
			std::vector<std::thread> workers;
			int64_t unspawned = sort_spawn(workers, 1, nseg, merge_seg);
			merge_seg(0);
			for (int64_t s = unspawned; s < nseg; ++s) merge_seg(s); // GCOVR_EXCL_LINE -- only after a refusal
			for (auto &w : workers) w.join();
			// The last round is always the one with exactly two runs left, whatever odd-run carries
			// happened earlier, so it needs no separate bookkeeping to recognize.
			if (nruns == 2) g_debug_sort_merge_threads_used = static_cast<int64_t>(workers.size()) + 1;
			g_debug_sort_phase_ns[nruns == 2 ? 2 : 1] += sort_now_ns() - t_round;
			std::vector<int64_t> next;
			next.reserve(static_cast<size_t>(npairs) + 2);
			for (int64_t p = 0; p <= npairs; ++p) next.push_back(bounds[static_cast<size_t>(2 * p)]);
			if (nruns % 2 == 1) next.push_back(bounds[nruns]);
			bounds.swap(next);
			std::swap(from, to);
		}
		return std::move(*from);
	}

	// ---- Selection (parquet_sorting's M2 operations) ----
	//
	// Both below reuse the counting fast path unchanged when it applies. That is not laziness: the
	// counting sort is already O(n) and already produces a FULLY ordered permutation, so there is
	// nothing a partial or nth variant of it could save. It does mean a low-cardinality integer key
	// performs ZERO comparisons on either path -- which any test asserting "partial does fewer
	// comparisons than full" has to account for, by using a key the counting path declines.

	// 0-based permutation whose first `count` entries are exactly the first `count` a full sort
	// would produce. Everything past `count` is unspecified and must not be read.
	static std::vector<int64_t> sort_build_partial_permutation(const std::vector<SortKeyData> &keys,
		int64_t n, int64_t count)
	{
		sort_check_keys_finalized(keys, n, "sort_build_partial_permutation");
		// Clamped here as well as on the Fortran side: the Fortran clamp is what sizes the output
		// array, this one is what keeps the entry point safe for any other caller.
		if (count < 0) count = 0;
		if (count > n) count = n;
		int64_t lo = 0, hi = 0;
		if (g_sort_counting_path && sort_counting_candidate(keys, n, lo, hi))
		{
			return sort_counting_permutation(keys[0], n, lo, hi);
		}
		std::vector<int64_t> perm(static_cast<size_t>(n));
		std::iota(perm.begin(), perm.end(), static_cast<int64_t>(0));
		std::partial_sort(perm.begin(), perm.begin() + static_cast<ptrdiff_t>(count), perm.end(),
			SortRowLess{&keys});
		return perm;
	}

	// The 0-based row index a full stable sort would place at 0-based rank `nth`.
	//
	// Only the one index is computed, not a permutation -- the caller reads its own value out of
	// its own array with it, which is what keeps this free of any per-type value handling.
	static int64_t sort_nth_index(const std::vector<SortKeyData> &keys, int64_t n, int64_t nth)
	{
		sort_check_keys_finalized(keys, n, "sort_nth_index");
		int64_t lo = 0, hi = 0;
		if (g_sort_counting_path && sort_counting_candidate(keys, n, lo, hi))
		{
			auto perm = sort_counting_permutation(keys[0], n, lo, hi);
			return perm[static_cast<size_t>(nth)];
		}
		std::vector<int64_t> perm(static_cast<size_t>(n));
		std::iota(perm.begin(), perm.end(), static_cast<int64_t>(0));
		std::nth_element(perm.begin(), perm.begin() + static_cast<ptrdiff_t>(nth), perm.end(),
			SortRowLess{&keys});
		return perm[static_cast<size_t>(nth)];
	}

	// ---- Arrow binding (the only Arrow-aware part of the engine) ----

	// Extracts `array` into a SortKeyData. Returns false, leaving `out` untouched, when the column's
	// physical type is not orderable by this engine -- the caller reports that with the type's own
	// name. The families deliberately match what the row filter accepts (see run_qc_range_check and
	// eval_filter_clause's own switches), so "a column you can filter on, you can sort on".
	static bool sort_bind_arrow_key(const std::shared_ptr<arrow::Array> &array, bool descending,
		bool nulls_first, SortKeyData &out)
	{
		int64_t n = array->length();
		auto id = array->type_id();
		SortKeyData key;
		key.descending = descending;
		key.nulls_first = nulls_first;

		if (is_small_integer_family(id))
		{
			key.kind = SortValueKind::Integer;
			key.ints.resize(static_cast<size_t>(n));
			for (int64_t i = 0; i < n; ++i) key.ints[static_cast<size_t>(i)] = small_integer_value_at(array.get(), i);
		}
		else if (id == arrow::Type::BOOL)
		{
			auto arr = std::static_pointer_cast<arrow::BooleanArray>(array);
			key.kind = SortValueKind::Integer;
			key.ints.resize(static_cast<size_t>(n));
			for (int64_t i = 0; i < n; ++i) key.ints[static_cast<size_t>(i)] = arr->Value(i) ? 1 : 0;
		}
		else if (id == arrow::Type::DATE32)
		{
			// Every temporal type is integer-valued, and its stored value orders exactly as the
			// date/time/instant does -- so temporal keys need no dispatch of their own beyond
			// picking the right concrete array class. (A timestamp column carries one stored unit
			// for the whole column, so comparing raw values compares instants.)
			auto arr = std::static_pointer_cast<arrow::Date32Array>(array);
			key.kind = SortValueKind::Integer;
			key.ints.resize(static_cast<size_t>(n));
			for (int64_t i = 0; i < n; ++i) key.ints[static_cast<size_t>(i)] = arr->Value(i);
		}
		else if (id == arrow::Type::TIME32)
		{
			auto arr = std::static_pointer_cast<arrow::Time32Array>(array);
			key.kind = SortValueKind::Integer;
			key.ints.resize(static_cast<size_t>(n));
			for (int64_t i = 0; i < n; ++i) key.ints[static_cast<size_t>(i)] = arr->Value(i);
		}
		// DATE64 cannot actually occur in a Parquet file this library (or a genuine foreign writer)
		// produces -- Arrow's writer always coerces a date64() array to date32() on write, and
		// Parquet has no int64/DATE64 physical representation at all (see CLAUDE.md's temporal
		// notes and the identical DATE64 exclusion a few hundred lines below in
		// convert_date_values/temporal_type_token). Kept only for symmetry with the read-side
		// DATE64 handling, which is itself equally unreachable.
		else if (id == arrow::Type::DATE64)
		{ // GCOVR_EXCL_START
			auto arr = std::static_pointer_cast<arrow::Date64Array>(array);
			key.kind = SortValueKind::Integer;
			key.ints.resize(static_cast<size_t>(n));
			for (int64_t i = 0; i < n; ++i) key.ints[static_cast<size_t>(i)] = arr->Value(i);
		}
		// GCOVR_EXCL_STOP
		else if (id == arrow::Type::TIME64)
		{
			auto arr = std::static_pointer_cast<arrow::Time64Array>(array);
			key.kind = SortValueKind::Integer;
			key.ints.resize(static_cast<size_t>(n));
			for (int64_t i = 0; i < n; ++i) key.ints[static_cast<size_t>(i)] = arr->Value(i);
		}
		else if (id == arrow::Type::TIMESTAMP)
		{
			auto arr = std::static_pointer_cast<arrow::TimestampArray>(array);
			key.kind = SortValueKind::Integer;
			key.ints.resize(static_cast<size_t>(n));
			for (int64_t i = 0; i < n; ++i) key.ints[static_cast<size_t>(i)] = arr->Value(i);
		}
		else if (id == arrow::Type::FLOAT || id == arrow::Type::DOUBLE || id == arrow::Type::HALF_FLOAT ||
			id == arrow::Type::UINT64 || id == arrow::Type::DECIMAL32 || id == arrow::Type::DECIMAL64 ||
			id == arrow::Type::DECIMAL128 || id == arrow::Type::DECIMAL256)
		{
			bool is_decimal = id == arrow::Type::DECIMAL32 || id == arrow::Type::DECIMAL64 ||
				id == arrow::Type::DECIMAL128 || id == arrow::Type::DECIMAL256;
			key.kind = SortValueKind::Real;
			key.reals.resize(static_cast<size_t>(n));
			for (int64_t i = 0; i < n; ++i)
			{
				if (array->IsNull(i)) { key.reals[static_cast<size_t>(i)] = 0.0; continue; }
				if (is_decimal) key.reals[static_cast<size_t>(i)] = decimal_value_at(array.get(), i);
				else if (id == arrow::Type::UINT64)
					key.reals[static_cast<size_t>(i)] = static_cast<double>(std::static_pointer_cast<arrow::UInt64Array>(array)->Value(i));
				else key.reals[static_cast<size_t>(i)] = real_family_value_at(array.get(), i);
			}
		}
		else if (is_string_like_type(id))
		{
			auto acc = make_string_like_accessor(array);
			key.kind = SortValueKind::Str;
			key.strs.resize(static_cast<size_t>(n));
			for (int64_t i = 0; i < n; ++i)
			{
				key.strs[static_cast<size_t>(i)] = acc.is_null(i) ? std::string_view() : acc.get_view(i);
			}
			key.owner = array; // the views point into this array's buffers
		}
		else
		{
			return false;
		}

		// An empty validity vector is the "no nulls at all" fast path the comparator checks for, so
		// only fill it when the column actually has nulls.
		if (array->null_count() > 0)
		{
			key.valid.assign(static_cast<size_t>(n), 1);
			for (int64_t i = 0; i < n; ++i)
			{
				if (array->IsNull(i)) key.valid[static_cast<size_t>(i)] = 0;
			}
		}
		sort_key_finalize(key);
		out = std::move(key);
		return true;
	}

	// ---- Raw-array binding (no Arrow at all) ----

	// Collects keys handed over as plain typed vectors, so the SAME engine that orders a
	// read-time sort_by= also orders parquet_table's in-memory %sort_by. An in-memory table's
	// Arrow buffers are gone by design (the table owns the only Fortran-side copy), so
	// sort_bind_arrow_key cannot serve it -- and reimplementing the ordering on the Fortran side
	// would give the library two comparators that could silently disagree, which is exactly the
	// failure this handle exists to make impossible.
	//
	// A handle rather than process-global builder state: two threads each sorting their own table
	// must not see each other's keys, and a global would have to be serialized instead.
	struct SortBuilderHandle
	{
		int64_t nrows = 0;
		std::vector<SortKeyData> keys;
		// Backing storage for the string keys' views. A deque, not a vector, because
		// SortKeyData::strs holds string_views into these strings and a vector reallocating on
		// the next add_key would dangle every one of them.
		std::deque<std::vector<std::string>> string_stores;
	};

	// Fills the per-row validity vector from Fortran's int8 flags. `valid` may be null, meaning
	// "no nulls at all" -- the empty-vector fast path the comparator already checks for, and what
	// parquet_column%row_validity produces for a null-free column.
	static void sort_builder_set_valid(SortKeyData &key, const int8_t *valid, int64_t n)
	{
		if (valid == nullptr) return;
		bool any_null = false;
		for (int64_t i = 0; i < n; ++i)
		{
			if (valid[i] == 0) { any_null = true; break; }
		}
		if (!any_null) return;
		key.valid.assign(static_cast<size_t>(n), 1);
		for (int64_t i = 0; i < n; ++i) key.valid[static_cast<size_t>(i)] = valid[i] != 0 ? 1 : 0;
	}

	// ==== Reader lifecycle (create/prefetch/qc/sample/filter/introspection) ====
	//
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
		// See ensure_arrow_type_singletons_initialized's own comment: this is the other of the two
		// entry points (with create_parquet_writer) any OpenMP thread can reach FIRST.
		ensure_arrow_type_singletons_initialized();
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
		g_debug_last_use_threads = use_threads;
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

		// The row-group -> first-physical-row table (see row_group_offsets' own comment). Reading
		// it here, once, replaces the per-call footer walk every row-group-scoped query used to do.
		{
			auto *file_metadata = handle->reader->parquet_reader()->metadata().get();
			handle->row_group_offsets.reserve(static_cast<size_t>(handle->num_row_groups) + 1);
			int64_t offset = 0;
			handle->row_group_offsets.push_back(0);
			for (int64_t rg = 0; rg < handle->num_row_groups; ++rg)
			{
				offset += file_metadata->RowGroup(static_cast<int>(rg))->num_rows();
				handle->row_group_offsets.push_back(offset);
			}
		}

		auto kv_metadata = handle->schema->metadata();
		if (kv_metadata)
		{
			for (int i = 0; i < kv_metadata->size(); ++i)
			{
				handle->table_metadata_cache.emplace_back(kv_metadata->key(i), kv_metadata->value(i));
			}
		}

		for (int i = 0; i < handle->schema->num_fields(); ++i)
		{
			collect_column_leaf_paths(handle->schema->field(i), std::string(), handle->column_path_cache);
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

			if (g_debug_force_whole_column_read_error)
			{
				report_fatal_error("parquet_reader_prefetch_columns",
					"forced debug error: whole-column prefetch attempted"); // GCOVR_EXCL_LINE
			}
			auto table = read_live_row_groups(reader_handle, leaf_indices);

			for (size_t i = 0; i < indices.size(); ++i)
			{
				// The result table always reconstructs one column per distinct top-level field
				// actually touched, in original schema order (regardless of leaf request order)
				// -- so its own field name (unique at the top level) is what maps a requested
				// column back to its result position, not a positional index into `indices`.
				auto result_pos = table->schema()->GetFieldIndex(top_names[i]);
				auto chunked = table->column(result_pos);
				auto array = apply_row_transform(reader_handle, combine_column_chunks(chunked, top_names[i]));
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
	// here before the mask is set would stay raw/unfiltered forever, since
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

		if (g_debug_force_whole_column_read_error)
		{
			report_fatal_error("parquet_reader_prefetch_columns_by_index",
				"forced debug error: whole-column prefetch attempted"); // GCOVR_EXCL_LINE
		}
		auto table = read_live_row_groups(reader_handle, leaf_indices);

		for (size_t i = 0; i < indices.size(); ++i)
		{
			auto result_pos = table->schema()->GetFieldIndex(names[i]);
			auto chunked = table->column(result_pos);
			auto array = apply_row_transform(reader_handle, combine_column_chunks(chunked, names[i]));
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

	// True (1) if `handle` was opened with an active row filter.
	//
	// Apparently unused: grepping every src/*.f90 file finds no Fortran call site (not even a
	// local bind(C) interface in test/), and parquet_get_chunk_size (parquet_read.f90) only
	// guards against an active SORT (check_reader_no_sort) -- a filtered reader can call
	// parquet_get_chunk_size/parquet_read_column_chunk today, contradicting this function's own
	// doc comment (kept below for now) that claims both are disallowed on a filtered reader. Most
	// likely a leftover from a design this function's own guard used to enforce before row-group-
	// scoped filtering existed; left in place (not deleted) since removing exported surface is a
	// bigger decision than a coverage pass should make unilaterally -- flag for the maintainer to
	// confirm before either deleting it or wiring it back into an actual guard.
	int parquet_reader_has_filter(void *handle) // GCOVR_EXCL_START
	{
		auto reader_handle = as_reader_handle(handle);
		return reader_handle->live_mask ? 1 : 0;
	}
	// GCOVR_EXCL_STOP

	// True (1) if `handle` has filter CLAUSES installed. Narrower than parquet_reader_has_filter
	// above, which also reports a sample-only mask: parquet_reader_set_filter (parquet.f90)
	// refuses an already-filtered reader but accepts a sampled one, and needs to tell those two
	// states apart. filter_clauses is populated by every successful clause, so it is empty exactly
	// when no filter has been applied.
	int64_t parquet_reader_has_filter_clauses(void *handle)
	{
		auto reader_handle = as_reader_handle(handle);
		return reader_handle->filter_clauses.empty() ? 0 : 1;
	}

	// True (1) if any column of `handle` has already been decoded into column_cache. The guard
	// parquet_reader_set_filter (parquet.f90) needs before applying a filter post-open: a column
	// already handed back over the unfiltered row set could not be aligned with anything read
	// after the filter is installed.
	int64_t parquet_reader_has_decoded_columns(void *handle)
	{
		auto reader_handle = as_reader_handle(handle);
		return reader_handle->column_cache.empty() ? 0 : 1;
	}

	// Whether any column has been read through the CHUNK api on this reader. Deliberately a
	// separate question from parquet_reader_has_decoded_columns above: a chunked read frees each
	// row group's array before returning and caches nothing, precisely so a loop over a thousand
	// row groups holds no more than a loop over one -- so column_cache stays empty and cannot
	// answer this. Without it, parquet_reader_set_filter/_set_sort would accept a reader that has
	// already handed back rows in physical, unfiltered order.
	//
	// chunk_read_row_groups is reused rather than a fresh flag being added: it already records
	// exactly this, per column, for parquet_reader_check_complete.
	int64_t parquet_reader_has_chunk_reads(void *handle)
	{
		auto reader_handle = as_reader_handle(handle);
		return reader_handle->chunk_read_row_groups.empty() ? 0 : 1;
	}

	// Returns the EFFECTIVE row count of row group `row_group` (1-based; already resolved/
	// validated by the Fortran caller -- see parquet_get_chunk_size's reader specifics in
	// parquet_read.f90, which check row_group against parquet_reader_get_num_row_groups first).
	// Row groups are not guaranteed uniform, so this is a genuine per-row-group query, not a
	// single file-wide constant the way the writer side's resolved chunk_size is.
	//
	// "Effective", not "physical": row_group_effective_rows reports that row group's SURVIVING
	// rows once a filter or sample is active, and its physical count only when neither is. The
	// distinction is what lets a chunked loop over a filtered reader size its buffers from this
	// call alone, and it is why the sizes still sum to parquet_get_nrows.
	int64_t parquet_reader_get_chunk_size_at(void *handle, int64_t row_group)
	{
		auto reader_handle = as_reader_handle(handle);
		return row_group_effective_rows(reader_handle, row_group);
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

	// Random-downsampling support for parquet_open_reader(..., sample_fraction=). The Bernoulli
	// trials are NOT made here: parquet_apply_sample (parquet_read.f90) builds the whole
	// per-PHYSICAL-row keep mask with this library's own counter-based generator and hands it over
	// as `keep`, one byte per row of the file. parquet_sample_algorithm (parquet_core.f90) states
	// the frozen mapping; test/test_random_vectors.f90 pins it against an independent oracle.
	//
	// WHY THE MASK ARRIVES READY-MADE RATHER THAN BEING DRAWN HERE, because the previous shape is
	// the one a reader will expect. The draw used to be a std::mt19937_64 stepped once per row, so
	// row i's decision depended on how many draws had preceded it. That made two things true at
	// once: a row group the statistics screen pruned still had to be drawn for and discarded, or
	// the same seed would select different rows depending on an unrelated optimisation; and
	// std::uniform_real_distribution is unspecified across standard library implementations, so
	// "the same seed selects the same rows" was a promise the C++ standard did not underwrite.
	// keep[i] is now a pure function of (seed, i). Nothing downstream can shift it, the deferred
	// and immediate paths below are identical by construction rather than by discipline, and the
	// mapping is frozen and tested rather than inherited from whichever libstdc++ is installed.

	// Test-only: makes the length guard below reject a length that is in fact correct. The guard
	// cannot otherwise be reached -- Fortran sizes the mask from this same handle's total_nrows --
	// so without this hook it would be defensive code no test could exercise. Reached only through
	// the local bind(C) interface in test/error_scenarios.f90.
	// Its setter, parquet_debug_set_force_sample_len_mismatch, sits with the other debug setters
	// further down; only the flag lives here, next to the guard it forces.
	static bool g_debug_force_sample_len_mismatch = false;

	// See parquet_debug_set_force_sample_mask_error further down for what this one forces.
	static bool g_debug_force_sample_mask_error = false;

	// Applies the caller-built Bernoulli row mask to `handle`, called from parquet_apply_sample
	// (parquet_read.f90) right after the reader is created and before any filter=/qc setup.
	// sample_fraction and seed_used are recorded for parquet_reader_print_stat's "sample:" line
	// only -- every keep/drop decision is already in `keep`, which spans the file's PHYSICAL rows,
	// all keep_len == total_nrows of them, one byte each, nonzero meaning keep.
	//
	// filter_will_follow (set by the Fortran caller from `present(filter) .and. filter%n > 0`):
	// when true the mask is only STASHED here, and parquet_reader_set_filter folds it in once it
	// knows which row groups survive. That deferral is about ORDERING and nothing else now --
	// installing a mask here would make the filter's own referenced columns come back already
	// sample-compacted while its clauses are being evaluated, breaking the row-index alignment
	// clause evaluation depends on (confirmed by a real Arrow "must all be the same length" crash
	// when this wasn't deferred). It is NO LONGER about which rows survive screening: the mask is
	// the same either way, which is exactly what the coordinate-addressed draw bought.
	//
	// Returns 0 on success; 1 with a reason in err_out on a length disagreement or on the (not
	// fixture-triggerable on its own) BooleanBuilder allocation failure inside install_row_mask.
	int64_t parquet_reader_set_sample(void *handle, double sample_fraction, int64_t seed_used,
		const int8_t *keep, int64_t keep_len, int8_t filter_will_follow, char *err_out, int64_t err_cap)
	{
		auto reader_handle = as_reader_handle(handle);

		if (g_debug_force_sample_mask_error)
		{
			std::snprintf(err_out, static_cast<size_t>(err_cap), "forced debug error: sample mask build failed");
			return 1;
		}

		// Fortran sizes the mask from this handle's own total_nrows, so this cannot fire through the
		// public API. It is here because the two sides could drift -- a future row-ranged or
		// otherwise derived length is the obvious way -- and the failure would then be a silent
		// out-of-bounds read rather than an error. tools/check_bindc_boundary.py checks signatures,
		// never buffer lengths, so nothing else covers this.
		int64_t expected = reader_handle->total_nrows + (g_debug_force_sample_len_mismatch ? 1 : 0);
		if (keep_len != expected)
		{
			std::snprintf(err_out, static_cast<size_t>(err_cap),
				"sample mask covers %lld rows but the file has %lld",
				static_cast<long long>(keep_len), static_cast<long long>(expected));
			return 1;
		}

		reader_handle->has_sample = true;
		reader_handle->sample_fraction = sample_fraction;
		reader_handle->sample_seed_used = seed_used;

		if (filter_will_follow != 0)
		{
			reader_handle->has_pending_sample = true;
			reader_handle->pending_sample_keep.assign(keep, keep + keep_len);
			return 0;
		}

		// No screen runs for a sample-only reader (there is no expression to screen with), so every
		// row group is live and the live-row layout is just the physical one.
		reader_handle->row_group_live.clear();
		reader_handle->row_groups_pruned = 0;
		int64_t live_rows = assign_row_group_live_offsets(reader_handle);
		std::vector<uint8_t> combined(static_cast<size_t>(live_rows), 0);
		// Every row group is live here, so the live-row index and the physical row index coincide.
		for (int64_t i = 0; i < live_rows; ++i) combined[static_cast<size_t>(i)] = keep[i] != 0 ? 1 : 0;

		if (!install_row_mask(reader_handle, combined, err_out, err_cap)) return 1; // GCOVR_EXCL_LINE
		return 0;
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

	// Kleene truth values one row of a filter expression can take. Three rather than two because
	// the combinators need to tell "this comparison is false" from "this comparison had nothing to
	// compare": under NOT, a Null row must stay excluded rather than flip into the result, and OR
	// must not resurrect a Null row merely because its other operand was false. kUnknown collapses
	// to kFalse exactly once, when the BooleanArray is built (see parquet_reader_set_filter), which
	// is what makes the whole thing agree with SQL's WHERE and with Arrow's own kernels.
	static constexpr uint8_t kFalse = 0;
	static constexpr uint8_t kTrue = 1;
	static constexpr uint8_t kUnknown = 2;

	// kTrue/kFalse from a plain comparison result.
	static inline uint8_t kleene_of(bool b) { return b ? kTrue : kFalse; }

	// Evaluates one filter clause (one leaf of the expression) against `array` -- the filter
	// column's own, still-unfiltered decoded array; no mask is installed on the reader
	// while this runs. WRITES one Kleene value per row into `out` (pre-sized to the array's
	// length) rather than combining into a shared accumulator: combining is the stack machine's
	// job in parquet_reader_set_filter, because which rows a leaf's result combines with depends
	// on the expression's shape, not on the leaf.
	//
	// A Null row is kUnknown for every comparison operator. is_null/is_not_null are the exception
	// by design -- selecting on nullness is exactly their purpose, so they always answer
	// kTrue/kFalse and are the only way to let a Null row through.
	//
	// A NaN, by contrast, is an ordinary VALUE, not a missing one, so it is never kUnknown: it
	// compares kFalse under >, >=, <, <= and == (IEEE says every comparison against NaN is false)
	// and kTrue under /= -- which means, unlike a Null, a NaN row survives a negated comparison.
	// is_nan/is_not_nan (floating-point columns only) select on it directly; they are Kleene-honest
	// about nullness, answering kUnknown for a Null row exactly as a comparison does, so that
	// nullness stays governed solely by is_null/is_not_null.
	//
	// Returns false (with `err` set) on any validation failure (unknown/unsupported type for the
	// clause's operator, unparseable value, ...); the caller then aborts the whole
	// parquet_reader_set_filter call, same as for an unknown filter column.
	static bool eval_filter_clause(const std::shared_ptr<arrow::Array> &array, const std::string &colname,
		const std::string &op, bool is_string, const std::string &value_text,
		std::vector<uint8_t> &out, std::string &err)
	{
		int64_t n = array->length();

		if (op == "is_null" || op == "is_not_null")
		{
			bool want_null = (op == "is_null");
			for (int64_t i = 0; i < n; ++i)
			{
				out[static_cast<size_t>(i)] = kleene_of(want_null ? array->IsNull(i) : array->IsValid(i));
			}
			return true;
		}

		if (op == "is_nan" || op == "is_not_nan")
		{
			// Restricted to the three types that can actually hold a NaN. DECIMAL* and UINT64 also
			// reach the comparison arms below as doubles, but no value of either can ever BE a NaN,
			// so accepting them would answer a constant (all-false / all-true) for what is almost
			// certainly a mistyped column name or a misunderstanding -- rejected instead, the same
			// call this file already makes for an ordering comparison on a boolean column.
			arrow::Type::type tid = array->type_id();
			if (tid != arrow::Type::FLOAT && tid != arrow::Type::DOUBLE && tid != arrow::Type::HALF_FLOAT)
			{
				err = "'" + op + "' is only supported for floating-point columns, and column '" + colname +
					"' is " + array->type()->ToString();
				return false;
			}
			bool want_nan = (op == "is_nan");
			for (int64_t i = 0; i < n; ++i)
			{
				if (array->IsNull(i))
				{
					out[static_cast<size_t>(i)] = kUnknown;
					continue;
				}
				bool isnan = std::isnan(real_family_value_at(array.get(), i));
				out[static_cast<size_t>(i)] = kleene_of(want_nan ? isnan : !isnan);
			}
			return true;
		}

		// Resolved ONCE for this clause, then used by every row loop below -- see CmpOp's own
		// comment for the measurement that made this worth doing. Placed after the is_null/is_nan
		// arms above, which are not comparisons and return before reaching it.
		const CmpOp cmp = cmp_op_of(op);

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
					out[static_cast<size_t>(i)] = arr->IsNull(i) ? kUnknown
						: kleene_of(compare_op<int32_t>(arr->Value(i), v, cmp));
				}
			}
			else if (array->type_id() == arrow::Type::INT64)
			{
				auto arr = std::static_pointer_cast<arrow::Int64Array>(array);
				for (int64_t i = 0; i < n; ++i)
				{
					out[static_cast<size_t>(i)] = arr->IsNull(i) ? kUnknown
						: kleene_of(compare_op<int64_t>(arr->Value(i), parsed, cmp));
				}
			}
			else
			{
				// INT8/INT16/UINT8/UINT16/UINT32: every value widens into
				// int64_t exactly, so compare directly with no extra range
				// pre-check (same as the plain INT64 branch above).
				for (int64_t i = 0; i < n; ++i)
				{
					out[static_cast<size_t>(i)] = array->IsNull(i) ? kUnknown
						: kleene_of(compare_op<int64_t>(small_integer_value_at(array.get(), i), parsed, cmp));
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
			// strtod accepts "nan" and "inf" alike, but only one of them is meaningful here. Every
			// IEEE comparison against NaN is false and every /= against it is true, so "x == nan"
			// can only ever match nothing and "x /= nan" everything non-null -- never what the
			// caller meant. Rejected in favour of is_nan/is_not_nan, which say it directly. An
			// infinity is a genuine, comparable bound and stays accepted.
			if (std::isnan(parsed))
			{
				err = "value '" + value_text + "' for column '" + colname +
					"' is not a comparable number; use the 'is_nan'/'is_not_nan' operators instead";
				return false;
			}
			if (array->type_id() == arrow::Type::FLOAT || array->type_id() == arrow::Type::DOUBLE ||
				array->type_id() == arrow::Type::HALF_FLOAT)
			{
				for (int64_t i = 0; i < n; ++i)
				{
					out[static_cast<size_t>(i)] = array->IsNull(i) ? kUnknown
						: kleene_of(compare_op<double>(real_family_value_at(array.get(), i), parsed, cmp));
				}
			}
			else if (array->type_id() == arrow::Type::UINT64)
			{
				auto arr = std::static_pointer_cast<arrow::UInt64Array>(array);
				for (int64_t i = 0; i < n; ++i)
				{
					out[static_cast<size_t>(i)] = arr->IsNull(i) ? kUnknown
						: kleene_of(compare_op<double>(static_cast<double>(arr->Value(i)), parsed, cmp));
				}
			}
			else
			{
				// DECIMAL32/64/128/256.
				for (int64_t i = 0; i < n; ++i)
				{
					out[static_cast<size_t>(i)] = array->IsNull(i) ? kUnknown
						: kleene_of(compare_op<double>(decimal_value_at(array.get(), i), parsed, cmp));
				}
			}
			return true;
		}
		case arrow::Type::DATE32:
		case arrow::Type::DATE64:
		case arrow::Type::TIME32:
		case arrow::Type::TIME64:
		case arrow::Type::TIMESTAMP:
		{
			// The clause's ISO-8601 literal was already converted, Fortran-side, into the raw
			// integer this column physically stores -- days for a DATE32, units-of-day for a
			// TIME32/64, units-since-epoch for a TIMESTAMP (see convert_temporal_filter_values in
			// parquet_read.f90, which is also where a literal too precise for the column's own
			// unit is rejected). So there is no ISO parsing and no unit arithmetic to do here:
			// compare raw stored values, exactly as the integer arms above do.
			int64_t parsed;
			if (!parse_int64_strict(value_text, parsed))
			{ // GCOVR_EXCL_START -- unreachable through the public API: the Fortran side rewrites
			  // every temporal clause's value into a decimal integer, and rejects anything it
			  // could not convert, before this is called.
				err = "value '" + value_text + "' is not a valid " + array->type()->ToString() +
					" for column '" + colname + "'";
				return false;
			}
			// GCOVR_EXCL_STOP
			// Read the values buffer directly through ArrayData::GetValues, which applies the
			// array's own offset (so a sliced array is handled) and works for any of these five
			// types without a per-type Array subclass cast -- DATE32/TIME32 store int32,
			// DATE64/TIME64/TIMESTAMP int64.
			if (array->type_id() == arrow::Type::DATE32 || array->type_id() == arrow::Type::TIME32)
			{
				const int32_t *raw = array->data()->GetValues<int32_t>(1);
				for (int64_t i = 0; i < n; ++i)
				{
					out[static_cast<size_t>(i)] = array->IsNull(i) ? kUnknown
						: kleene_of(compare_op<int64_t>(static_cast<int64_t>(raw[i]), parsed, cmp));
				}
			}
			else
			{
				// DATE64 cannot actually occur -- Arrow's writer always coerces date64() to
				// date32() (see CLAUDE.md's temporal notes and convert_date_values' own DATE64
				// branch) -- but it costs nothing to handle alongside the two that can.
				const int64_t *raw = array->data()->GetValues<int64_t>(1);
				for (int64_t i = 0; i < n; ++i)
				{
					out[static_cast<size_t>(i)] = array->IsNull(i) ? kUnknown
						: kleene_of(compare_op<int64_t>(raw[i], parsed, cmp));
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
				out[static_cast<size_t>(i)] = arr->IsNull(i) ? kUnknown
					: kleene_of((op == "==") ? (arr->Value(i) == bval) : (arr->Value(i) != bval));
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
			// compare_op is used at string_view rather than std::string: this used to build a
			// std::string from the view PER ROW -- a malloc, a copy and a free -- purely to make
			// the call. string_view's operator< and operator== are byte-lexicographic, which is
			// what compare_op<std::string> already was, so the ordering is unchanged. value_view
			// is hoisted because the bound is loop-invariant.
			const std::string_view value_view(value_text);
			for (int64_t i = 0; i < n; ++i)
			{
				out[static_cast<size_t>(i)] = acc.is_null(i) ? kUnknown
					: kleene_of(compare_op<std::string_view>(acc.get_view(i), value_view, cmp));
			}
			return true;
		}
		default:
			err = "column '" + colname + "' has a type that filtering does not support";
			return false;
		}
	}

	// Combines two Kleene vectors in place: `lhs` becomes (lhs AND rhs) or (lhs OR rhs).
	//   AND: false if either is false; unknown if either is unknown; else true.
	//   OR : true  if either is true ; unknown if either is unknown; else false.
	// Written into lhs rather than a third vector so a long chain of combinators keeps exactly one
	// row-length vector live per stack slot -- which, for a flat AND-only expression of any length,
	// is the same single allocation the AND-only implementation used before expressions existed.
	static void kleene_combine(std::vector<uint8_t> &lhs, const std::vector<uint8_t> &rhs, bool is_and)
	{
		size_t n = lhs.size();
		for (size_t i = 0; i < n; ++i)
		{
			uint8_t a = lhs[i];
			uint8_t b = rhs[i];
			if (is_and)
			{
				if (a == kFalse || b == kFalse) lhs[i] = kFalse;
				else if (a == kUnknown || b == kUnknown) lhs[i] = kUnknown;
				else lhs[i] = kTrue;
			}
			else
			{
				if (a == kTrue || b == kTrue) lhs[i] = kTrue;
				else if (a == kUnknown || b == kUnknown) lhs[i] = kUnknown;
				else lhs[i] = kFalse;
			}
		}
	}

	// Negates a Kleene vector in place: false <-> true, unknown unchanged. An unknown row staying
	// unknown (rather than becoming true) is the whole reason the third state exists -- it is what
	// keeps "not (x > 5)" from admitting every row where x is Null.
	static void kleene_negate(std::vector<uint8_t> &v)
	{
		for (auto &x : v)
		{
			if (x == kFalse) x = kTrue;
			else if (x == kTrue) x = kFalse;
		}
	}


	// ==== Row-group statistics pre-screen (F4) ====
	//
	// Consults each row group's own footer statistics and rules out the ones that provably cannot
	// contain a matching row, so a filtered read skips them entirely -- both when evaluating the
	// filter and, via read_live_row_groups, for every payload column the caller reads afterwards.
	//
	// NOTHING ABOUT THE ANSWER CHANGES. The mask this produces is bit-identical to the mask built
	// without it; only how much of the file was read to produce it differs. That makes every
	// failure here a SILENT WRONG ANSWER rather than an abort, which is why every rule below is
	// written so the failure direction is always "prune nothing":
	//
	//   * every uncertainty (statistics absent, unusable ordering, unsupported type, unparseable
	//     literal) returns kScreenAnything, a leaf that prunes nothing and, per the combinators,
	//     cannot make anything else prune either;
	//   * a row group is pruned ONLY if the whole expression's may_true is false.
	//
	// It lives next to the evaluator rather than next to column_has_nulls_from_footer (its nearest
	// structural relative) for one reason: it walks the SAME postfix node list, with the same stack
	// shape, as evaluate_nodes in parquet_reader_set_filter. Keeping the two adjacent is what makes
	// a future change to the node kinds hard to apply to one and miss in the other -- the drift
	// between these two code paths is the second-biggest risk in F4 after the rules themselves.

	// Which Kleene values a node can take SOMEWHERE in one row group -- the powerset lift of the
	// per-row kFalse/kTrue/kUnknown the evaluator computes. Three flags rather than one value
	// because the combinators need them; only may_true decides pruning.
	struct KleenePossible
	{
		bool may_true;
		bool may_false;
		bool may_unknown;
	};

	// "This leaf could be anything here" -- the answer every gate below returns when it declines.
	static const KleenePossible kScreenAnything{true, true, true};

	// The powerset lift of kleene_negate.
	static KleenePossible screen_negate(const KleenePossible &a)
	{
		return KleenePossible{a.may_false, a.may_true, a.may_unknown};
	}

	// The powerset lift of kleene_combine.
	//
	// AND's may_true is an OVER-APPROXIMATION and must stay one: "some row satisfies a, and some
	// row satisfies b" is not "the same row satisfies both". Row-group statistics are per-column
	// marginals and carry no joint information, so nothing better is available -- and the
	// over-approximation is sound, since it can only fail to prune. Tightening it is a bug.
	static KleenePossible screen_combine(const KleenePossible &a, const KleenePossible &b, bool is_and)
	{
		KleenePossible res;
		if (is_and)
		{
			res.may_true = a.may_true && b.may_true;
			res.may_false = a.may_false || b.may_false;
			res.may_unknown = (a.may_unknown && !b.may_false) || (b.may_unknown && !a.may_false);
		}
		else
		{
			res.may_true = a.may_true || b.may_true;
			res.may_false = a.may_false && b.may_false;
			res.may_unknown = (a.may_unknown && !b.may_true) || (b.may_unknown && !a.may_true);
		}
		return res;
	}

	// Which comparison family a leaf's column belongs to, decided once from the Arrow schema (no
	// data read) and paired with the Parquet physical statistics type that carries its bounds.
	enum class ScreenFamily { kNone, kInt, kReal, kBool, kString };

	// One filter leaf, pre-resolved for the screen: everything that does not vary per row group,
	// worked out once before the row-group loop. `usable` false means every rule for this leaf
	// declines -- the leaf then contributes kScreenAnything to every row group.
	struct ScreenLeaf
	{
		bool usable = false;
		int leaf_index = -1;             // Parquet flat-leaf column index, for ColumnChunk()
		ScreenFamily family = ScreenFamily::kNone;
		bool is_null_test = false;       // is_null / is_not_null: needs only the null count
		bool is_nan_test = false;        // is_nan / is_not_nan: needs only the null count too
		bool want_null = false;          // for is_null (true) vs is_not_null (false)
		bool want_nan = false;           // for is_nan (true) vs is_not_nan (false)
		bool is_float = false;           // FLOAT/DOUBLE: NaN makes the bounds one-directional
		std::string op;
		int64_t ival = 0;
		double dval = 0.0;
		bool bval = false;
		std::string sval;
	};

	// The comparison rules of the screen, over a THREE-WAY comparison of each bound against the
	// literal rather than over the values themselves: cmp_lo is -1/0/+1 as min is below/equal
	// to/above the literal, cmp_hi likewise for max. One rule set then serves every family, which
	// is what keeps the integer, float and string cases from drifting apart (this file cannot use
	// a template -- it is all inside extern "C").
	//
	//   nn    = non-null value count in this chunk (Statistics::num_values)
	//   nc    = null count
	//   exact = both bounds are known not to be truncated (see the caller)
	static KleenePossible screen_compare_from_bounds(const std::string &op, int cmp_lo, int cmp_hi,
		int64_t nn, int64_t nc, bool exact, bool is_float)
	{
		KleenePossible res{true, true, nc > 0};
		bool constant_at_v = (cmp_lo == 0 && cmp_hi == 0 && exact);
		if (op == ">")
		{
			res.may_true = nn > 0 && cmp_hi > 0;
			res.may_false = nn > 0 && cmp_lo <= 0;
		}
		else if (op == ">=")
		{
			res.may_true = nn > 0 && cmp_hi >= 0;
			res.may_false = nn > 0 && cmp_lo < 0;
		}
		else if (op == "<")
		{
			res.may_true = nn > 0 && cmp_lo < 0;
			res.may_false = nn > 0 && cmp_hi >= 0;
		}
		else if (op == "<=")
		{
			res.may_true = nn > 0 && cmp_lo <= 0;
			res.may_false = nn > 0 && cmp_hi > 0;
		}
		else if (op == "==")
		{
			res.may_true = nn > 0 && cmp_lo <= 0 && cmp_hi >= 0;
			res.may_false = nn > 0 && !constant_at_v;
		}
		else if (op == "/=")
		{
			res.may_true = nn > 0 && !constant_at_v;
			res.may_false = nn > 0 && cmp_lo <= 0 && cmp_hi >= 0;
		}
		else
		{ // GCOVR_EXCL_START -- every operator reaching here is one of the six above; the null and
		  // NaN tests never call this, and an unknown operator is rejected by the Fortran lexer.
			return kScreenAnything;
		}
		// GCOVR_EXCL_STOP

		// The single subtlest rule in F4, in BOTH directions. Parquet excludes NaN from min/max and
		// records no NaN count anywhere, so for a float column the bounds cannot rule a NaN in or
		// out -- and a NaN behaves oppositely to a Null, being an ordinary value that compares
		// false rather than a missing one that compares unknown (see eval_filter_clause).
		//
		//   * may_false becomes unconditional: a NaN row makes EVERY comparison false while sitting
		//     outside [min, max], so the ordering-derived may_false above can say false for a chunk
		//     that really does contain false rows. may_false is what NOT consumes, so getting this
		//     wrong prunes a row group that has matching rows -- e.g. {1.0, 2.0, NaN} under
		//     "not (x > 0.5)", where the NaN row matches but min = 1.0 > 0.5 claims nothing is
		//     false. Reachable without is_nan too: "not (x >= 0 or x < 0)" is exactly "x is_nan".
		//   * /= may never prune: NaN /= anything is true, so a chunk whose min == max == v can
		//     still contain matching rows.
		//
		// >, >=, <, <=, == keep their may_true: NaN fails all of them, so excluding NaN from the
		// bounds can only make the screen less willing to prune.
		if (is_float)
		{
			res.may_false = nn > 0;
			if (op == "/=") res.may_true = nn > 0;
		}
		return res;
	}

	// Resolves one filter leaf into a ScreenLeaf, or leaves it unusable (declining to prune with
	// it). This is where §4.3's gates (b), (c) and the type coverage of (§4.4) live: the family
	// comes from the ARROW schema -- literally the same type test eval_filter_clause dispatches on
	// -- while the bounds themselves come from Parquet's typed statistics, so the screen and the
	// evaluator can never disagree about what kind of column this is.
	static ScreenLeaf resolve_screen_leaf(ParquetReaderHandle *reader_handle, const std::string &name,
		const std::string &op, bool is_string, const std::string &value_text)
	{
		ScreenLeaf leaf;
		leaf.op = op;

		auto resolved = resolve_struct_path(reader_handle->schema, name);
		auto idx = get_column_index(reader_handle, resolved.top_level_name.c_str());
		leaf.leaf_index = static_cast<int>(
			resolve_single_leaf_index(reader_handle, static_cast<int>(idx), resolved.child_path));

		// A struct leaf is allowed, unlike in column_has_nulls_from_footer, and the two are not
		// inconsistent. A Parquet leaf's null_count counts every slot whose definition level falls
		// short of the maximum, which INCLUDES ancestor-struct nulls -- i.e. exactly the rows
		// unwrap_struct_path produces as null -- so for a scalar leaf (the only kind filtering
		// accepts) num_values/null_count describe the unwrapped result rather than contradicting
		// it, and min/max bound only the leaf's own present values either way. Proven by a
		// dedicated equality test over test/fixtures/nested_struct.parquet rather than assumed.

		if (op == "is_null" || op == "is_not_null")
		{
			leaf.usable = true;
			leaf.is_null_test = true;
			leaf.want_null = (op == "is_null");
			return leaf;
		}
		if (op == "is_nan" || op == "is_not_nan")
		{
			leaf.usable = true;
			leaf.is_nan_test = true;
			leaf.want_nan = (op == "is_nan");
			return leaf;
		}

		auto *file_metadata = reader_handle->reader->parquet_reader()->metadata().get();
		const parquet::ColumnDescriptor *descr = file_metadata->schema()->Column(leaf.leaf_index);

		// Gate (b): Parquet's own answer to "are this column's min/max usable at all", folding
		// together ColumnOrder and SortOrder exactly as the format specifies (including the legacy
		// undefined-order case, where the deprecated signed-only fields apply). Never hand-roll
		// the equivalent.
		//
		// This and gate (c) below are REDUNDANT AS A PAIR for every fixture this repository can
		// build, and each masks the other: removing this one alone changes no test result, because
		// (c) declines the same unsigned columns a few lines later. Removing (c) alone -- or both
		// -- is caught (test/test_filter_screen.f90's declined-unsigned-type case, whose unsigned
		// max reads back as -1 if compared signed). The redundancy is deliberate and worth keeping:
		// (c) only knows the two orderings this screen can read, while this one is the format's own
		// verdict, and it is what would catch a legacy file whose column order is UNDEFINED -- a
		// file parquet-cpp cannot write, hence the missing fixture. Same situation, and the same
		// resolution, as column_has_nulls_from_footer's is_stats_set()/statistics() pair.
		if (descr->sort_order() == parquet::SortOrder::UNKNOWN) return leaf;

		auto type_id = resolved.leaf_field->type()->id();
		switch (type_id)
		{
		case arrow::Type::INT8:
		case arrow::Type::INT16:
		case arrow::Type::INT32:
		case arrow::Type::INT64:
		// The unsigned widths the evaluator also compares as int64_t. They are listed here rather
		// than left to `default:` on purpose: their bounds are ordered UNSIGNED, so gate (c) below
		// declines them a few lines later -- and routing them through that gate, instead of past
		// it, is what makes the gate reachable by a real fixture (test/fixtures/extended_types.
		// parquet's v_uint32_ovf, whose unsigned max reads back as -1 if compared signed) rather
		// than defensive code no test can distinguish from a no-op.
		case arrow::Type::UINT8:
		case arrow::Type::UINT16:
		case arrow::Type::UINT32:
		case arrow::Type::DATE32:
		case arrow::Type::DATE64:
		case arrow::Type::TIME32:
		case arrow::Type::TIME64:
		case arrow::Type::TIMESTAMP:
			leaf.family = ScreenFamily::kInt;
			break;
		case arrow::Type::FLOAT:
		case arrow::Type::DOUBLE:
			leaf.family = ScreenFamily::kReal;
			leaf.is_float = true;
			break;
		case arrow::Type::BOOL:
			leaf.family = ScreenFamily::kBool;
			break;
		case arrow::Type::STRING:
		case arrow::Type::LARGE_STRING:
		case arrow::Type::STRING_VIEW:
			leaf.family = ScreenFamily::kString;
			break;
		default:
			// HALF_FLOAT (no typed statistics), UINT64 and DECIMAL* (the evaluator compares both as
			// double, while their statistics are unsigned/scale-encoded bytes), INT96 (legacy).
			// Declining costs only the optimization -- those filters behave exactly as before F4.
			return leaf;
		}

		// Gate (c): the bounds were aggregated under the column's declared sort order, and this
		// screen only knows how to read two of them -- signed for numbers and temporals, unsigned
		// byte order for strings, which is what compare_op<std::string> itself does. Anything else
		// (an unsigned integer column, say) declines rather than comparing under the wrong order.
		// BOOL is exempt: its "min/max" are just false/true, unambiguous under either order, and
		// only == and /= are legal on it anyway.
		parquet::SortOrder::type sort_order = descr->sort_order();
		if (leaf.family == ScreenFamily::kString)
		{
			if (sort_order != parquet::SortOrder::UNSIGNED) return leaf;
		}
		else if (leaf.family != ScreenFamily::kBool)
		{
			if (sort_order != parquet::SortOrder::SIGNED) return leaf;
		}

		// The literal, parsed exactly as eval_filter_clause parses it (same helpers, deliberately
		// -- a second parser here would be free to disagree on a boundary value). A literal this
		// cannot parse is not an error here: the evaluator will report it in a moment, with the
		// message and the column context that belong to it. Declining is the right response.
		switch (leaf.family)
		{
		case ScreenFamily::kInt:
			if (is_string || !parse_int64_strict(value_text, leaf.ival)) return leaf;
			break;
		case ScreenFamily::kReal:
			if (is_string || !parse_double_strict(value_text, leaf.dval)) return leaf;
			// A NaN literal is rejected by the evaluator (use is_nan instead), and comparing
			// against one here would be meaningless anyway.
			if (std::isnan(leaf.dval)) return leaf;
			break;
		case ScreenFamily::kBool:
		{
			if (is_string || (op != "==" && op != "/=")) return leaf;
			std::string lowered = ascii_to_lower(value_text);
			if (lowered == "true") leaf.bval = true;
			else if (lowered == "false") leaf.bval = false;
			else return leaf;
			break;
		}
		case ScreenFamily::kString:
			if (!is_string) return leaf;
			leaf.sval = value_text;
			break;
		default: // GCOVR_EXCL_LINE -- kNone returned above; every other value is handled.
			return leaf; // GCOVR_EXCL_LINE
		}

		leaf.usable = true;
		return leaf;
	}

	// What one resolved leaf can evaluate to somewhere in row group `rg`, from that row group's
	// column-chunk statistics alone. Reads no column data and cannot fail: every uncertainty is a
	// decline.
	static KleenePossible screen_leaf_in_row_group(ParquetReaderHandle *reader_handle,
		const ScreenLeaf &leaf, int64_t rg)
	{
		if (!leaf.usable) return kScreenAnything;
		auto *file_metadata = reader_handle->reader->parquet_reader()->metadata().get();
		auto chunk = file_metadata->RowGroup(static_cast<int>(rg - 1))->ColumnChunk(leaf.leaf_index);

		// Gate (a). The is_stats_set() test and the null-statistics() test are load-bearing as a
		// PAIR and each masks the other individually -- exactly as column_has_nulls_from_footer
		// records for its own copy: removing either alone changes no test result, while removing
		// both segfaults on test/fixtures/no_stats.parquet, where statistics() returns null. Do not
		// drop one on the strength of a coverage report. HasNullCount() is required for every rule
		// (not just the null tests): every rule below reads num_values/null_count, and a statistics
		// object missing the count is one this screen has no reason to trust the rest of.
		if (!chunk->is_stats_set()) return kScreenAnything;
		auto stats = chunk->statistics();
		if (!stats || !stats->HasNullCount()) return kScreenAnything;

		int64_t nn = stats->num_values();
		int64_t nc = stats->null_count();

		if (leaf.is_null_test)
		{
			// Never unknown -- matching eval_filter_clause, whose first branch answers true/false
			// for every row including null ones. That is what makes these two the only way to
			// select a Null row.
			KleenePossible res{false, false, false};
			res.may_true = leaf.want_null ? (nc > 0) : (nn > 0);
			res.may_false = leaf.want_null ? (nn > 0) : (nc > 0);
			return res;
		}
		if (leaf.is_nan_test)
		{
			// Parquet records no NaN count and excludes NaN from min/max, so neither "this chunk
			// contains a NaN" nor "it contains none" is ever provable. nn > 0 is the only thing
			// that can be said: a chunk with no non-null values has every row unknown for both
			// operators, and is prunable for that reason alone.
			KleenePossible res{nn > 0, nn > 0, nc > 0};
			return res;
		}

		if (!stats->HasMinMax()) return kScreenAnything;

		// Gate (d): a writer may truncate a long BYTE_ARRAY min/max. Truncation must preserve
		// BOUNDEDNESS (a truncated min is still <= every value, a truncated max still >=), so every
		// range rule stays sound -- what it breaks is the one place a bound is used as a proof of
		// EQUALITY (min == max == v proving every value equals v), which is the `exact` term in
		// == and /=. An unset optional means "possibly truncated".
		//
		// NO FIXTURE THIS REPOSITORY CAN BUILD REACHES A NON-EXACT BOUND, and that is a property of
		// the writer, not a gap in the tests: parquet-cpp does not truncate at all. Confirmed in
		// EncodedStatistics::ApplyStatSizeLimits (parquet/statistics.h), which DROPS a bound longer
		// than max_statistics_size (4096 by default) -- clearing has_min/has_max and setting the
		// exact flag to nullopt -- precisely so no consumer can mistake a truncated bound for a
		// real one. Such a chunk therefore fails HasMinMax() above and declines before reaching
		// here (covered: the long-string case in test/test_filter_screen.f90). Only a file from a
		// writer that does truncate (parquet-mr, which sets these flags for exactly this purpose)
		// can produce one, so this term is here for foreign files and cannot be mutation-tested
		// with a fixture built here. Do not delete it as dead code on the strength of that.
		auto min_exact = stats->is_min_value_exact();
		auto max_exact = stats->is_max_value_exact();
		bool exact = min_exact.has_value() && max_exact.has_value() && *min_exact && *max_exact;

		int cmp_lo = 0;
		int cmp_hi = 0;
		switch (leaf.family)
		{
		case ScreenFamily::kInt:
		{
			int64_t lo = 0;
			int64_t hi = 0;
			if (stats->physical_type() == parquet::Type::INT32)
			{
				auto typed = std::static_pointer_cast<parquet::Int32Statistics>(stats);
				lo = typed->min();
				hi = typed->max();
			}
			else if (stats->physical_type() == parquet::Type::INT64)
			{
				auto typed = std::static_pointer_cast<parquet::Int64Statistics>(stats);
				lo = typed->min();
				hi = typed->max();
			}
			else
			{ // GCOVR_EXCL_START -- an Arrow integer/temporal leaf is always physically INT32/INT64
				return kScreenAnything;
			}
			// GCOVR_EXCL_STOP
			cmp_lo = (lo < leaf.ival) ? -1 : (lo > leaf.ival ? 1 : 0);
			cmp_hi = (hi < leaf.ival) ? -1 : (hi > leaf.ival ? 1 : 0);
			break;
		}
		case ScreenFamily::kReal:
		{
			double lo = 0.0;
			double hi = 0.0;
			if (stats->physical_type() == parquet::Type::FLOAT)
			{
				auto typed = std::static_pointer_cast<parquet::FloatStatistics>(stats);
				lo = static_cast<double>(typed->min());
				hi = static_cast<double>(typed->max());
			}
			else if (stats->physical_type() == parquet::Type::DOUBLE)
			{
				auto typed = std::static_pointer_cast<parquet::DoubleStatistics>(stats);
				lo = typed->min();
				hi = typed->max();
			}
			else
			{ // GCOVR_EXCL_START -- an Arrow FLOAT/DOUBLE leaf is always physically FLOAT/DOUBLE
				return kScreenAnything;
			}
			// GCOVR_EXCL_STOP
			// A NaN bound would mean the writer wrote one despite the format excluding NaN; every
			// comparison against it is false, which would make both cmp values 0 and read as
			// "constant at v". Decline instead.
			if (std::isnan(lo) || std::isnan(hi)) return kScreenAnything; // GCOVR_EXCL_LINE -- gcov attribution artifact under GCC: the condition is evaluated for every float leaf, so the line shows hits, but the return is never taken (no writer records a NaN bound)
			cmp_lo = (lo < leaf.dval) ? -1 : (lo > leaf.dval ? 1 : 0);
			cmp_hi = (hi < leaf.dval) ? -1 : (hi > leaf.dval ? 1 : 0);
			break;
		}
		case ScreenFamily::kBool:
		{
			if (stats->physical_type() != parquet::Type::BOOLEAN)
			{ // GCOVR_EXCL_START -- an Arrow BOOL leaf is always physically BOOLEAN
				return kScreenAnything;
			}
			// GCOVR_EXCL_STOP
			auto typed = std::static_pointer_cast<parquet::BoolStatistics>(stats);
			int lo = typed->min() ? 1 : 0;
			int hi = typed->max() ? 1 : 0;
			int v = leaf.bval ? 1 : 0;
			cmp_lo = (lo < v) ? -1 : (lo > v ? 1 : 0);
			cmp_hi = (hi < v) ? -1 : (hi > v ? 1 : 0);
			break;
		}
		case ScreenFamily::kString:
		{
			if (stats->physical_type() != parquet::Type::BYTE_ARRAY)
			{ // GCOVR_EXCL_START -- an Arrow string leaf is always physically BYTE_ARRAY
				return kScreenAnything;
			}
			// GCOVR_EXCL_STOP
			auto typed = std::static_pointer_cast<parquet::ByteArrayStatistics>(stats);
			std::string lo(reinterpret_cast<const char *>(typed->min().ptr), typed->min().len);
			std::string hi(reinterpret_cast<const char *>(typed->max().ptr), typed->max().len);
			int lo_cmp = lo.compare(leaf.sval);
			int hi_cmp = hi.compare(leaf.sval);
			cmp_lo = (lo_cmp < 0) ? -1 : (lo_cmp > 0 ? 1 : 0);
			cmp_hi = (hi_cmp < 0) ? -1 : (hi_cmp > 0 ? 1 : 0);
			break;
		}
		default: // GCOVR_EXCL_LINE -- kNone leaves are never usable, and returned above.
			return kScreenAnything; // GCOVR_EXCL_LINE
		}

		return screen_compare_from_bounds(leaf.op, cmp_lo, cmp_hi, nn, nc, exact, leaf.is_float);
	}

	// Whether the screen runs at all is a SETTING (parquet_set_statistics_prescreen), read as
	// g_statistics_prescreen below. Turning it off forces every row group live, so the same fixture
	// can be read with and without pruning in one test and the two results compared element for
	// element -- equality alone is what proves the optimization changed no answer. Mirrors
	// parquet_set_sort_counting_path, which exists for exactly the same reason: a second code path
	// that must produce the same result as the first.

	// Test-only: how many row groups the most recent screen_row_groups call ruled out -- see
	// parquet_debug_get_row_groups_pruned, far below, for why this is process-global rather than
	// read off the handle.
	static int64_t g_debug_row_groups_pruned = 0;

	// Walks the postfix node list once per row group over KleenePossible triples instead of one
	// row vector per leaf, and writes reader_handle->row_group_live (plus row_groups_pruned).
	// Reads no column data, allocates nothing per row, and cannot fail.
	//
	// For a SCOPED filter, row groups outside [rg_lo, rg_hi] are marked not-live too: the mask is
	// all-false there by construction, so a later whole-column read may skip them for the same
	// reason it may skip a screened-out one.
	static void screen_row_groups(ParquetReaderHandle *reader_handle,
		const std::vector<ScreenLeaf> &leaves,
		const int8_t *node_kind, const int32_t *node_leaf, int64_t n_nodes,
		int64_t rg_lo, int64_t rg_hi)
	{
		reader_handle->row_group_live.assign(static_cast<size_t>(reader_handle->num_row_groups), 1);
		reader_handle->row_groups_pruned = 0;
		g_debug_row_groups_pruned = 0;
		if (!g_statistics_prescreen) return;

		for (int64_t rg = 1; rg <= reader_handle->num_row_groups; ++rg)
		{
			if (rg_lo > 0 && (rg < rg_lo || rg > rg_hi))
			{
				reader_handle->row_group_live[static_cast<size_t>(rg - 1)] = 0;
				++reader_handle->row_groups_pruned;
				continue;
			}
			// A clause-less call (a bare row/row-group range, carrying only a slice's own bounds --
			// see parquet_reader_set_filter's row_lo/row_hi) has no expression to screen with, so
			// the scope check above is the whole screen. Explicit rather than falling into the
			// stack machine below, whose "did not end with exactly one result" arm is a genuine
			// malformed-input assertion and must stay unreachable.
			if (n_nodes <= 0) continue;
			// The same postfix walk evaluate_nodes performs, over one triple per stack slot
			// instead of one row vector. The structural identity is deliberate.
			std::vector<KleenePossible> stack;
			bool malformed = false;
			for (int64_t k = 0; k < n_nodes; ++k)
			{
				int kind = static_cast<int>(node_kind[k]);
				if (kind == 1) // leaf
				{
					int li = static_cast<int>(node_leaf[k]) - 1;
					if (li < 0 || li >= static_cast<int>(leaves.size()))
					{ // GCOVR_EXCL_START -- malformed node list; unreachable from
					  // parquet_parse_filter_expr, which emits the leaf before its own node.
						malformed = true;
						break;
					}
					// GCOVR_EXCL_STOP
					stack.push_back(screen_leaf_in_row_group(reader_handle, leaves[static_cast<size_t>(li)], rg));
				}
				else if (kind == 4) // not
				{
					stack.back() = screen_negate(stack.back());
				}
				else // and (2) / or (3)
				{
					KleenePossible rhs = stack.back();
					stack.pop_back();
					stack.back() = screen_combine(stack.back(), rhs, kind == 2);
				}
			}
			if (malformed || stack.size() != 1)
			{ // GCOVR_EXCL_START -- unreachable through the public API, see above. Declining to
			  // prune is the right response even here.
				continue;
			}
			// GCOVR_EXCL_STOP
			if (!stack.front().may_true)
			{
				reader_handle->row_group_live[static_cast<size_t>(rg - 1)] = 0;
				++reader_handle->row_groups_pruned;
			}
		}
		g_debug_row_groups_pruned = reader_handle->row_groups_pruned;
	}

	// Validates and applies one filter EXPRESSION to this reader. The expression arrives as `n`
	// packed leaves (one per clause) plus `n_nodes` postfix nodes over them (node_kind: 1=leaf,
	// 2=and, 3=or, 4=not; node_leaf: 1-based leaf index for a leaf, 0 otherwise), built by
	// parquet_parse_filter_expr (parquet_read_filter.f90). Every referenced column must exist and
	// be a plain scalar column (col_size == 1; a vector/list column always fails, regardless of
	// its size).
	//
	// Evaluation is a stack machine over the node list, with Kleene three-valued logic per row
	// (see kFalse/kTrue/kUnknown): each leaf is evaluated once into its own row vector, and the
	// combinators fold the stack. For a flat AND-only expression over non-null data the result is
	// bit-identical to the AND-only implementation this replaced, which is what makes every
	// pre-existing filter test a regression check on the rewrite.
	//
	// Two ways the row set can already be narrowed when this runs, both folded in at the END
	// (never as the starting value of the expression's own evaluation -- a sample zero must not be
	// indistinguishable from an evaluated false, or an OR could resurrect a non-sampled row):
	//   - has_pending_sample: parquet_reader_set_sample deferred its draw for this call (the
	//     parquet_open_reader path -- see has_pending_sample's own comment).
	//   - an already-installed sample-only mask: the post-open path
	//     (parquet_reader_set_filter in parquet.f90), where the sample was installed at open time.
	//     It is uninstalled here so clause evaluation still reads raw, unmasked columns; safe
	//     because that path refuses to run once any column has been decoded.
	//
	// On success, updates nrows to the filtered row count, stores the resulting mask on the handle
	// (so every column decoded from here on -- via get_single_chunk_array or
	// parquet_reader_prefetch_columns -- is filtered to just the matching rows), and
	// re-filters/updates column_cache for every filter column itself (already decoded above, as a
	// side effect of evaluating its own clause) so it's consistent with every other column.
	// Returns 0 on success; on failure, returns 1 and writes a human-readable reason into err_out
	// (truncated to err_cap).
	// Adds the elapsed time since `t0` to one of the phase counters above, and returns a fresh mark.
	// A function rather than a macro so it can be stepped through, and taking the counter by
	// reference so a new phase costs one line at the call site.
	extern "C++" {
	static std::chrono::steady_clock::time_point charge_phase(std::chrono::steady_clock::time_point t0,
		int64_t &counter)
	{
		auto now = std::chrono::steady_clock::now();
		counter += std::chrono::duration_cast<std::chrono::nanoseconds>(now - t0).count();
		return now;
	}
	}

	int64_t parquet_reader_set_filter(void *handle,
		const char *names_packed, int64_t name_len,
		const char *ops_packed, int64_t op_len,
		const char *values_packed, int64_t value_len,
		const int8_t *is_string_flags,
		int64_t n,
		const int8_t *node_kind, const int32_t *node_leaf, int64_t n_nodes,
		const char *expr_text,
		int64_t rg_lo, int64_t rg_hi,
		int64_t row_lo, int64_t row_hi,
		char *err_out, int64_t err_cap)
	{
		auto reader_handle = as_reader_handle(handle);

		// rg_lo = -1 means "the caller named no row-group range at all" -- the SENTINEL the Fortran
		// side passes from parquet_open_reader(..., filter=) and from the two-argument
		// parquet_reader_set_filter(reader, filter). Any other value scopes the filter to an
		// inclusive, 1-based row-group range, with rg_lo <= 0 meaning "all row groups" (and rg_hi
		// then ignored), matching parquet_measure_list_width and parquet_column_has_nulls, whose
		// row-group arguments have always read a non-positive lower bound that way.
		//
		// So it is the PRESENCE of the arguments that picks the engine and their VALUE that picks
		// the row groups -- which is the whole reason for the sentinel. Inferring "unscoped" from
		// the value 0, as this did before, made parquet_reader_set_filter(reader, filt, 0, 0)
		// impossible to express: it is a bounded-memory filter over the whole file, and it used to
		// be indistinguishable from the caching whole-file form. The two paths differ in more than
		// which rows they look at:
		//
		//   unscoped -- every filter column is read in one batched, thread-parallel call (issued
		//     here, further below) covering the LIVE row groups, and left decoded in column_cache,
		//     so a later read of that same column costs nothing. Fastest, and the right default;
		//     memory is one copy of each filter column's live rows.
		//
		//   scoped -- the expression is evaluated row group by row group over the range, reading
		//     each leaf's chunk with ReadRowGroup and discarding it before moving on. Peak memory
		//     is one row group's worth of the filter columns instead of the whole file, which is
		//     what makes a filtered read possible on a file larger than memory. The cost is that
		//     nothing lands in column_cache, so a filter column read afterwards is read again.
		//
		// Rows outside a scoped range never match: no bits are held for those row groups at all, so
		// the reader presents exactly the surviving rows of the chosen row groups and nothing else.
		//
		// row_lo/row_hi = 0 means "every row of the chosen row groups"; otherwise rows outside that
		// inclusive, 1-based PHYSICAL row range never match either. This is a strictly finer cut
		// than the row-group range, and it is what lets a parquet_table slice whose bounds fall
		// INSIDE a row group express itself as a filter: without it the reader would hand back the
		// whole covering row groups' survivors, and the table's own physical-row arithmetic and the
		// reader's post-filter chunks would be in two different coordinate systems.
		bool scoped = (rg_lo != -1);
		if (scoped)
		{
			if (rg_lo <= 0)
			{
				// "All row groups", bounded-memory engine. rg_hi is deliberately ignored rather
				// than validated: a caller who has not looked up the row-group count has nothing
				// sensible to put there, and requiring parquet_get_num_row_groups first is exactly
				// the friction this form removes.
				rg_lo = 1;
				rg_hi = reader_handle->num_row_groups;
			}
			else if (rg_lo < 1 || rg_hi < rg_lo || rg_hi > reader_handle->num_row_groups)
			{
				std::snprintf(err_out, static_cast<size_t>(err_cap),
					"filter row-group range %lld..%lld is out of range (file has %lld row group(s))",
					static_cast<long long>(rg_lo), static_cast<long long>(rg_hi),
					static_cast<long long>(reader_handle->num_row_groups));
				return 1;
			}
		}
		bool row_ranged = (row_lo > 0 || row_hi > 0);
		if (row_ranged)
		{
			if (row_lo < 1 || row_hi < row_lo || row_hi > reader_handle->total_nrows)
			{
				std::snprintf(err_out, static_cast<size_t>(err_cap),
					"filter row range %lld..%lld is out of range (file has %lld row(s))",
					static_cast<long long>(row_lo), static_cast<long long>(row_hi),
					static_cast<long long>(reader_handle->total_nrows));
				return 1;
			}
			// The row range must lie INSIDE the rows the chosen row groups span. Without this the
			// caller silently receives the intersection of the two, which for a disjoint pair is
			// empty -- and an empty result is indistinguishable from a selective filter that
			// matched nothing, so the mistake reports as data rather than as an error. Checked
			// after the resolution above, so "all row groups" spans the whole file and can never
			// fail it. (feature_risks.md Risk-81)
			if (scoped)
			{
				int64_t span_lo = reader_handle->row_group_offsets[static_cast<size_t>(rg_lo - 1)] + 1;
				int64_t span_hi = reader_handle->row_group_offsets[static_cast<size_t>(rg_hi)];
				if (row_lo < span_lo || row_hi > span_hi)
				{
					std::snprintf(err_out, static_cast<size_t>(err_cap),
						"filter row range %lld..%lld is not contained in row groups %lld..%lld, "
						"which span rows %lld..%lld",
						static_cast<long long>(row_lo), static_cast<long long>(row_hi),
						static_cast<long long>(rg_lo), static_cast<long long>(rg_hi),
						static_cast<long long>(span_lo), static_cast<long long>(span_hi));
					return 1;
				}
			}
		}
		// With no clauses AND nothing to scope to, there is simply nothing to install. With no
		// clauses but a scope or a row range, there is: an all-true-within-range mask, which is how
		// a slice-regime table with sample_fraction= but no filter= carries its own row range. The
		// expression machinery below is skipped entirely in that case (n_nodes is 0, so there is
		// nothing for the screen or the stack machine to walk).
		if (n <= 0 && !scoped && !row_ranged) return 0;

		// The row sample that has to be folded in, if any, and where it comes from. Both forms are
		// applied once at the very end, after unknown has collapsed to false -- never as the
		// starting value of the expression's own evaluation, or an OR could resurrect a
		// non-sampled row.
		std::vector<uint8_t> sample_keep;
		std::shared_ptr<arrow::BooleanArray> prior_mask;
		std::vector<int64_t> prior_offsets;
		if (reader_handle->has_pending_sample)
		{
			// parquet_reader_set_sample ran first and stashed its caller-built mask here (see
			// has_pending_sample's own comment), so that every column read below is still
			// raw/unfiltered while clauses are evaluated. It spans the file's PHYSICAL rows.
			sample_keep = std::move(reader_handle->pending_sample_keep);
			reader_handle->pending_sample_keep.clear();
			reader_handle->has_pending_sample = false;
		}
		else if (reader_handle->live_mask)
		{
			// The post-open path: a sample-only mask is already installed (there are no clauses
			// yet -- parquet_reader_set_filter in parquet.f90 refuses an already-filtered reader).
			// Keep it, with its own live-row layout, and uninstall it from the handle so clause
			// evaluation sees raw columns; nothing has been decoded under it, since that same path
			// refuses to run after any column has been read. Retaining the array rather than
			// expanding it into a flat per-row vector is what keeps this path O(live rows) too.
			prior_mask = reader_handle->live_mask;
			prior_offsets = reader_handle->row_group_live_offsets;
			reader_handle->live_mask.reset();
			reader_handle->row_group_live_offsets.clear();
			reader_handle->nrows = reader_handle->total_nrows;
		}
		std::vector<int> touched_indices;
		std::vector<std::string> touched_names; // every filter clause's own (possibly dotted) name, deduplicated
		// Each leaf's validated inputs and its already-decoded array, in leaf order. Resolved in
		// the loop below (so an unknown/vector column is still reported in the order the clauses
		// were written) and evaluated afterwards in NODE order, which is what the expression's
		// shape dictates. The arrays are the same objects column_cache holds, so keeping them here
		// costs no extra memory.
		std::vector<std::string> leaf_names, leaf_ops, leaf_values;
		std::vector<bool> leaf_is_string;
		std::vector<std::shared_ptr<arrow::Array>> leaf_arrays;

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

			// No column data is read anywhere in this loop, on either path -- validation has to
			// complete before the statistics screen runs, and the screen has to run before
			// anything is read, or there would be nothing left to prune. The vector-column
			// rejection therefore comes from the schema rather than from a decoded array; every
			// other per-value check (type supported, value parses) happens inside
			// eval_filter_clause, once the data it needs is in hand.
			auto leaf_type = resolve_struct_path(reader_handle->schema, name).leaf_field->type()->id();
			if (leaf_type == arrow::Type::FIXED_SIZE_LIST || leaf_type == arrow::Type::LIST ||
				leaf_type == arrow::Type::LARGE_LIST)
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

			// Retain this clause (operator+value, column name stripped) for the
			// print_stat "filter" column; multiple clauses on one column are kept
			// in order and joined with ", " at print time. Keyed by the physical
			// top-level column index -- two clauses on different leaves of the
			// same struct show up merged under that struct's one print_stat row
			// (see CLAUDE.md's nested-struct-field design notes; an accepted v1
			// limitation, same as was_read/output_type_used above). For an
			// expression that is not a flat AND this per-column view is lossy by
			// construction, which is what filter_expr_text (printed as its own
			// line) exists to cover.
			reader_handle->filter_clauses[idx].push_back(value.empty() ? op : op + value);

			leaf_names.push_back(name);
			leaf_ops.push_back(op);
			leaf_values.push_back(value);
			leaf_is_string.push_back(is_string);
		}

		// The statistics pre-screen: the first thing that happens after validation and BEFORE any
		// column data is read, which is what makes the reads below skippable at all. It writes
		// row_group_live and nothing else, reads no data, and cannot fail -- every uncertainty is
		// a decline. Everything downstream then treats a pruned row group exactly as a row group
		// with no surviving rows, which F2/F2b already handle everywhere.
		{
			std::vector<ScreenLeaf> screen_leaves;
			screen_leaves.reserve(leaf_names.size());
			for (size_t li = 0; li < leaf_names.size(); ++li)
			{
				screen_leaves.push_back(resolve_screen_leaf(reader_handle, leaf_names[li], leaf_ops[li],
					leaf_is_string[li], leaf_values[li]));
			}
			screen_row_groups(reader_handle, screen_leaves, node_kind, node_leaf, n_nodes, rg_lo, rg_hi);
		}

		// The unscoped path's filter-column read, moved here from Fortran (it used to be
		// prefetch_filter_columns in parquet_read.f90, issued before this call). It has to be here
		// rather than there: the screen needs the parsed expression, which only reaches C++ in
		// this call, so a read issued earlier would already have spent the I/O the screen exists
		// to save. Moving it also removes the double parse that arrangement needed -- every rule
		// was parsed once to collect the column names and again to build the node list.
		//
		// Still ONE batched, thread-parallel Arrow call over every distinct filter column, exactly
		// as before; read_live_row_groups issues it over the surviving row groups instead of the
		// whole file. Deliberately NOT parquet_reader_prefetch_columns: that runs read-time qc,
		// and qc must see the FILTERED rows, which do not exist yet -- it runs at the end of this
		// function instead, on the columns cached here.
		if (!scoped && !touched_indices.empty())
		{
			if (g_debug_force_whole_column_read_error)
			{
				report_fatal_error("parquet_reader_set_filter",
					"forced debug error: whole-column filter read attempted"); // GCOVR_EXCL_LINE
			}
			std::vector<int> filter_leaf_indices;
			for (int idx : touched_indices)
			{
				collect_leaf_indices(reader_handle->manifest.schema_fields[static_cast<size_t>(idx)],
					filter_leaf_indices);
			}
			std::shared_ptr<arrow::Table> table;
			auto t_dec = std::chrono::steady_clock::now();
			try
			{
				table = read_live_row_groups(reader_handle, filter_leaf_indices);
			}
			// GCOVR_EXCL_START -- the only throw reachable here is read_live_row_groups' own
			// file-I/O backstop, itself excluded for want of a fixture that triggers it. The catch
			// clause is included in the exclusion (not just its body): under GCC a catch clause no
			// test enters shows uncovered in its own right, distinct from Clang's gcov.
			catch (const std::exception &e)
			{
				std::snprintf(err_out, static_cast<size_t>(err_cap), "failed to read filter columns: %s", e.what());
				return 1;
			}
			// GCOVR_EXCL_STOP
			charge_phase(t_dec, g_debug_filter_decode_nanos);
			for (int idx : touched_indices)
			{
				const std::string &top_name = reader_handle->schema->field(idx)->name();
				auto result_pos = table->schema()->GetFieldIndex(top_name);
				// No apply_row_transform: there is no mask yet, and installing one before the
				// clauses are evaluated is exactly what the deferred sample draw exists to prevent.
				reader_handle->column_cache[idx] = combine_column_chunks(table->column(result_pos), top_name);
			}
		}
		if (!scoped)
		{
			for (const auto &leaf_name : leaf_names)
			{
				auto resolved = resolve_struct_path(reader_handle->schema, leaf_name);
				auto idx = get_column_index(reader_handle, resolved.top_level_name.c_str());
				auto array = reader_handle->column_cache.at(static_cast<int>(idx));
				if (!resolved.child_path.empty()) array = unwrap_struct_path(array, resolved.child_path);
				leaf_arrays.push_back(array);
			}
		}

		// Evaluates the postfix node list over one set of per-leaf arrays (all the same length),
		// writing one Kleene value per row into `out`. A stack of row vectors: a leaf pushes its
		// own result, `not` negates the top in place, and `and`/`or` fold the top two into one.
		// Peak memory is (deepest simultaneous operand count) * rows bytes, which the parser's own
		// nesting cap (filter_max_depth, parquet.f90) bounds; a flat chain of any length keeps
		// exactly one vector live. Shared by both paths: the unscoped one calls it once over the
		// whole-file arrays, the scoped one once per row group over that row group's chunks.
		auto evaluate_nodes = [&](const std::vector<std::shared_ptr<arrow::Array>> &arrays, size_t rows,
			std::vector<uint8_t> &out, std::string &err) -> bool
		{
			std::vector<std::vector<uint8_t>> stack;
			for (int64_t k = 0; k < n_nodes; ++k)
			{
				int kind = static_cast<int>(node_kind[k]);
				if (kind == 1) // leaf
				{
					int li = static_cast<int>(node_leaf[k]) - 1;
					if (li < 0 || li >= static_cast<int>(arrays.size()))
					{ // GCOVR_EXCL_START -- malformed node list; unreachable from
					  // parquet_parse_filter_expr, which emits the leaf before its own node.
						err = "malformed expression";
						return false;
					}
					// GCOVR_EXCL_STOP
					std::vector<uint8_t> leaf_result(rows);
					if (!eval_filter_clause(arrays[static_cast<size_t>(li)], leaf_names[static_cast<size_t>(li)],
						leaf_ops[static_cast<size_t>(li)], leaf_is_string[static_cast<size_t>(li)],
						leaf_values[static_cast<size_t>(li)], leaf_result, err))
					{
						return false;
					}
					stack.push_back(std::move(leaf_result));
				}
				else if (kind == 4) // not
				{
					kleene_negate(stack.back());
				}
				else // and (2) / or (3)
				{
					std::vector<uint8_t> rhs = std::move(stack.back());
					stack.pop_back();
					kleene_combine(stack.back(), rhs, kind == 2);
				}
			}
			// One expression always leaves exactly one result on the stack; anything else means
			// the node list did not come from parquet_parse_filter_expr.
			if (stack.size() != 1)
			{ // GCOVR_EXCL_START -- unreachable through the public API, see above.
				err = "malformed expression";
				return false;
			}
			// GCOVR_EXCL_STOP
			out = std::move(stack.front());
			return true;
		};

		// The live-row layout the mask is about to be built in: one slot per row of every row group
		// the screen left live, and NOTHING at all for the rest. An excluded row group -- pruned by
		// statistics, or outside a scoped range -- therefore costs zero bytes here rather than a
		// run of all-false bits, which is what makes a slice-scoped filter on a huge file hold a
		// mask proportional to its own slice instead of to total_nrows.
		int64_t live_rows = assign_row_group_live_offsets(reader_handle);
		// All-false to start. With clauses, every live row is written below on both paths, so this
		// is defensive; with none (a bare range) it is the value the range itself overrides.
		std::vector<uint8_t> combined(static_cast<size_t>(live_rows), n > 0 ? kFalse : kTrue);
		std::string eval_err;
		if (n <= 0)
		{
			// A clause-less call: nothing to evaluate, and `combined` is already all-true. The row
			// range (and the row-group scope, already applied by the screen) is folded in below,
			// exactly as it would be for a call that did have clauses.
		}
		else if (scoped)
		{
			for (int64_t rg = rg_lo; rg <= rg_hi; ++rg)
			{
				// A pruned row group's segment is provably all-false, so evaluating it would read
				// a row group's worth of every filter column to confirm what the footer already
				// proved. This is the scoped path's entire share of the F4 saving.
				if (reader_handle->row_group_live[static_cast<size_t>(rg - 1)] == 0) continue;
				int64_t rows = row_group_rows(reader_handle, rg);
				int64_t offset = reader_handle->row_group_live_offsets[static_cast<size_t>(rg - 1)];
				// This row group's chunk of every leaf column, read and then released with the
				// vector when the iteration ends -- read_row_group_array_for_measuring rather than
				// get_row_group_chunk_array, so measuring the filter does not mark the row group
				// read for parquet_reader_check_complete or fire qc on rows the caller has not
				// asked for yet (qc runs when the column is actually read, on filtered rows).
				std::vector<std::shared_ptr<arrow::Array>> rg_arrays;
				rg_arrays.reserve(leaf_names.size());
				auto t_rg = std::chrono::steady_clock::now();
				for (const auto &leaf_name : leaf_names)
				{
					rg_arrays.push_back(read_row_group_array_for_measuring(reader_handle, leaf_name.c_str(), rg));
				}
				t_rg = charge_phase(t_rg, g_debug_filter_decode_nanos);
				std::vector<uint8_t> local;
				if (!evaluate_nodes(rg_arrays, static_cast<size_t>(rows), local, eval_err))
				{
					std::snprintf(err_out, static_cast<size_t>(err_cap), "filter rule: %s", eval_err.c_str());
					return 1;
				}
				charge_phase(t_rg, g_debug_filter_eval_nanos);
				for (int64_t i = 0; i < rows; ++i)
				{
					combined[static_cast<size_t>(offset + i)] = local[static_cast<size_t>(i)];
				}
			}
		}
		else
		{
			// The unscoped path evaluates ONCE over the concatenated live rows -- one pass, one
			// vector per stack slot, exactly as before. Its result is already in live-row order and
			// live-row length, which is now exactly the mask's own layout, so it moves straight in
			// rather than being scattered by row-group offset the way it used to be.
			std::vector<uint8_t> local;
			auto t_ev = std::chrono::steady_clock::now();
			if (!evaluate_nodes(leaf_arrays, static_cast<size_t>(live_rows), local, eval_err))
			{
				// Tag the clause-level message so it is unambiguously a row-filter error (vs a
				// read-time qc check, which labels its own messages). The other set_filter
				// failures (unknown/vector/read-fail column) already say "filter" themselves.
				std::snprintf(err_out, static_cast<size_t>(err_cap), "filter rule: %s", eval_err.c_str());
				return 1;
			}
			charge_phase(t_ev, g_debug_filter_eval_nanos);
			combined = std::move(local);
		}

		// Collapse unknown to false, apply the row range, and fold in whatever already narrowed the
		// row set -- once, here, in one walk. This is the single point where three-valued logic
		// becomes the two-valued mask Arrow needs, and it is why a Null row never survives without
		// an explicit is_null clause.
		//
		// The walk is over PHYSICAL rows, row group by row group, because two of the things folded
		// in here are addressed physically: the row range compares against the file's own row
		// numbers, and sample_keep is indexed by physical row. An excluded row group has no slot in
		// `combined` and is simply stepped over -- which it could NOT be while the sample was drawn
		// by a sequential engine, since skipping a row group would then have shifted every later
		// row's draw. keep[i] now depends only on (seed, i), so nothing here can move a decision.
		auto t_mask = std::chrono::steady_clock::now();
		{
			const bool have_sample = !sample_keep.empty();
			for (int64_t rg = 1; rg <= reader_handle->num_row_groups; ++rg)
			{
				int64_t rows = row_group_rows(reader_handle, rg);
				int64_t first_row = reader_handle->row_group_offsets[static_cast<size_t>(rg - 1)];
				int64_t live_off = reader_handle->row_group_live_offsets[static_cast<size_t>(rg - 1)];
				int64_t prior_off = prior_mask ? prior_offsets[static_cast<size_t>(rg - 1)] : -1;
				if (live_off < 0) continue; // excluded row group: no slots to write
				for (int64_t i = 0; i < rows; ++i)
				{
					size_t slot = static_cast<size_t>(live_off + i);
					bool keep = combined[slot] == kTrue;
					if (keep && row_ranged) keep = (first_row + i + 1 >= row_lo && first_row + i + 1 <= row_hi);
					if (keep && have_sample) keep = sample_keep[static_cast<size_t>(first_row + i)] != 0;
					if (keep && prior_mask) keep = prior_mask->Value(prior_off + i);
					combined[slot] = keep ? 1 : 0;
				}
			}
		}

		if (!install_row_mask(reader_handle, combined, err_out, err_cap)) return 1; // GCOVR_EXCL_LINE
		// Retained for parquet_reader_print_stat's "filter:" line only (never parsed here).
		if (expr_text != nullptr) reader_handle->filter_expr_text = expr_text;

		// Every filter column was decoded (and cached) above, before
		// the mask existed -- re-filter those specific cache entries now
		// so they're consistent with every other column, which will only
		// ever see the filtered version (via apply_row_transform, from here on).
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
			auto coerced = coerce_string_view_to_offset_string(it->second);
			if (!coerced.ok())
			{ // GCOVR_EXCL_START -- Cast-kernel Status backstop on already-validated input
				std::snprintf(err_out, static_cast<size_t>(err_cap), "failed to apply filter: %s", coerced.status().ToString().c_str());
				return 1;
			}
			// GCOVR_EXCL_STOP
			// live_mask: these cached arrays came from read_live_row_groups above, so they span
			// the live row groups' rows -- exactly the layout the mask itself is built in.
			auto filtered = arrow::compute::Filter(coerced.ValueOrDie(), reader_handle->live_mask);
			if (!filtered.ok())
			{ // GCOVR_EXCL_START -- Filter-kernel Status backstop on already-validated input
				std::snprintf(err_out, static_cast<size_t>(err_cap), "failed to apply filter: %s", filtered.status().ToString().c_str());
				return 1;
			}
			// GCOVR_EXCL_STOP
			it->second = filtered.ValueOrDie().make_array();
			reader_handle->was_prefetched.insert(idx);
		}

		charge_phase(t_mask, g_debug_filter_mask_nanos);

		// Read-time qc on the filter columns themselves. Only the unscoped path can do this here:
		// it is the one that leaves those columns decoded in column_cache. Under a scoped filter
		// nothing is retained, so a filter column's qc runs when that column is actually read --
		// per row group, on filtered rows (get_row_group_chunk_array), which is the same "qc after
		// filtering" rule, just deferred to the read that will happen anyway.
		if (!scoped)
		{
			for (const auto &touched_name : touched_names)
			{
				auto resolved = resolve_struct_path(reader_handle->schema, touched_name);
				auto idx = static_cast<int>(get_column_index(reader_handle, resolved.top_level_name.c_str()));
				auto array = reader_handle->column_cache.at(idx);
				if (!resolved.child_path.empty()) array = unwrap_struct_path(array, resolved.child_path);
				run_qc_checks(reader_handle, touched_name, touched_name, array);
			}
		}

		return 0;
	}


	// Installs a read-time sort on an open reader: every column read from here on comes back in
	// key order, and so does every column already decoded (re-Taken below, exactly as
	// parquet_reader_set_filter re-Filters). Returns 0 on success, or 1 with a message in `err_out`
	// -- the same error-by-string convention set_filter uses, so the Fortran side owns the
	// error stop text.
	//
	// `names_packed` is `n` fixed-width `name_len` column names (blank/NUL padded), `descending`
	// and `nulls_first` one int8 flag each per key, in the order the caller added them. `key_text`
	// is the whole key list re-rendered for parquet_reader_print_stat and never parsed here.
	//
	// Runs AFTER any filter/sample mask is installed, which is what makes "filter first, then sort
	// within the survivors" true: each key column is read through the normal path, so it arrives
	// already filtered, and the permutation it produces is over the surviving rows only.
	//
	// A key column is necessarily read WHOLE -- there is no row-group-scoped equivalent, because a
	// global order needs every row. That is inherent to sorting, and is the one place F3 costs
	// memory the filter path does not.
	// Gives `handle` the row transform `source` already worked out -- its filter/sample mask and its
	// sort permutation, together with the row-group bookkeeping derived from them -- instead of
	// making it derive the same thing from the same file a second time.
	//
	// **This is a POINTER copy of the two expensive objects, not a data copy.** live_mask is an
	// arrow::BooleanArray and sort_perm an arrow::Array; both are immutable once built, so sharing
	// them costs one atomic refcount increment each however large the file. Everything else copied
	// below is O(row groups) or O(1). That is the whole point: a reader that adopts pays nothing,
	// where one that rebuilds re-decodes the filter's key columns and re-runs the whole sort.
	//
	// **`source` is read WITHOUT a ConcurrencyGuard, deliberately.** The intended caller is several
	// threads each opening their own reader and adopting from one shared, idle source, so guarding
	// it would make the second thread abort on a reader nobody is writing to. What makes that safe
	// is that every field read here is either immutable (the two arrays) or not written after the
	// source's own open (the vectors and strings) -- so this is a concurrent read of settled state,
	// plus two atomic increments. The `guard_owner` check below is a net for the case that
	// assumption is wrong, not the thing that makes it right; it cannot be airtight, because a
	// reader could become busy the instant after it is tested.
	//
	// Refuses rather than silently producing a reader whose mask describes a different file: the
	// two must agree on row-group count and total row count, the destination must be untouched (a
	// column already read on it was read UNMASKED and would not match the adopted mask), and the
	// source's own sample draw must be complete rather than deferred. A source with no transform at
	// all is a legal no-op -- the caller should not have to ask whether there is anything to adopt.
	int64_t parquet_reader_adopt_transform(void *handle, void *source, char *err_out, int64_t err_cap)
	{
		auto reader_handle = as_reader_handle(handle);
		auto *src = static_cast<ParquetReaderHandle *>(source);

		// The BODY below is reachable only from a genuine race: another thread must be INSIDE a
		// call on `source` at this instant. A deterministic test would have to hold that thread
		// mid-call, and a timing-dependent one is worse than none (it passes on a quiet machine and
		// gets disabled on a busy one). Kept because the transform being copied out is exactly the
		// state a concurrent call could be rebuilding.
		//
		// The exclusion starts INSIDE the braces, not above the `if`: the condition is evaluated on
		// every call and is genuinely covered, so excluding its line too would report a live line
		// as a stale-exclusion candidate for good.
		if (src->guard_owner.load(std::memory_order_acquire) != 0)
		{
			// GCOVR_EXCL_START
			std::snprintf(err_out, static_cast<size_t>(err_cap),
				"the source reader is in use by another thread; its transform can only be adopted while it is idle");
			return 1;
			// GCOVR_EXCL_STOP
		}
		if (src->num_row_groups != reader_handle->num_row_groups || src->total_nrows != reader_handle->total_nrows)
		{
			std::snprintf(err_out, static_cast<size_t>(err_cap),
				"the two readers describe different files (%lld vs %lld row groups, %lld vs %lld rows)",
				static_cast<long long>(src->num_row_groups), static_cast<long long>(reader_handle->num_row_groups),
				static_cast<long long>(src->total_nrows), static_cast<long long>(reader_handle->total_nrows));
			return 1;
		}
		// The BODY below is unreachable through the public API, and the reasoning is worth keeping
		// because it is a chain of three facts rather than one. has_pending_sample is set only by
		// parquet_reader_set_sample with filter_will_follow != 0; its one Fortran caller
		// (parquet_apply_sample, parquet_read.f90) passes that flag as `filter%n > 0`; and
		// parquet_open_reader_base then calls parquet_apply_filter unconditionally, which cannot
		// take its own early return for a filter with clauses. So the draw is always installed
		// before the reader is handed back, and no caller can hold one with a pending draw. Kept
		// as a backstop: adopting a transform that is not finished yet would silently copy a mask
		// the source has not drawn. Exclusion starts inside the braces, as above.
		if (src->has_pending_sample)
		{
			// GCOVR_EXCL_START
			std::snprintf(err_out, static_cast<size_t>(err_cap),
				"the source reader's sample draw has not been installed yet");
			return 1;
			// GCOVR_EXCL_STOP
		}
		if (reader_handle->live_mask || reader_handle->sort_perm)
		{
			std::snprintf(err_out, static_cast<size_t>(err_cap),
				"this reader already has a filter, a sample or a sort of its own");
			return 1;
		}
		if (!reader_handle->column_cache.empty())
		{
			std::snprintf(err_out, static_cast<size_t>(err_cap),
				"a column has already been read on this reader, and would have been read unmasked");
			return 1;
		}

		// The mask and everything derived from it. Order does not matter here -- unlike
		// set_filter/set_sort, nothing below is computed from anything else below.
		reader_handle->nrows = src->nrows;
		reader_handle->live_mask = src->live_mask;                          // refcount increment
		reader_handle->row_group_live = src->row_group_live;
		reader_handle->row_group_live_offsets = src->row_group_live_offsets;
		reader_handle->row_group_surviving = src->row_group_surviving;
		reader_handle->row_groups_pruned = src->row_groups_pruned;
		reader_handle->sort_perm = src->sort_perm;                          // refcount increment
		// Cosmetic, but they belong to the same act: parquet_reader_print_stat on an adopting
		// reader should describe the transform it is actually applying, not report none.
		reader_handle->filter_clauses = src->filter_clauses;
		reader_handle->filter_expr_text = src->filter_expr_text;
		reader_handle->sort_key_text = src->sort_key_text;
		reader_handle->has_sample = src->has_sample;
		reader_handle->sample_fraction = src->sample_fraction;
		reader_handle->sample_seed_used = src->sample_seed_used;
		return 0;
	}

	// Validates one sort key's name, decodes its column and binds it. Returns false with err_out
	// filled on any refusal. **Shared by parquet_reader_set_sort and by the Fortran-engine export
	// below**, so the two cannot disagree about which columns are sortable, about struct-path
	// resolution, or about the wording of a refusal -- three rules that would otherwise be stated
	// twice and drift apart silently.
	static bool bind_one_sort_key(ParquetReaderHandle *reader_handle, const char *name_in,
		bool descending, bool nulls_first, SortKeyData &out, char *err_out, int64_t err_cap)
	{
		std::string name = trim_right_spaces_and_nuls(std::string(name_in));
		if (!struct_path_exists(reader_handle->schema, name.c_str()))
		{
			std::snprintf(err_out, static_cast<size_t>(err_cap), "unknown column in sort key: %s", name.c_str());
			return false;
		}
		// A vector column has no single value per row to order by. Answered from the schema so
		// the rejection costs no read at all.
		auto leaf_type = resolve_struct_path(reader_handle->schema, name.c_str()).leaf_field->type()->id();
		if (leaf_type == arrow::Type::FIXED_SIZE_LIST || leaf_type == arrow::Type::LIST ||
			leaf_type == arrow::Type::LARGE_LIST)
		{
			std::snprintf(err_out, static_cast<size_t>(err_cap),
				"sort key '%s' is a vector column; sorting only supports scalar columns", name.c_str());
			return false;
		}

		std::shared_ptr<arrow::Array> array;
		try
		{
			array = get_single_chunk_array(reader_handle, name.c_str());
		}
		// GCOVR_EXCL_START -- file-I/O backstop on an already-validated column name; the same
		// class of unreachable catch documented on parquet_reader_set_filter's own.
		catch (const std::exception &e)
		{
			std::snprintf(err_out, static_cast<size_t>(err_cap), "failed to read sort key column '%s': %s", name.c_str(), e.what());
			return false;
		}
		// GCOVR_EXCL_STOP

		if (!sort_bind_arrow_key(array, descending, nulls_first, out))
		{
			std::snprintf(err_out, static_cast<size_t>(err_cap),
				"sort key '%s' has an unsupported column type: %s", name.c_str(), array->type()->ToString().c_str());
			return false;
		}
		return true;
	}

	// ---- Exporting a sort key to the Fortran engine ----
	//
	// The read-time sort builds its permutation with parquet_sorting's radix engine rather than
	// with the comparator engine above; these three entry points are the whole bridge. Fortran
	// asks what family and how large a key is, copies the reduced values out, sorts, and hands the
	// permutation back.
	//
	// **_info's reduction is handed to _fetch through a one-slot cache on the handle**
	// (`sort_key_cache`; its declaration carries the reasoning). Each entry point used to bind the
	// key it was asked about and discard it, which meant the O(rows) value copy happened TWICE per
	// key -- Arrow's decode did not repeat, because get_single_chunk_array serves the second call
	// from column_cache, but the reduction did. That was measured at 5-9% of a whole read-time open
	// on both compilers, which is why it is no longer done.
	//
	// An earlier version of this banner said "nothing is staged on the reader handle between calls"
	// and cited a 1.4-3% cost for the repeat. The figure was an estimate and was low by roughly
	// three times; the phase counters below are what replaced it with a measurement.
	//
	// **The guarantee the old shape bought is not weakened -- it is strengthened.** The size _info
	// promises and the size _fetch writes now come from ONE object rather than from two code paths
	// agreeing. And the lifetime worry the old note raised does not arise: a finalized key may be
	// moved without invalidating its read pointers (see sort_key_finalize), the slot is cleared the
	// moment it is consumed, and _fetch keeps its from-scratch path for every case where the slot
	// does not match.

	// Drops whatever _info staged. Called on every error return from either entry point and once
	// more when the sort is installed: a slot left full holds one key's reduction alive until the
	// reader closes, which is bounded but free to avoid -- and, more importantly, a slot left full
	// after an ABORTED sort is the one that a later, unrelated _fetch could find.
	static inline void sort_key_cache_clear(ParquetReaderHandle *reader_handle)
	{
		reader_handle->sort_key_cached = false;
		reader_handle->sort_key_cache = SortKeyData();
		reader_handle->sort_key_cache_name.clear();
		reader_handle->sort_key_cache_descending = false;
		reader_handle->sort_key_cache_nulls_first = false;
	}

	// Reports a sort key's reduced family and dimensions.
	//   family: 0 = integer (boolean and every temporal type reduce to this), 1 = real, 2 = string
	//   nbytes: total payload bytes, family 2 only; 0 otherwise
	//   has_nulls: 1 when the key carries a validity vector at all. Fortran uses this to skip
	//              allocating a mask entirely, which lets it pass an unallocated optional and so
	//              reach pf_argsort's no-mask fast path (F2018 15.5.2.12).
	int64_t parquet_reader_sort_key_info(void *handle, const char *name, int8_t descending,
		int8_t nulls_first, int32_t *family, int64_t *nrows, int64_t *nbytes, int8_t *has_nulls,
		char *err_out, int64_t err_cap)
	{
		auto reader_handle = as_reader_handle(handle);
		SortKeyData key;
		auto t_info = std::chrono::steady_clock::now();
		bool bound = bind_one_sort_key(reader_handle, name, descending != 0, nulls_first != 0, key, err_out, err_cap);
		charge_phase(t_info, g_debug_sort_info_nanos);
		if (!bound) { sort_key_cache_clear(reader_handle); return 1; }

		*nbytes = 0;
		switch (key.kind)
		{
		case SortValueKind::Integer:
			*family = 0;
			*nrows = static_cast<int64_t>(key.ints.size());
			break;
		case SortValueKind::Real:
			*family = 1;
			*nrows = static_cast<int64_t>(key.reals.size());
			break;
		default:
			*family = 2;
			*nrows = static_cast<int64_t>(key.strs.size());
			for (const auto &s : key.strs) *nbytes += static_cast<int64_t>(s.size());
			break;
		}
		*has_nulls = key.valid.empty() ? 0 : 1;

		// Hand the reduction to the _fetch that is about to follow. Every size above is read from
		// `key` BEFORE this move -- moving a finalized key is safe (a vector's heap buffer survives
		// it, so ints_ptr/reals_ptr stay valid) but it does leave `key` empty.
		reader_handle->sort_key_cache = std::move(key);
		reader_handle->sort_key_cache_name = name ? name : "";
		reader_handle->sort_key_cache_descending = (descending != 0);
		reader_handle->sort_key_cache_nulls_first = (nulls_first != 0);
		reader_handle->sort_key_cached = true;
		return 0;
	}

	// Copies one bound sort key's reduced values into Fortran-owned buffers. Exactly one of
	// ints/reals/(offsets,data) is written, per the family _info reported; the unused pointers may
	// be null. `valid` is written only when _info reported has_nulls, and `offsets` is written
	// 0-based with offsets[0] = 0, which is the form parquet_string_column%append_buffers takes.
	int64_t parquet_reader_sort_key_fetch(void *handle, const char *name, int8_t descending,
		int8_t nulls_first, int64_t *ints, double *reals, int64_t *offsets, char *data,
		int8_t *valid, char *err_out, int64_t err_cap)
	{
		auto reader_handle = as_reader_handle(handle);
		SortKeyData key;
		auto t_bind = std::chrono::steady_clock::now();
		// The slot _info filled, if it holds THIS key. All three identity fields must match: a name
		// match alone would hand back the wrong reduction for the same column sorted twice under
		// different directions, and that is a wrong answer rather than a slowdown.
		bool bound = false;
		if (reader_handle->sort_key_cached
			&& reader_handle->sort_key_cache_name == (name ? name : "")
			&& reader_handle->sort_key_cache_descending == (descending != 0)
			&& reader_handle->sort_key_cache_nulls_first == (nulls_first != 0))
		{
			key = std::move(reader_handle->sort_key_cache);
			reader_handle->sort_key_cached = false;
			reader_handle->sort_key_cache = SortKeyData();
			reader_handle->sort_key_cache_name.clear();
			bound = true;
		}
		else
		{ // GCOVR_EXCL_START -- unreachable today; see the note below, kept deliberately
			// Not a match, or nothing staged. Bind exactly as this entry point always did.
			//
			// **NO FIXTURE IN THIS REPOSITORY CAN REACH THIS ARM, and it stays anyway.** The two
			// entry points have exactly one caller -- add_read_sort_key (src/parquet_read.f90) --
			// which calls _info and then _fetch with identical arguments, per key, with nothing in
			// between. So the slot always holds the key being fetched and the identity match above
			// always succeeds. Confirmed by mutation: replacing the four-way match with a bare
			// `if (reader_handle->sort_key_cached)` leaves all 29 tests in the `sort` suite
			// passing, exit 0. (The mutation that IS caught is moving the `std::move` in _key_info
			// above the size reads, which aborts the suite outright -- exit 134, zero tests run.)
			//
			// Two reasons not to replace it with an assertion, which is what a coverage report will
			// suggest. The pairing it relies on is a property of ONE Fortran caller and nothing
			// enforces it; and an assertion converts a recoverable mismatch into an abort inside a
			// library whose C++ half cannot throw across the extern "C" boundary. One comparison is
			// a cheap price for not making the two entry points secretly order-dependent.
			bound = bind_one_sort_key(reader_handle, name, descending != 0, nulls_first != 0, key, err_out, err_cap);
		}
		// GCOVR_EXCL_STOP
		// The phase counter stays where it was and still wraps WHICHEVER path was taken, so the
		// benchmark shows `bind` collapsing towards zero rather than the counter vanishing. A
		// counter that disappears is indistinguishable from one that was never reached.
		auto t_copy = charge_phase(t_bind, g_debug_sort_bind_nanos);
		if (!bound) { sort_key_cache_clear(reader_handle); return 1; }

		switch (key.kind)
		{
		case SortValueKind::Integer:
			std::memcpy(ints, key.ints.data(), key.ints.size() * sizeof(int64_t));
			break;
		case SortValueKind::Real:
			std::memcpy(reals, key.reals.data(), key.reals.size() * sizeof(double));
			break;
		default:
		{
			int64_t at = 0;
			offsets[0] = 0;
			for (size_t i = 0; i < key.strs.size(); ++i)
			{
				if (!key.strs[i].empty()) std::memcpy(data + at, key.strs[i].data(), key.strs[i].size());
				at += static_cast<int64_t>(key.strs[i].size());
				offsets[i + 1] = at;
			}
			break;
		}
		}
		if (!key.valid.empty())
		{
			for (size_t i = 0; i < key.valid.size(); ++i) valid[i] = static_cast<int8_t>(key.valid[i]);
		}
		charge_phase(t_copy, g_debug_sort_copy_nanos);
		return 0;
	}

	// Installs a permutation built by the Fortran engine, and releases everything already decoded
	// so that it is re-read through the permutation instead. Everything after the engine call in
	// parquet_reader_set_sort below, and it must stay that way: the permutation has to reach
	// sort_perm as an arrow::Int64Array because apply_row_transform consumes it as one.
	//
	// `perm` arrives 0-based, which is what Arrow's Take wants and what the Fortran side converts
	// to on its way out -- pf_argsort produces 1-based indices.
	int64_t parquet_reader_sort_install(void *handle, const int64_t *perm, int64_t n,
		const char *key_text, int64_t keep_cache, char *err_out, int64_t err_cap)
	{
		auto reader_handle = as_reader_handle(handle);
		// Every key has been fetched by the time a permutation comes back, so the slot is spent;
		// clearing it here is what stops one surviving an aborted or partial install.
		sort_key_cache_clear(reader_handle);
		auto t_perm = std::chrono::steady_clock::now();
		arrow::Int64Builder perm_builder;
		auto append_status = perm_builder.AppendValues(perm, n);
		if (!append_status.ok())
		{ // GCOVR_EXCL_START -- Int64Builder allocation backstop, not fixture-triggerable
			std::snprintf(err_out, static_cast<size_t>(err_cap), "failed to build sort permutation: %s", append_status.ToString().c_str());
			return 1;
		}
		// GCOVR_EXCL_STOP
		std::shared_ptr<arrow::Array> perm_array;
		auto finish_status = perm_builder.Finish(&perm_array);
		if (!finish_status.ok())
		{ // GCOVR_EXCL_START -- Int64Builder allocation backstop, not fixture-triggerable
			std::snprintf(err_out, static_cast<size_t>(err_cap), "failed to build sort permutation: %s", finish_status.ToString().c_str());
			return 1;
		}
		// GCOVR_EXCL_STOP
		reader_handle->sort_perm = perm_array;
		if (key_text != nullptr) reader_handle->sort_key_text = key_text;
		auto t_take = charge_phase(t_perm, g_debug_sort_perm_nanos);

		// **What happens to everything already decoded, and why there are two answers.**
		// column_cache at this moment holds exactly the columns this sort decoded to bind its own
		// keys: the guards on parquet_reader_set_sort (parquet_read.f90) refuse a reader that has
		// read anything, and parquet_open_reader(sort_by=) installs before its own prefetch. Those
		// entries predate sort_perm, so they are in physical order and cannot simply be left.
		//
		// **The ordering invariant is enforced by the READ path, which is what makes RELEASING an
		// option at all.** A column decoded after sort_perm exists is permuted by
		// apply_row_transform before it is cached, and a cache hit is returned untransformed
		// precisely because the transform already happened. Dropping an entry therefore means the
		// next read decodes it again and takes that ordinary path -- no second cache state, and no
		// per-entry flag to consult. The eager Take below exists only to fix up entries that
		// predate the permutation; it is not what upholds the invariant.
		//
		// **Default: RELEASE, because the key columns are ones the caller never asked for.**
		// Sorting by a column does not mean reading it, and reordering data nobody looks at is the
		// whole of feature_sort.md's P13. Measured on machine C at n = 2e7 with a string key: the
		// Take is 3115 ms, 49% of a sort-then-read workflow that never touches the key. A caller
		// that DOES read the key pays one extra decode instead -- under 564 ms, and only that,
		// since the Take has to happen either way once the column is read.
		//
		// **keep_cache != 0: re-Take instead, because the caller has said it wants everything
		// resident.** The one caller that sets it is parquet_open_reader(..., sort_by=,
		// prefetch=.true.), where releasing would be a pure loss: the prefetch that follows would
		// decode the key columns a second time to put them straight back. Nothing else may set it,
		// and in particular it must never be set on a path that does not immediately re-read what
		// it kept -- keeping an entry is only safe because the Take below reorders it here.
		//
		// Releasing is correct for any entry, key or not, which is why it is a clear() rather than
		// a selective erase: the array is rebuilt from the file on demand, so the worst case is a
		// re-decode and never a wrong answer. was_released is marked for the same reason
		// parquet_reader_release_column marks it -- parquet_reader_print_stat must not look a
		// dropped column up in a cache it has left.
		ensure_compute_initialized();
		if (keep_cache != 0)
		{
			for (auto &entry : reader_handle->column_cache)
			{
				auto taken = arrow::compute::Take(arrow::Datum(entry.second), arrow::Datum(perm_array));
				if (!taken.ok())
				{ // GCOVR_EXCL_START -- Take-kernel Status backstop on an already-validated permutation
					std::snprintf(err_out, static_cast<size_t>(err_cap), "failed to apply sort: %s", taken.status().ToString().c_str());
					return 1;
				}
				// GCOVR_EXCL_STOP
				entry.second = taken.ValueOrDie().make_array();
				++g_debug_sort_take_columns;
			}
		}
		else
		{
			for (const auto &entry : reader_handle->column_cache)
			{
				reader_handle->was_released.insert(entry.first);
				++g_debug_sort_released_columns;
			}
			reader_handle->column_cache.clear();
		}
		charge_phase(t_take, g_debug_sort_take_nanos);
		return 0;
	}

	// ---- The raw-array sort builder (parquet_table's in-memory %sort_by) ----
	//
	// Same engine, same comparator, same null/NaN tiers as parquet_reader_set_sort above -- only
	// the source of the key values differs. Usage: new -> add_key_* per key, in order of
	// precedence -> build -> free.

	// Starts a builder for an `nrows`-row sort. Returns an opaque handle; the caller must free it.
	void *parquet_sort_builder_new(int64_t nrows)
	{
		auto *h = new SortBuilderHandle{};
		h->nrows = nrows;
		return h;
	}

	// Adds an integer key. Boolean and every temporal kind arrive here too: their stored values
	// order exactly as the values they represent, which is the same reduction sort_bind_arrow_key
	// makes on the Arrow side.
	void parquet_sort_builder_add_key_int64(void *handle, const int64_t *values, const int8_t *valid,
		int8_t descending, int8_t nulls_first)
	{
		auto *h = static_cast<SortBuilderHandle *>(handle);
		SortKeyData key;
		key.kind = SortValueKind::Integer;
		key.descending = descending != 0;
		key.nulls_first = nulls_first != 0;
		key.ints.assign(values, values + h->nrows);
		sort_builder_set_valid(key, valid, h->nrows);
		sort_key_finalize(key);
		h->keys.push_back(std::move(key));
	}

	// Adds a floating-point key. NaNs are ordinary values here and are tiered by sort_tier_of,
	// never by the caller.
	void parquet_sort_builder_add_key_double(void *handle, const double *values, const int8_t *valid,
		int8_t descending, int8_t nulls_first)
	{
		auto *h = static_cast<SortBuilderHandle *>(handle);
		SortKeyData key;
		key.kind = SortValueKind::Real;
		key.descending = descending != 0;
		key.nulls_first = nulls_first != 0;
		key.reals.assign(values, values + h->nrows);
		sort_builder_set_valid(key, valid, h->nrows);
		sort_key_finalize(key);
		h->keys.push_back(std::move(key));
	}

	// Adds a string key from a packed (offsets, data) pair: row i is data[offsets[i]
	// .. offsets[i+1]), so `offsets` has nrows+1 entries. That is the layout parquet_string_column
	// already stores, so the Fortran side hands over what it has rather than reformatting it.
	//
	// The bytes are COPIED into the handle, because the caller's buffers belong to a column the
	// sort is about to permute -- borrowing views into storage that reindex() is going to
	// reallocate would dangle exactly when the permutation is applied.
	void parquet_sort_builder_add_key_string(void *handle, const int64_t *offsets, const char *data,
		const int8_t *valid, int8_t descending, int8_t nulls_first)
	{
		auto *h = static_cast<SortBuilderHandle *>(handle);
		SortKeyData key;
		key.kind = SortValueKind::Str;
		key.descending = descending != 0;
		key.nulls_first = nulls_first != 0;
		h->string_stores.emplace_back();
		auto &store = h->string_stores.back();
		store.reserve(static_cast<size_t>(h->nrows));
		key.strs.resize(static_cast<size_t>(h->nrows));
		for (int64_t i = 0; i < h->nrows; ++i)
		{
			int64_t lo = offsets[i], hi = offsets[i + 1];
			store.emplace_back(data + lo, static_cast<size_t>(hi - lo));
		}
		for (int64_t i = 0; i < h->nrows; ++i) key.strs[static_cast<size_t>(i)] = store[static_cast<size_t>(i)];
		sort_builder_set_valid(key, valid, h->nrows);
		sort_key_finalize(key);
		h->keys.push_back(std::move(key));
	}

	// Writes the 1-BASED permutation into `perm_out` (which the caller sized to nrows), ready to
	// feed parquet_column%reindex. The engine works 0-based, so the +1 happens here rather than
	// being repeated at every Fortran call site.
	int64_t parquet_sort_builder_build(void *handle, int64_t threads, int64_t *perm_out)
	{
		auto *h = static_cast<SortBuilderHandle *>(handle);
		if (h->keys.empty()) return 1;
		auto perm = sort_build_permutation_threaded(h->keys, h->nrows, threads);
		for (int64_t i = 0; i < h->nrows; ++i) perm_out[i] = perm[static_cast<size_t>(i)] + 1;
		return 0;
	}

	// 1 when every row is already in the stated order under the FULL key list, 0 when it is not,
	// and -1 when no key was added.
	//
	// The multi-key counterpart of parquet_sort_is_sorted_* below, and the reason it has to exist:
	// a parquet_timestamp binds as TWO integer keys (seconds, then nanoseconds), so no single-key
	// entry point can answer the question for one. Answering it by argsorting and testing the
	// permutation for identity would be correct but O(n log n), turning a documented O(n) query
	// into a sort.
	//
	// Same rule as sort_is_sorted_key: adjacent rows only, no index tiebreaker, so a run of equal
	// rows is sorted -- applied across the keys in precedence order, which is exactly what
	// sort_build_permutation's own comparator does minus that tiebreaker.
	int64_t parquet_sort_builder_is_sorted(void *handle)
	{
		auto *h = static_cast<SortBuilderHandle *>(handle);
		if (h->keys.empty()) return -1;
		for (int64_t i = 1; i < h->nrows; ++i)
		{
			if (sort_keys_compare(h->keys, i - 1, i, h->keys.size()) > 0) return 0;
		}
		return 1;
	}

	// ---- Conformance hooks for the Fortran comparator core (TEST-ONLY) -------------------------
	//
	// feature_sort.md Stage 1 replaces sort_tier_of/sort_compare_key/SortRowLess/sort_keys_compare
	// with Fortran equivalents, and requires them proved equal to THESE, comparator answer for
	// comparator answer, before any sorting code is written. That cannot be done through any of the
	// 24 entry points above: every one of them answers at whole-permutation level, and at Stage 1
	// the Fortran side does not sort yet, so there is no permutation to compare. The four
	// comparators are `static`, i.e. unreachable from outside this translation unit. Hence these.
	//
	// Rows are 0-BASED, matching every other internal index in this file -- the entry points add
	// the +1 on the way out. The Fortran test declares its own local bind(C) interfaces (the
	// convention every parquet_debug_* hook follows) and passes i-1. Deliberately NOT declared in
	// src/parquet_bindings.f90, so none of this reaches the library's own interface.
	//
	// Being C++-side is the point: the six hooks of feature_sort.md section 7.4 have to become
	// PUBLIC Fortran procedures because their state is Fortran-side, and these do not, because the
	// state they read is here.
	//
	// NOTE for anyone writing a test that also counts comparisons: parquet_debug_sort_row_less
	// invokes SortRowLess, so it increments g_debug_sort_comparison_count when counting is armed.
	//
	// Lifetime: under Endpoint B they are deleted with the rest of the engine at Stage 9; under
	// Endpoint A they stop being scaffolding and become the permanent conformance harness of
	// feature_sort.md section 8.1.

	// 1 when row `a` sorts before row `b` under the full sort comparator, 0 when it does not,
	// -1 when no key has been added (which no correct caller does).
	int64_t parquet_debug_sort_row_less(void *handle, int64_t a, int64_t b)
	{
		auto *h = static_cast<SortBuilderHandle *>(handle);
		if (h->keys.empty()) return -1;
		SortRowLess less{&h->keys};
		return less(a, b) ? 1 : 0;
	}

	// The three-way, tiebreaker-free answer over the leading `nkeys` keys: -1, 0 or +1.
	//
	// Returns -2 when no key has been added. NOT -1: that is a legitimate answer here, so the
	// error sentinel has to sit outside the value range -- unlike parquet_debug_sort_row_less
	// above, whose answers are only 0 and 1.
	int64_t parquet_debug_sort_keys_compare(void *handle, int64_t a, int64_t b, int64_t nkeys)
	{
		auto *h = static_cast<SortBuilderHandle *>(handle);
		if (h->keys.empty()) return -2;
		if (nkeys < 0) nkeys = 0;
		return sort_keys_compare(h->keys, a, b, static_cast<size_t>(nkeys));
	}

	// ---- Batched sweeps, for the Stage 1e comparator benchmark (TEST-ONLY) ----------------------
	//
	// The two hooks above cross bind(C) once per comparison, which is fine for a conformance test
	// and useless for a measurement: the crossing costs more than the comparator does, so a per-call
	// timing would be a timing of the crossing. These do the whole sweep inside one call, so the
	// crossing amortises to nothing and what is left is the comparator.
	//
	// bench/benchmark_sort_comparator.f90 runs a Fortran loop of IDENTICAL shape against
	// parquet_debug_sort_sweep_* in parquet_sorting, so the two arms differ only in whose comparator
	// runs. Keep the two loops identical if either is touched -- their returned checksums must
	// match, which is what proves they did the same work.
	//
	// The stride walk is deliberate: `j = i + stride` with one conditional subtraction, never
	// `mod(i + stride, nrows)`. A runtime-divisor `mod` compiles to an integer division (~6 ns on
	// x86-64), which in a loop measuring a ~5 ns comparator would be most of the measurement --
	// CLAUDE.md records a benchmark where exactly that was the entire reported floor.
	int64_t parquet_debug_sort_sweep_less_cpp(void *handle, int64_t nrows, int64_t nreps)
	{
		auto *h = static_cast<SortBuilderHandle *>(handle);
		if (h->keys.empty() || nrows < 2) return -1;
		SortRowLess less{&h->keys};
		int64_t count = 0;
		for (int64_t rep = 0; rep < nreps; ++rep)
		{
			int64_t stride = 1 + (rep % (nrows - 1)); // once per REP, not per comparison
			for (int64_t i = 0; i < nrows; ++i)
			{
				int64_t j = i + stride;
				if (j >= nrows) j -= nrows;
				if (less(i, j)) ++count;
			}
		}
		return count;
	}

	int64_t parquet_debug_sort_sweep_compare_cpp(void *handle, int64_t nrows, int64_t nreps,
		int64_t nkeys)
	{
		auto *h = static_cast<SortBuilderHandle *>(handle);
		if (h->keys.empty() || nrows < 2) return -1;
		if (nkeys < 0) nkeys = 0;
		int64_t sum = 0;
		for (int64_t rep = 0; rep < nreps; ++rep)
		{
			int64_t stride = 1 + (rep % (nrows - 1));
			for (int64_t i = 0; i < nrows; ++i)
			{
				int64_t j = i + stride;
				if (j >= nrows) j -= nrows;
				sum += sort_keys_compare(h->keys, i, j, static_cast<size_t>(nkeys));
			}
		}
		return sum;
	}

	// Writes the first `count` entries of the 1-BASED permutation into `perm_out` (which the caller
	// sized to `count`, not to nrows). `count` is clamped to nrows, so asking for more elements than
	// exist returns all of them rather than failing -- pf_partial_sort's documented behaviour.
	// Returns 0 on success, 1 when no key was added.
	int64_t parquet_sort_builder_build_partial(void *handle, int64_t count, int64_t *perm_out)
	{
		auto *h = static_cast<SortBuilderHandle *>(handle);
		if (h->keys.empty()) return 1;
		if (count > h->nrows) count = h->nrows;
		auto perm = sort_build_partial_permutation(h->keys, h->nrows, count);
		for (int64_t i = 0; i < count; ++i) perm_out[i] = perm[static_cast<size_t>(i)] + 1;
		return 0;
	}

	// The 1-BASED row index a full stable sort would place at 1-based rank `nth`, or 0 when no key
	// was added or `nth` is outside 1..nrows. The caller reads its own value out with this index,
	// which is why nothing here knows anything about value types.
	int64_t parquet_sort_builder_nth_element(void *handle, int64_t nth)
	{
		auto *h = static_cast<SortBuilderHandle *>(handle);
		if (h->keys.empty()) return 0;
		if (nth < 1 || nth > h->nrows) return 0;
		return sort_nth_index(h->keys, h->nrows, nth - 1) + 1;
	}

	// ---- The M3 "extras": run detection, binary search and merge ----
	//
	// All three answer questions plain ordering cannot, and all three route through
	// sort_keys_compare rather than SortRowLess, because each turns on rows comparing EQUAL -- the
	// one relation the sort comparator's index tiebreaker deliberately destroys.
	//
	// Each is builder-only, with no one-shot single-key twin like the argsorts above. That is a
	// considered trade: the one-shot forms exist to skip a copy of the key on the hottest path in
	// the library, and these are not it. Sharing one entry point per operation across every element
	// type (a timestamp's two keys included) is worth one extra copy of an already-extracted buffer.

	// Sorts, then reports where the runs of EQUAL rows are: perm_out receives the 1-based
	// permutation and tie_out[k] is 1 when the row at output position k compares equal to the row
	// before it. Returns 0, or 1 when no key was added.
	//
	// One call rather than a sort followed by a separate comparison pass, because the caller
	// (pf_unique, pf_rank) needs both and building the permutation twice would double the cost of
	// the operation. tie_out[0] is always 0 -- the first row starts a run by definition.
	//
	// `group_keys` is how many LEADING keys decide a tie, and it is the ONLY thing here that does
	// not use every key: the sort below always orders by all of them. That asymmetry is the whole
	// point -- it produces "grouped by field, ordered by magnitude within each group" from one pass.
	// Fortran always sends a real count (never 0 meaning "all"), already translated from the
	// caller's key count, so this side neither interprets nor defaults it.
	int64_t parquet_sort_builder_build_runs(void *handle, int64_t threads, int64_t group_keys,
		int64_t *perm_out, int8_t *tie_out)
	{
		auto *h = static_cast<SortBuilderHandle *>(handle);
		if (h->keys.empty()) return 1;
		if (group_keys < 1) group_keys = static_cast<int64_t>(h->keys.size());
		auto perm = sort_build_permutation_threaded(h->keys, h->nrows, threads);
		for (int64_t i = 0; i < h->nrows; ++i) perm_out[i] = perm[static_cast<size_t>(i)] + 1;
		if (h->nrows > 0) tie_out[0] = 0;
		for (int64_t i = 1; i < h->nrows; ++i)
		{
			tie_out[i] = sort_keys_compare(h->keys, perm[static_cast<size_t>(i - 1)],
				perm[static_cast<size_t>(i)], static_cast<size_t>(group_keys)) == 0 ? 1 : 0;
		}
		return 0;
	}

	// Binary search. The builder holds n_search + 1 rows: [0, n_search) is the array being searched
	// and the LAST row is the target value, appended by the caller. That is what makes drift from
	// the sort comparator structurally impossible -- the target is compared by the very same
	// sort_compare_key over the very same key layout, with no compare-a-row-against-a-value arm to
	// keep in step (feature_risks.md Risk-34).
	//
	// `which` is 0 for lower_bound (first position not ordered before the target) and 1 for
	// upper_bound (first position the target is ordered before). Returns a 1-BASED insertion point
	// in 1 .. n_search+1, or -1 when no key was added.
	//
	// Written as an explicit loop rather than std::lower_bound: that would need an iterator over a
	// materialized [0, n) index vector, which is O(n) time and memory to set up for an O(log n)
	// search -- the whole point of the operation.
	int64_t parquet_sort_builder_search(void *handle, int64_t n_search, int8_t which)
	{
		auto *h = static_cast<SortBuilderHandle *>(handle);
		if (h->keys.empty()) return -1;
		int64_t target = h->nrows - 1;
		if (n_search < 0) n_search = 0;
		if (n_search > target) n_search = target;
		int64_t lo = 0, hi = n_search;
		while (lo < hi)
		{
			int64_t mid = lo + (hi - lo) / 2;
			int c = sort_keys_compare(h->keys, mid, target, h->keys.size());
			bool before = (which == 0) ? (c < 0) : (c <= 0);
			if (before) lo = mid + 1;
			else hi = mid;
		}
		return lo + 1;
	}

	// Merges two already-ordered ranges of one builder into a 1-based permutation of all its rows:
	// [0, na) is the first input and [na, nrows) the second, concatenated by the caller. Returns 0,
	// or 1 when no key was added.
	//
	// Hand-rolled rather than std::merge for the same reason parquet_sort_builder_search is: merging
	// INDICES with std::merge needs two materialized index vectors, and the loop that avoids them is
	// six lines. `<= 0` takes from the first input on a tie, which is std::merge's own stability
	// guarantee and what makes pf_merge agree with pf_sort of the concatenation element for element.
	int64_t parquet_sort_builder_merge(void *handle, int64_t na, int64_t *perm_out)
	{
		auto *h = static_cast<SortBuilderHandle *>(handle);
		if (h->keys.empty()) return 1;
		if (na < 0) na = 0;
		if (na > h->nrows) na = h->nrows;
		int64_t i = 0, j = na, k = 0;
		while (i < na && j < h->nrows)
		{
			if (sort_keys_compare(h->keys, i, j, h->keys.size()) <= 0) perm_out[k++] = (i++) + 1;
			else perm_out[k++] = (j++) + 1;
		}
		while (i < na) perm_out[k++] = (i++) + 1;
		while (j < h->nrows) perm_out[k++] = (j++) + 1;
		return 0;
	}

	void parquet_sort_builder_free(void *handle)
	{
		delete static_cast<SortBuilderHandle *>(handle);
	}

	// ---- One-shot single-key entry points (parquet_sorting's direct forms) ----
	//
	// Same engine, same comparator, same tiers as everything above -- the only difference is that
	// nothing outlives the call, which is what makes it safe for these to BORROW the caller's array
	// instead of copying it. That matters at scale: an argsort over a billion-element array copied
	// 8 GB of key for values the caller already held.
	//
	// The lifetime argument has to be exact, because Fortran can defeat it. A non-contiguous actual
	// argument (`pf_argsort(a(1:n:2), perm)`) makes the compiler pass a contiguous TEMPORARY, which
	// it is free to discard the moment the call returns. Borrowing is therefore safe here, where
	// the borrowed array is used and finished with before returning, and would NOT be safe on the
	// builder adders above, whose keys outlive the call that added them -- see SortKeyData's own
	// comment. Do not "unify" these with the builder by having them create one.

	// Fills a borrowed key. `valid` may be null, which the engine reads as "no nulls at all".
	static SortKeyData sort_borrowed_key(SortValueKind kind, const int64_t *ints, const double *reals,
		const int8_t *valid, int64_t n, int8_t descending, int8_t nulls_first)
	{
		SortKeyData key;
		key.kind = kind;
		key.descending = descending != 0;
		key.nulls_first = nulls_first != 0;
		key.ints_ptr = ints;
		key.reals_ptr = reals;
		sort_builder_set_valid(key, valid, n);
		sort_key_finalize(key);
		return key;
	}

	// Writes the 1-BASED permutation of a single integer key into `perm_out` (sized n by the
	// caller). Boolean and every temporal kind arrive here too, reduced to their stored integers.
	void parquet_sort_argsort_int64(int64_t n, const int64_t *values, const int8_t *valid,
		int8_t descending, int8_t nulls_first, int64_t threads, int64_t *perm_out)
	{
		if (n <= 0) return;
		std::vector<SortKeyData> keys;
		keys.push_back(sort_borrowed_key(SortValueKind::Integer, values, nullptr, valid, n, descending, nulls_first));
		auto perm = sort_build_permutation_threaded(keys, n, threads);
		for (int64_t i = 0; i < n; ++i) perm_out[i] = perm[static_cast<size_t>(i)] + 1;
	}

	// The floating-point counterpart. NaNs are ordinary values and are tiered by sort_tier_of.
	void parquet_sort_argsort_double(int64_t n, const double *values, const int8_t *valid,
		int8_t descending, int8_t nulls_first, int64_t threads, int64_t *perm_out)
	{
		if (n <= 0) return;
		std::vector<SortKeyData> keys;
		keys.push_back(sort_borrowed_key(SortValueKind::Real, nullptr, values, valid, n, descending, nulls_first));
		auto perm = sort_build_permutation_threaded(keys, n, threads);
		for (int64_t i = 0; i < n; ++i) perm_out[i] = perm[static_cast<size_t>(i)] + 1;
	}

	// The string counterpart, over a packed (offsets, data) pair: row i is data[offsets[i] ..
	// offsets[i+1]), so `offsets` has n+1 entries. The string_views point straight into the
	// caller's `data` -- the bytes are never copied, unlike the builder's own string adder, which
	// has to copy because its key outlives the call.
	void parquet_sort_argsort_string(int64_t n, const int64_t *offsets, const char *data,
		const int8_t *valid, int8_t descending, int8_t nulls_first, int64_t threads, int64_t *perm_out)
	{
		if (n <= 0) return;
		SortKeyData key;
		key.kind = SortValueKind::Str;
		key.descending = descending != 0;
		key.nulls_first = nulls_first != 0;
		key.strs.resize(static_cast<size_t>(n));
		for (int64_t i = 0; i < n; ++i)
		{
			key.strs[static_cast<size_t>(i)] =
				std::string_view(data + offsets[i], static_cast<size_t>(offsets[i + 1] - offsets[i]));
		}
		sort_builder_set_valid(key, valid, n);
		sort_key_finalize(key);
		std::vector<SortKeyData> keys;
		keys.push_back(std::move(key));
		auto perm = sort_build_permutation_threaded(keys, n, threads);
		for (int64_t i = 0; i < n; ++i) perm_out[i] = perm[static_cast<size_t>(i)] + 1;
	}

	// ---- is_sorted ----
	//
	// Deliberately NOT routed through the builder: that would copy every value to answer a question
	// that is O(n) with an early exit. It reuses sort_compare_key WITHOUT the caller's index
	// tiebreaker, comparing adjacent rows only, so a run of equal values is sorted -- using the
	// tiebreaker would make every array trivially "sorted". Sharing the comparator is what stops
	// is_sorted and argsort disagreeing about nulls, NaNs or direction on the same array.
	static int64_t sort_is_sorted_key(const SortKeyData &key, int64_t n)
	{
		for (int64_t i = 1; i < n; ++i)
		{
			if (sort_compare_key(key, i - 1, i) > 0) return 0;
		}
		return 1;
	}

	// 1 when the integer key is in the stated order, 0 otherwise.
	int64_t parquet_sort_is_sorted_int64(int64_t n, const int64_t *values, const int8_t *valid,
		int8_t descending, int8_t nulls_first)
	{
		if (n < 2) return 1;
		return sort_is_sorted_key(
			sort_borrowed_key(SortValueKind::Integer, values, nullptr, valid, n, descending, nulls_first), n);
	}

	// 1 when the floating-point key is in the stated order, 0 otherwise.
	int64_t parquet_sort_is_sorted_double(int64_t n, const double *values, const int8_t *valid,
		int8_t descending, int8_t nulls_first)
	{
		if (n < 2) return 1;
		return sort_is_sorted_key(
			sort_borrowed_key(SortValueKind::Real, nullptr, values, valid, n, descending, nulls_first), n);
	}

	// 1 when the string key is in the stated order, 0 otherwise. Same packed (offsets, data) layout
	// as parquet_sort_argsort_string.
	int64_t parquet_sort_is_sorted_string(int64_t n, const int64_t *offsets, const char *data,
		const int8_t *valid, int8_t descending, int8_t nulls_first)
	{
		if (n < 2) return 1;
		SortKeyData key;
		key.kind = SortValueKind::Str;
		key.descending = descending != 0;
		key.nulls_first = nulls_first != 0;
		key.strs.resize(static_cast<size_t>(n));
		for (int64_t i = 0; i < n; ++i)
		{
			key.strs[static_cast<size_t>(i)] =
				std::string_view(data + offsets[i], static_cast<size_t>(offsets[i + 1] - offsets[i]));
		}
		sort_builder_set_valid(key, valid, n);
		sort_key_finalize(key);
		return sort_is_sorted_key(key, n);
	}

	// ---- One-shot single-key selection (pf_partial_sort / pf_partial_argsort / pf_nth_element) ----
	//
	// Same borrowing rule as the one-shot argsorts above: nothing outlives the call, so the caller's
	// array is read in place. The string forms build string_views into the caller's `data` for the
	// duration of the call and copy nothing.

	// Builds the borrowed string key the two string entry points below share.
	static SortKeyData sort_borrowed_string_key(int64_t n, const int64_t *offsets, const char *data,
		const int8_t *valid, int8_t descending, int8_t nulls_first)
	{
		SortKeyData key;
		key.kind = SortValueKind::Str;
		key.descending = descending != 0;
		key.nulls_first = nulls_first != 0;
		key.strs.resize(static_cast<size_t>(n));
		for (int64_t i = 0; i < n; ++i)
		{
			key.strs[static_cast<size_t>(i)] =
				std::string_view(data + offsets[i], static_cast<size_t>(offsets[i + 1] - offsets[i]));
		}
		sort_builder_set_valid(key, valid, n);
		sort_key_finalize(key);
		return key;
	}

	// Writes the first `count` entries of the 1-based permutation of an integer key into `perm_out`.
	// `count` is clamped to n, so asking for more than exists returns all of it.
	void parquet_sort_partial_argsort_int64(int64_t n, const int64_t *values, const int8_t *valid,
		int8_t descending, int8_t nulls_first, int64_t count, int64_t *perm_out)
	{
		if (n <= 0 || count <= 0) return;
		if (count > n) count = n;
		std::vector<SortKeyData> keys;
		keys.push_back(sort_borrowed_key(SortValueKind::Integer, values, nullptr, valid, n, descending, nulls_first));
		auto perm = sort_build_partial_permutation(keys, n, count);
		for (int64_t i = 0; i < count; ++i) perm_out[i] = perm[static_cast<size_t>(i)] + 1;
	}

	// The floating-point counterpart. NaNs are ordinary values and are tiered by sort_tier_of.
	void parquet_sort_partial_argsort_double(int64_t n, const double *values, const int8_t *valid,
		int8_t descending, int8_t nulls_first, int64_t count, int64_t *perm_out)
	{
		if (n <= 0 || count <= 0) return;
		if (count > n) count = n;
		std::vector<SortKeyData> keys;
		keys.push_back(sort_borrowed_key(SortValueKind::Real, nullptr, values, valid, n, descending, nulls_first));
		auto perm = sort_build_partial_permutation(keys, n, count);
		for (int64_t i = 0; i < count; ++i) perm_out[i] = perm[static_cast<size_t>(i)] + 1;
	}

	// The string counterpart, over the same packed (offsets, data) pair as parquet_sort_argsort_string.
	void parquet_sort_partial_argsort_string(int64_t n, const int64_t *offsets, const char *data,
		const int8_t *valid, int8_t descending, int8_t nulls_first, int64_t count, int64_t *perm_out)
	{
		if (n <= 0 || count <= 0) return;
		if (count > n) count = n;
		std::vector<SortKeyData> keys;
		keys.push_back(sort_borrowed_string_key(n, offsets, data, valid, descending, nulls_first));
		auto perm = sort_build_partial_permutation(keys, n, count);
		for (int64_t i = 0; i < count; ++i) perm_out[i] = perm[static_cast<size_t>(i)] + 1;
	}

	// The 1-based row index a full stable sort would place at 1-based rank `nth`, for an integer
	// key. Returns 0 when `nth` is outside 1..n.
	int64_t parquet_sort_nth_index_int64(int64_t n, const int64_t *values, const int8_t *valid,
		int8_t descending, int8_t nulls_first, int64_t nth)
	{
		if (n <= 0 || nth < 1 || nth > n) return 0;
		std::vector<SortKeyData> keys;
		keys.push_back(sort_borrowed_key(SortValueKind::Integer, values, nullptr, valid, n, descending, nulls_first));
		return sort_nth_index(keys, n, nth - 1) + 1;
	}

	// The floating-point counterpart.
	int64_t parquet_sort_nth_index_double(int64_t n, const double *values, const int8_t *valid,
		int8_t descending, int8_t nulls_first, int64_t nth)
	{
		if (n <= 0 || nth < 1 || nth > n) return 0;
		std::vector<SortKeyData> keys;
		keys.push_back(sort_borrowed_key(SortValueKind::Real, nullptr, values, valid, n, descending, nulls_first));
		return sort_nth_index(keys, n, nth - 1) + 1;
	}

	// The string counterpart.
	int64_t parquet_sort_nth_index_string(int64_t n, const int64_t *offsets, const char *data,
		const int8_t *valid, int8_t descending, int8_t nulls_first, int64_t nth)
	{
		if (n <= 0 || nth < 1 || nth > n) return 0;
		std::vector<SortKeyData> keys;
		keys.push_back(sort_borrowed_string_key(n, offsets, data, valid, descending, nulls_first));
		return sort_nth_index(keys, n, nth - 1) + 1;
	}

	// 1 when a read-time sort is active on this reader, 0 otherwise. This is what the Fortran side's
	// check_reader_no_sort guards key on -- see reader_has_sort_permutation for why the predicate is
	// deliberately about a permutation and not about a filter/sample mask.
	int64_t parquet_reader_has_sort(void *handle)
	{
		auto reader_handle = as_reader_handle(handle);
		return reader_has_sort_permutation(reader_handle) ? 1 : 0;
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

	// ==== Element families ====
	//
	// The one place a physical Arrow leaf type is mapped onto the Fortran kind this library reads
	// it into. Two callers want that mapping and used to be at risk of disagreeing about it: the
	// type-name query below, which answers a caller's "what do I declare?", and the LIST read
	// path (parquet_read_list_column_shape), which must tell Fortran which payload kind to %init
	// a parquet_list_column to before filling it.
	//
	// A family is an integer rather than a token string because it crosses the bind(C) boundary:
	// see the PF_ELEM_* parameters in parquet_bindings.f90, which MUST match these values. The
	// C++ side deliberately knows nothing about Fortran's own PK_* discriminators -- those are a
	// Fortran-side vocabulary and the mapping from a family to one lives there.
	static constexpr int32_t kElemFamilyNone = 0; // not readable by this library at all
	static constexpr int32_t kElemFamilyInt32 = 1;
	static constexpr int32_t kElemFamilyInt64 = 2;
	static constexpr int32_t kElemFamilyFloat32 = 3;
	static constexpr int32_t kElemFamilyFloat64 = 4;
	static constexpr int32_t kElemFamilyBool = 5;
	static constexpr int32_t kElemFamilyString = 6;
	static constexpr int32_t kElemFamilyDate = 7;
	static constexpr int32_t kElemFamilyTime = 8;
	static constexpr int32_t kElemFamilyTimestamp = 9;

	// The element family `type` reads into, or kElemFamilyNone if this library cannot read it.
	// `type` is a LEAF type -- a list/vector wrapper must already have been unwrapped by the
	// caller, since whether to unwrap is the caller's question, not this one's.
	static int32_t arrow_leaf_family(const std::shared_ptr<arrow::DataType> &type)
	{
		switch (type->id())
		{
		// Every integer physical type narrower than int64 is exactly representable in int32 or
		// int64, so the narrowest LOSSLESS Fortran kind is what it maps to. Note UINT32 needs
		// int64, not int32: its top half does not fit a signed 32-bit integer.
		case arrow::Type::INT8: return kElemFamilyInt32;
		case arrow::Type::INT16: return kElemFamilyInt32;
		case arrow::Type::INT32: return kElemFamilyInt32;
		case arrow::Type::UINT8: return kElemFamilyInt32;
		case arrow::Type::UINT16: return kElemFamilyInt32;
		case arrow::Type::INT64: return kElemFamilyInt64;
		case arrow::Type::UINT32: return kElemFamilyInt64;
		// UINT64 and the decimals have no lossless Fortran kind at all, and answer with the
		// CONVENTIONAL LOSSY target rather than "unknown": a caller asking "what do I declare?" is
		// better served by the kind this library will actually read the column into than by being
		// told a readable column is unreadable. A uint64 value above huge(int64) aborts on read,
		// and a decimal is read through double.
		case arrow::Type::UINT64: return kElemFamilyInt64;
		// Mapped on the type ID alone -- deliberately no precision/scale awareness, so
		// decimal(9,0) answers float64 like every other decimal rather than int32.
		case arrow::Type::DECIMAL32: return kElemFamilyFloat64;
		case arrow::Type::DECIMAL64: return kElemFamilyFloat64;
		case arrow::Type::DECIMAL128: return kElemFamilyFloat64;
		case arrow::Type::DECIMAL256: return kElemFamilyFloat64;
		case arrow::Type::HALF_FLOAT: return kElemFamilyFloat32;
		case arrow::Type::FLOAT: return kElemFamilyFloat32;
		case arrow::Type::DOUBLE: return kElemFamilyFloat64;
		case arrow::Type::BOOL: return kElemFamilyBool;
		case arrow::Type::STRING: return kElemFamilyString;
		case arrow::Type::LARGE_STRING: return kElemFamilyString;
		// STRING_VIEW is deliberately absent, matching what parquet_get_column_type has always
		// answered for one ("unknown"): the compact string read coerces such a column to an
		// offset string before reading it (recache_coerced_string_view), but the TYPE query never
		// claimed it, and this helper exists to be that query's single source of truth rather
		// than to change what it says.
		case arrow::Type::DATE32: return kElemFamilyDate;
		case arrow::Type::DATE64: return kElemFamilyDate; // GCOVR_EXCL_LINE -- DATE64 never actually produced (see CLAUDE.md's temporal notes).
		case arrow::Type::TIME32: return kElemFamilyTime;
		case arrow::Type::TIME64: return kElemFamilyTime;
		case arrow::Type::TIMESTAMP: return kElemFamilyTimestamp;
		default:
			return kElemFamilyNone;
		}
	}

	// The canonical data-type token for a family, as parquet_get_column_type reports it.
	static const char *elem_family_token(int32_t family)
	{
		switch (family)
		{
		case kElemFamilyInt32: return "int32";
		case kElemFamilyInt64: return "int64";
		case kElemFamilyFloat32: return "float32";
		case kElemFamilyFloat64: return "float64";
		case kElemFamilyBool: return "boolean";
		case kElemFamilyString: return "string";
		case kElemFamilyDate: return "date";
		case kElemFamilyTime: return "time";
		case kElemFamilyTimestamp: return "timestamp";
		default: return "unknown";
		}
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
		int32_t family = arrow_leaf_family(type);
		if (family == kElemFamilyNone)
		{
			// Not readable by this library at all (a MAP, a nested STRUCT reached as a whole, an
			// unsupported binary type, ...). Answering "unknown" rather than the raw Arrow type
			// name is what lets parquet_get_column_type report it instead of aborting: the query
			// exists to tell a caller whether a column can be read, so a type it cannot read is an
			// answer, not an error.
			copy_string_with_padding(buf, buf_len, std::string("unknown"));
			return 0;
		}
		copy_string_with_padding(buf, buf_len, std::string(elem_family_token(family)));
		return 1;
	}

	// Writes `name`'s CONTAINER SHAPE token ("scalar"/"vector"/"list"/"map"/"struct"/"unknown")
	// into `buf`, space-padded to buf_len. Schema-only: reads no column data at all, which is the
	// property that makes it usable from parquet_open_table's classification pass.
	//
	// Deliberately returns nothing, unlike parquet_reader_get_column_type_name beside it: EVERY
	// shape has a token, including the ones nothing can read, so there is no "not recognised" for
	// a return value to report. One that was always 1 would only have forced a dead local on the
	// Fortran side.
	//
	// Orthogonal to parquet_reader_get_column_type_name, which reports the ELEMENT type and
	// deliberately unwraps a list to it, so a list<double> answers "float64" there and "list"
	// here. Neither half is redundant: ("float64", "list") is a complete description of a column
	// and a caller needs both to decide what to declare.
	//
	// "vector" means a FIXED_SIZE_LIST -- and ONLY that. A plain LIST answers "list" even when its
	// data happens to be uniform and would read perfectly well into a 2-D array, because whether
	// it is uniform is a property of the DATA (see needs_data_to_measure_col_size) and answering
	// it would mean reading the column. That is the honest schema-level answer: the file declares
	// a variable-length list and says nothing about the lengths. A caller who wants to know
	// whether a 2-D read will work asks parquet_get_col_size, which does look.
	void parquet_reader_get_column_shape_name(void *handle, const char *name, char *buf, int64_t buf_len)
	{
		auto reader_handle = as_reader_handle(handle);
		auto resolved = resolve_struct_path(reader_handle->schema, name);
		auto type = resolved.leaf_field->type();
		const char *token = "unknown";
		switch (type->id())
		{
		case arrow::Type::FIXED_SIZE_LIST: token = "vector"; break;
		case arrow::Type::LIST: token = "list"; break;
		case arrow::Type::LARGE_LIST: token = "list"; break;
		case arrow::Type::MAP: token = "map"; break;
		// A bare STRUCT is not addressable at all -- resolve_struct_path refuses a path landing on
		// one, so this arm is only reachable for a TOP-LEVEL struct field, whose name
		// parquet_get_column_names does not emit either. Answering "struct" rather than aborting
		// keeps this query's contract identical to parquet_get_column_type's: a shape this
		// library cannot read is an ANSWER.
		case arrow::Type::STRUCT: token = "struct"; break; // GCOVR_EXCL_LINE
		default:
			// Every leaf this library can read one value at a time -- and equally every leaf it
			// cannot -- is a "scalar" in the only sense this query is about: it is not a
			// container. An unreadable ELEMENT type is the other query's business, not this one's.
			token = "scalar";
			break;
		}
		copy_string_with_padding(buf, buf_len, std::string(token));
	}

	// Returns 1 if `name`'s stored Arrow field is declared nullable, 0 if not -- the schema flag
	// only, reading no column data at all. Assumes `name` already resolves, same as
	// parquet_reader_get_column_type_name above (parquet_get_column_nullable in parquet_read.f90
	// probes existence first via check_column_exists).
	//
	// For a VECTOR column this deliberately reports the CHILD ("item") field's flag, not the outer
	// FIXED_SIZE_LIST/LIST field's. The outer one is a constant -- build_field always writes it
	// non-nullable, because a row's vector is never itself missing -- so answering it would make
	// the query useless for exactly the columns whose element nullability is interesting. A dotted
	// struct path reports the leaf's own flag, matching the rule
	// parquet_reader_get_column_type_name already follows.
	//
	// An unreadable column type is an ANSWER, not an error: the flag is a property of the stored
	// schema and is meaningful whether or not this library can decode the values, so a MAP or a
	// decimal column reports its flag rather than aborting -- the same precedent
	// parquet_reader_get_column_type_name sets with "unknown".
	int64_t parquet_reader_get_column_nullable(void *handle, const char *name)
	{
		auto reader_handle = as_reader_handle(handle);
		auto resolved = resolve_struct_path(reader_handle->schema, name);
		auto field = resolved.leaf_field;
		auto type_id = field->type()->id();
		if (type_id == arrow::Type::FIXED_SIZE_LIST || type_id == arrow::Type::LIST ||
			type_id == arrow::Type::LARGE_LIST)
		{
			field = field->type()->field(0);
		}
		return field->nullable() ? 1 : 0;
	}

	// Returns the declared vector-column element count of `name` (1 for a scalar column),
	// without reading any column data except for a plain LIST/LARGE_LIST. A FIXED_SIZE_LIST
	// column's width is a schema-level constant (arrow::FixedSizeListType::list_size()), so it's
	// read straight off the already in-memory schema -- the same schema-only introspection
	// pattern max_fixed_size_list_col_size/check_explicit_chunk_size_fits_arrow_limit use on the
	// write side. This is what lets col_size be queried on a multi-billion-element column
	// without ever materializing it (see get_single_chunk_array's whole-column read, and the
	// int32 element-count ceiling documented on parquet_reader_get_column_total_elements below
	// and in CLAUDE.md's "Guarding a hard Arrow int32-only ceiling"). A plain LIST/LARGE_LIST
	// column (only ever produced by a non-this-library writer -- this library always writes
	// FIXED_SIZE_LIST for vector columns) has no such schema-level constant, since its per-row
	// width can vary; that case, and ONLY that case, still falls back to get_col_size's own
	// data-scanning heuristic via get_single_chunk_array. Every other leaf type -- above all a
	// plain scalar column, whose width is 1 by construction -- returns without reading anything;
	// see needs_data_to_measure_col_size for why that matters so much to parquet_open_table.
	int64_t parquet_reader_get_column_col_size(void *handle, const char *name)
	{
		auto reader_handle = as_reader_handle(handle);
		auto resolved = resolve_struct_path(reader_handle->schema, name);
		if (resolved.leaf_field->type()->id() == arrow::Type::FIXED_SIZE_LIST)
		{
			return static_cast<int64_t>(std::static_pointer_cast<arrow::FixedSizeListType>(resolved.leaf_field->type())->list_size());
		}
		if (!needs_data_to_measure_col_size(resolved.leaf_field->type()))
		{
			return 1;
		}
		// A plain LIST/LARGE_LIST: screened from the footer, then proven one row group at a time,
		// so answering this never materializes the whole column (see the "Measuring a plain LIST
		// column's width" section above).
		return list_width_verified(reader_handle, name, 0, 0);
	}

	// Whether `name` contains any Null over row groups `rg_lo`..`rg_hi` (1-based inclusive;
	// rg_lo <= 0 meaning every row group), from the file footer alone -- no column data is read.
	// Returns 1 for "has nulls, or cannot be sure"; see column_has_nulls_from_footer for why the
	// uncertain cases must answer 1.
	int parquet_reader_column_has_nulls(void *handle, const char *name, int64_t rg_lo, int64_t rg_hi)
	{
		auto reader_handle = as_reader_handle(handle);
		return column_has_nulls_from_footer(reader_handle, name, rg_lo, rg_hi);
	}

	// Whether `name`'s width can only be determined by looking at its data -- i.e. whether it is a
	// plain LIST/LARGE_LIST. Schema-only, and the question parquet_table's open-time classification
	// asks so it can DEFER such a column's kind and width to first use instead of reading it at
	// open (see table_classify in parquet_tables_read.f90). Every other column, scalar or
	// FIXED_SIZE_LIST, is classified from the schema there and then.
	int parquet_reader_column_width_is_deferred(void *handle, const char *name)
	{
		auto reader_handle = as_reader_handle(handle);
		auto resolved = resolve_struct_path(reader_handle->schema, name);
		return needs_data_to_measure_col_size(resolved.leaf_field->type()) ? 1 : 0;
	}

	// The footer screen alone (no column data read): a CANDIDATE uniform width for `name` over the
	// 1-based inclusive row-group range [rg_lo, rg_hi] (rg_lo <= 0 meaning every row group), or 1
	// when no uniform width above 1 can exist. A candidate is unproven -- a caller that acts on one
	// must be able to survive it being wrong. parquet_table uses it on the read path, where the
	// read itself proves or rejects it (get_uniform_list_values checks every row).
	int64_t parquet_reader_list_width_candidate(void *handle, const char *name, int64_t rg_lo, int64_t rg_hi)
	{
		auto reader_handle = as_reader_handle(handle);
		auto resolved = resolve_struct_path(reader_handle->schema, name);
		if (!needs_data_to_measure_col_size(resolved.leaf_field->type()))
		{
			return parquet_reader_get_column_col_size_impl(reader_handle, name);
		}
		return list_width_candidate(reader_handle, name, rg_lo, rg_hi);
	}

	// The PROVEN uniform width of `name` over the 1-based inclusive row-group range [rg_lo, rg_hi]
	// (rg_lo <= 0 meaning every row group), 1 when the column is not a uniform vector column, or 0
	// when it has no rows at all.
	// Screens from the footer first and reads at most one row group at a time, so this is safe to
	// call on a column far larger than memory. parquet_table uses it for %kind/%width, which must
	// not answer with an unproven width.
	int64_t parquet_reader_list_width_verified(void *handle, const char *name, int64_t rg_lo, int64_t rg_hi)
	{
		auto reader_handle = as_reader_handle(handle);
		auto resolved = resolve_struct_path(reader_handle->schema, name);
		if (!needs_data_to_measure_col_size(resolved.leaf_field->type()))
		{
			return parquet_reader_get_column_col_size_impl(reader_handle, name);
		}
		return list_width_verified(reader_handle, name, rg_lo, rg_hi);
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
		if (!needs_data_to_measure_col_size(resolved.leaf_field->type()))
		{
			return nrows;
		}
		// A plain LIST/LARGE_LIST. Screened from the footer, then proven one row group at a time --
		// the SAME helper parquet_reader_get_column_col_size uses, so the two size queries answer
		// identically and neither materializes the whole column (see the "Measuring a plain LIST
		// column's width" section above).
		//
		// This used to be get_single_chunk_array + get_col_size, i.e. decode every row group at
		// once purely to answer a size query. That is the exact peak-memory hazard the
		// FIXED_SIZE_LIST branch above exists to avoid, and it hid well: the answer was correct
		// either way, so only a memory measurement could see it. The two helpers agree by
		// construction -- get_col_size returns 0 for an empty column, 1 for a ragged one, else the
		// uniform width, which is list_width_verified's contract exactly -- so this is a cost fix,
		// not a behaviour change. list_width_verified still takes the whole-column path when a
		// filter mask or a sort permutation is active, because a per-row-group width would then
		// answer about rows the caller removed; that branch lives in the helper, where both
		// callers get it.
		//
		// Calls the static helper rather than the exported parquet_reader_get_column_col_size:
		// going through the exported function would re-enter as_reader_handle on the same handle
		// while this call's own guard is still held, which ConcurrencyGuard now admits (the owner
		// may re-enter) but only after a second, pointless atomic claim/release pair.
		return nrows * list_width_verified(reader_handle, name, 0, 0);
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

	// Fills `out` with the 1-based PHYSICAL file row index of each row this reader currently
	// returns, in the order it returns them. `n` must be the reader's own row count
	// (parquet_get_nrows), which is what every caller already has.
	//
	// This is the one thing a caller cannot work out for itself once a transform is active: which
	// file rows survived a filter is inside live_mask, and what order a sort put them in is inside
	// sort_perm, and neither is otherwise visible. Without a transform it is plain arithmetic and
	// this function is simply the general form of it.
	//
	// The walk is deliberately the same shape as every other mask-aware walk in this file: row
	// groups in file order, each row group's own mask segment, skipping the ones the statistics
	// screen or a scoped range excluded (row_group_live_offsets[rg] < 0), because live_mask holds
	// no bits at all for those -- that is the memory saving it exists for.
	void parquet_reader_physical_row_indices(void *handle, int64_t *out, int64_t n)
	{
		auto reader_handle = as_reader_handle(handle);
		if (n != reader_handle->nrows)
		{
			// One line, deliberately: gcovr's --exclude-lines-by-pattern only matches the line a
			// report_fatal_error call STARTS on, so a wrapped argument list leaves its own
			// continuation counted as an ordinary -- and permanently uncovered, since the abort
			// discards this run's counters -- line. Keep every call in this file on one line.
			report_fatal_error("parquet_reader_physical_row_indices", "row count does not match the reader's own");
		}
		if (n == 0) return;
		// Live, surviving rows in FILE order first; the sort permutation (if any) reorders this
		// afterwards, exactly as apply_row_transform applies filter-then-sort.
		std::vector<int64_t> physical;
		physical.reserve(static_cast<size_t>(n));
		if (!reader_handle->live_mask)
		{
			for (int64_t i = 0; i < reader_handle->total_nrows; ++i)
			{
				physical.push_back(i + 1);
			}
		}
		else
		{
			const int64_t nrg = static_cast<int64_t>(reader_handle->row_group_live_offsets.size());
			for (int64_t rg = 0; rg < nrg; ++rg)
			{
				const int64_t live_at = reader_handle->row_group_live_offsets[rg];
				if (live_at < 0) continue;
				const int64_t first = reader_handle->row_group_offsets[rg];
				const int64_t rows = reader_handle->row_group_offsets[rg + 1] - first;
				for (int64_t i = 0; i < rows; ++i)
				{
					if (!reader_handle->live_mask->Value(live_at + i)) continue;
					physical.push_back(first + i + 1);
				}
			}
		}
		if (static_cast<int64_t>(physical.size()) != n)
		{
			report_fatal_error("parquet_reader_physical_row_indices", "surviving row count does not match the reader's own");
		}
		if (!reader_has_sort_permutation(reader_handle))
		{
			for (int64_t i = 0; i < n; ++i) out[i] = physical[static_cast<size_t>(i)];
			return;
		}
		auto perm = std::static_pointer_cast<arrow::Int64Array>(reader_handle->sort_perm);
		for (int64_t i = 0; i < n; ++i)
		{
			const int64_t src = perm->Value(i);
			if (src < 0 || src >= n)
			{
				report_fatal_error("parquet_reader_physical_row_indices", "sort permutation entry out of range");
			}
			out[i] = physical[static_cast<size_t>(src)];
		}
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

	// Bytes currently held by Arrow's process-wide default memory pool.
	//
	// Diagnostic only, and deliberately NOT declared in src/parquet_bindings.f90: the one
	// consumer is bench/benchmark_table.f90, which declares its own local bind(C) interface for
	// it (the same convention the debug-only g_debug_* setters use). It exists because RSS
	// cannot answer the question it answers -- Arrow's pool does not return freed pages to the
	// OS, so a released column buffer keeps counting toward RSS while no longer counting here.
	// That difference is what distinguishes "parquet_reader_release_column worked" from "the
	// buffers are still alive".
	//
	// Not called from anything fpm test runs, and deliberately so: it reads a PROCESS-GLOBAL
	// counter, and test-drive runs a suite's tests concurrently, so every other test's Arrow
	// allocations would land in the same number -- see the comment on
	// scenario_filter_scoped_reads_no_whole_column in test/error_scenarios.f90, which explains why
	// an earlier version of a check built on this counter had to move out of the ordinary test
	// suite for exactly that reason. bench/benchmark_table.f90 (a manual, never-fpm-test tool -- see
	// CLAUDE.md's "Manual (never-fpm test) large-scale/benchmark tools") is this function's only
	// caller, run single-process by a human, where the counter is meaningful.
	int64_t parquet_get_arrow_bytes_allocated() // GCOVR_EXCL_START
	{
		return arrow::default_memory_pool()->bytes_allocated();
	}
	// GCOVR_EXCL_STOP

	// Drops column `name`'s decoded Arrow array from column_cache, freeing its buffers, so a
	// caller that has already copied the values Fortran-side does not keep a second full copy
	// alive for the reader's whole lifetime (parquet_tables' materialization does exactly this).
	// A later read of the same column simply re-reads and re-decodes it -- this is a pure memory/
	// time trade, never a correctness change, and it composes with an active filter or sample
	// because those are re-applied to every freshly decoded column by get_single_chunk_array.
	//
	// `name` may be a dotted struct-leaf path; the cache is keyed by TOP-LEVEL field index, so
	// releasing any leaf releases the whole struct's array -- a caller walking several leaves of
	// one struct should release only after its last leaf, or it will re-read the struct each time.
	// Deliberately non-throwing and forgiving: an unknown name, or a column that was never read,
	// is a silent no-op rather than an error (there is nothing a caller could usefully do about
	// either, and an exception here would cross the extern "C" boundary uncaught).
	void parquet_reader_release_column(void *handle, const char *name)
	{
		auto reader_handle = as_reader_handle(handle);
		std::string full_name(name);
		// Mirrors resolve_struct_path's "an exact top-level match always wins" rule, without
		// needing its (throwing) leaf-type validation -- all this needs is the cache key.
		int idx = reader_handle->schema->GetFieldIndex(full_name);
		if (idx < 0)
		{
			auto dot = full_name.find('.');
			if (dot == std::string::npos) return;
			idx = reader_handle->schema->GetFieldIndex(full_name.substr(0, dot));
			if (idx < 0) return;
		}
		if (reader_handle->column_cache.erase(idx) > 0) reader_handle->was_released.insert(idx);
	}

	// parquet_get_column_names (parquet_read.f90) walks the reader's column_path_cache once
	// through these three accessors -- the same length-then-copy convention the table-metadata
	// accessors just above use. `index` is 0-based. Nothing here reads column data: the cache was
	// built from the schema alone at open time.
	int32_t parquet_reader_get_column_count(void *handle)
	{
		auto reader_handle = as_reader_handle(handle);
		return static_cast<int32_t>(reader_handle->column_path_cache.size());
	}

	// Returns the column path at `index`, or aborts via report_fatal_error if out of range.
	static const std::string &column_path_at(ParquetReaderHandle *reader_handle, int32_t index, const char *context)
	{
		if (index < 0 || index >= static_cast<int32_t>(reader_handle->column_path_cache.size()))
		{
			report_fatal_error(context, "column index out of range");
		}
		return reader_handle->column_path_cache[static_cast<size_t>(index)];
	} // GCOVR_EXCL_LINE -- gcov attribution artifact: this closing brace shows uncovered even though the covered `return` above proves the body ran.

	// Returns the byte length of column `index`'s (possibly dotted) name.
	int64_t parquet_reader_get_column_name_length(void *handle, int32_t index)
	{
		auto reader_handle = as_reader_handle(handle);
		return static_cast<int64_t>(
			column_path_at(reader_handle, index, "parquet_reader_get_column_name_length").size());
	}

	// Copies column `index`'s (possibly dotted) name into `buf`.
	void parquet_reader_get_column_name(void *handle, int32_t index, char *buf, int64_t buf_len)
	{
		auto reader_handle = as_reader_handle(handle);
		copy_string_with_padding(buf, buf_len, column_path_at(reader_handle, index, "parquet_reader_get_column_name"));
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

	// Masks every IEEE trap for its lifetime, then restores the caller's floating-point
	// environment exactly as it was. Needed for exactly ONE call -- arrow::compute::MinMax in
	// compute_stat_min_max below -- and deliberately not applied file-wide or at the bind(C)
	// boundary: a wider guard would also swallow a genuine FP fault in this library's own code,
	// which is the very thing a caller who turned trapping on is trying to see.
	//
	// Why it is needed at all: nagfor's DEFAULT (-ieee=stop) unmasks the IEEE traps for the whole
	// process, this file included, so a raised flag becomes SIGFPE wherever it is raised. Arrow's
	// min_max kernel raises FE_INVALID internally on every non-empty floating-point array --
	// benignly (the min/max it returns is correct) and independently of the data: a one-element
	// array raises it, and so does an array whose every element is Null, where no value is ever
	// compared. A length-0 array does not, and an integer array does not. Until this guard existed,
	// a NAG-built program calling parquet_close_reader(print_stat=.true.) on a reader that had
	// touched a float32/float64 column died inside Arrow with "Arithmetic exception: Floating
	// invalid operation", on data containing nothing exceptional at all.
	//
	// Scoped this narrowly on evidence, not on hope (Arrow 25, macOS): Filter, Take, Cast and Sum
	// all stay clear even on input holding NaN, +Inf and -0.0, and so do this file's own float
	// comparisons (eval_filter_clause, screen_row_groups), because the compiler emits the quiet
	// ucomisd for them. Re-measure with fetestexcept before adding a second guard elsewhere.
	//
	// The destructor CLEARS what Arrow raised and then restores with fesetenv -- never feupdateenv,
	// which re-raises the currently-flagged exceptions and would therefore trap on the way out,
	// under the caller's own unmasked environment, defeating the entire guard.
	struct ScopedMaskedFpTraps
	{
		std::fenv_t saved_env;
		ScopedMaskedFpTraps() { (void)std::feholdexcept(&saved_env); } // saves, clears, masks
		~ScopedMaskedFpTraps()
		{
			(void)std::feclearexcept(FE_ALL_EXCEPT);
			(void)std::fesetenv(&saved_env);
		}
		ScopedMaskedFpTraps(const ScopedMaskedFpTraps &) = delete;
		ScopedMaskedFpTraps &operator=(const ScopedMaskedFpTraps &) = delete;
	};

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

		// Arrow's min_max raises FE_INVALID on any non-empty float array, which is fatal under a
		// caller whose IEEE traps are unmasked (nagfor's default). See ScopedMaskedFpTraps above.
		ScopedMaskedFpTraps fp_traps_masked;
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
		// Solicited output: the caller asked for this report, so it is governed by verbosity exactly
		// as %print_stat and %print_schema_info are on the Fortran side. The rule is about who
		// asked, not about which language the printer happens to be written in.
		if (output_is_suppressed()) return;
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
			// A column released via parquet_reader_release_column is still "touched" (was_read/
			// was_prefetched are never cleared) but its array is gone, so every value-derived
			// cell below is unavailable. Report the row with those cells marked "released"
			// rather than either dropping the row (losing the fact that it was read at all) or
			// looking it up unconditionally -- an .at() miss here would throw across the
			// extern "C" boundary uncaught, i.e. abort the process from a diagnostic call.
			auto cached = reader_handle->column_cache.find(idx);
			if (cached == reader_handle->column_cache.end())
			{
				rows.push_back({
					field->name(), describe_parquet_type(field), "", "", "",
					reader_handle->was_released.count(idx) ? "released" : "-", "-", "-", "", "", "",
					reader_handle->was_prefetched.count(idx) ? "yes" : "no",
					reader_handle->was_read.count(idx) ? "yes" : "no",
					"",
				});
				continue;
			}
			auto array = cached->second;
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
			std::fprintf(stdout, "columns: %d   shown: %zu   rows: %lld (of %lld total)\n",
				reader_handle->schema->num_fields(), rows.size(),
				static_cast<long long>(reader_handle->nrows), static_cast<long long>(reader_handle->total_nrows));
			// GCOVR_EXCL_STOP
		}
		else
		{
			std::fprintf(stdout, "columns: %d   shown: %zu   rows: %lld\n",
				reader_handle->schema->num_fields(), rows.size(), static_cast<long long>(reader_handle->nrows));
		}
		if (reader_handle->has_sample)
		{
			std::fprintf(stdout, "sample: fraction=%.6g seed=%lld\n", reader_handle->sample_fraction,
				static_cast<long long>(reader_handle->sample_seed_used));
		}
		// The whole expression, as re-rendered from the parse tree. The per-column "filter" cell
		// below can only list the leaves that mention that column, which is lossy the moment an
		// expression is not a flat AND -- this line is what stays exact.
		if (!reader_handle->filter_expr_text.empty())
		{
			std::fprintf(stdout, "filter: %s\n", reader_handle->filter_expr_text.c_str());
		}
		// The sort keys as given, in the order they were added. Its own line for the same reason
		// the filter expression has one: it cannot be decomposed into the per-column table below.
		if (!reader_handle->sort_key_text.empty())
		{
			std::fprintf(stdout, "sort: %s\n", reader_handle->sort_key_text.c_str());
		}
		// What the row-group statistics pre-screen managed to skip. Printed only when it actually
		// pruned something, so the line is a statement that the optimization engaged rather than
		// noise on every filtered read -- and it is the only way a user can see that it did.
		if (reader_handle->row_groups_pruned > 0)
		{
			std::fprintf(stdout, "screened: %lld of %lld row groups skipped (statistics)\n",
				static_cast<long long>(reader_handle->row_groups_pruned),
				static_cast<long long>(reader_handle->num_row_groups));
		}
		std::fprintf(stdout, "\n");

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
		// Arrow already knows the null count, so a column with none needs no per-element scan at
		// all -- the mask it would build is uniformly 1. Worth short-circuiting because the callers
		// that pass a non-null valid_out include every parquet_table materialize, and the scan is
		// O(n) per column with no other purpose. Kept as a fast path rather than the only path:
		// a column that genuinely has Nulls still needs the element-by-element answer.
		if (array->null_count() == 0)
		{
			std::memset(valid_out, 1, static_cast<size_t>(array->length()));
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

	// Maps a 1-based row index to the (1-based row_group, 1-based local row-within-group) pair
	// that get_row_group_chunk_array/get_row_list_values need, by walking each row group's own
	// row count -- never reads any column data. Used by read_list_primitive_row
	// (parquet_read_array_row_mode) so a single row of a vector column can be fetched by reading
	// only the one row group it lives in, instead of materializing the whole column (see
	// get_single_chunk_array's int32 element-count ceiling, documented in CLAUDE.md's "Guarding a
	// hard Arrow int32-only ceiling"). Aborts via report_fatal_error if row_index is out of range.
	//
	// The row count it walks is row_group_effective_rows, i.e. the SURVIVING count when a filter/
	// sample mask is active and the physical footer count otherwise -- which is what makes this
	// work under a mask too. Both sides of the mapping shift together: `row_index` then means
	// "index into the filtered result" (the library's uniform contract -- as if the file only
	// contained the matching rows), and get_row_group_chunk_array hands back that row group's
	// chunk with its own mask segment already applied, so the local index this returns addresses
	// the surviving rows of that chunk directly. A row group all of whose rows were filtered away
	// contributes 0 here and is stepped over, exactly as a physically empty one already was.
	static void resolve_row_group_for_row(ParquetReaderHandle *reader_handle, int64_t row_index,
		const char *context, int64_t &row_group_out, int64_t &local_row_out)
	{
		if (row_index < 1)
		{
			report_fatal_error(context, "row_index out of bounds");
		}
		int64_t remaining = row_index;
		for (int64_t rg = 0; rg < reader_handle->num_row_groups; ++rg)
		{
			int64_t rg_rows = row_group_effective_rows(reader_handle, rg + 1);
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

	// The single decision every row-mode read makes about WHERE its row comes from, so all four
	// entry-point families (the numeric templates, the hand-written bool8 and string pair, and the
	// temporal templates) share one answer instead of four copies of it.
	//
	// Without a sort: resolve the one row group the row lives in and read only that -- the whole
	// point of row mode, and it holds under a filter/sample mask too, because a mask only removes
	// rows (resolve_row_group_for_row walks surviving counts).
	//
	// Under a SORT: there is no such row group. Sorted row 5 may come from row group 47 and row 6
	// from row group 3, so the only coherent source is the whole column -- which
	// get_single_chunk_array already returns filtered AND sorted (apply_row_transform), making
	// `row_index` a direct index into it. This is inherent to sorting, not a limitation to be
	// lifted later, and it is the documented cost of a sorted row-mode read.
	static std::shared_ptr<arrow::Array> fetch_row_mode_array(ParquetReaderHandle *reader_handle,
		const char *name, int64_t row_index, const char *context, int64_t &local_row_index)
	{
		if (reader_has_sort_permutation(reader_handle))
		{
			if (row_index < 1 || row_index > reader_handle->nrows)
			{
				report_fatal_error(context, "row_index out of bounds");
			}
			local_row_index = row_index;
			return get_single_chunk_array(reader_handle, name);
		}
		int64_t row_group = 0;
		resolve_row_group_for_row(reader_handle, row_index, context, row_group, local_row_index);
		return get_row_group_chunk_array(reader_handle, name, row_group, context);
	}

extern "C"
{

	// ==== Numeric/string/temporal column reads: scalar, array, row mode, element mode ====
	//
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
		// via an abort-ending extended-source-type overflow scenario (the fatal exit there discards
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
			for (int64_t i = 0; i < n; ++i) data[i] = static_cast<int32_t>(small_integer_value_at(vals.get(), offset + i * stride));
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
				double v = real_family_value_at(vals.get(), offset + i * stride);
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
				auto status = decimal_to_int64_checked(vals.get(), offset + i * stride, v64);
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
			for (int64_t i = 0; i < n; ++i) data[i] = small_integer_value_at(vals.get(), offset + i * stride);
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
				double v = real_family_value_at(vals.get(), offset + i * stride);
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
				auto status = decimal_to_int64_checked(vals.get(), offset + i * stride, v64);
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
			for (int64_t i = 0; i < n; ++i) data[i] = static_cast<float>(small_integer_value_at(vals.get(), offset + i * stride));
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
			for (int64_t i = 0; i < n; ++i) data[i] = static_cast<float>(real_family_value_at(vals.get(), offset + i * stride));
			break;
		}
		case arrow::Type::DECIMAL32:
		case arrow::Type::DECIMAL64:
		case arrow::Type::DECIMAL128:
		case arrow::Type::DECIMAL256:
		{
			for (int64_t i = 0; i < n; ++i) data[i] = static_cast<float>(decimal_value_at(vals.get(), offset + i * stride));
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
			for (int64_t i = 0; i < n; ++i) data[i] = static_cast<double>(small_integer_value_at(vals.get(), offset + i * stride));
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
			for (int64_t i = 0; i < n; ++i) data[i] = real_family_value_at(vals.get(), offset + i * stride);
			break;
		}
		case arrow::Type::DECIMAL32:
		case arrow::Type::DECIMAL64:
		case arrow::Type::DECIMAL128:
		case arrow::Type::DECIMAL256:
		{
			for (int64_t i = 0; i < n; ++i) data[i] = decimal_value_at(vals.get(), offset + i * stride);
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
	// Reads only the one row group the requested row lives in -- never the whole column -- under a
	// filter/sample mask as well as without one. A SORT is the one case that forces the whole
	// column, because sorted row i has no row group of its own; fetch_row_mode_array owns that
	// decision for every row-mode family.
	int64_t local_row_index = row_index;
	auto array = fetch_row_mode_array(reader_handle, name, row_index, "parquet_read_array_row_mode", local_row_index);
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
// file.
//
// Works under a filter/sample mask for the same reason row mode does (see
// resolve_row_group_for_row's comment): each row group's row count comes from
// row_group_effective_rows, so it is the SURVIVING count when a mask is active -- matching what
// get_row_group_chunk_array actually hands back for that row group -- and the row_offset the
// loop accumulates therefore walks the filtered result in order, ending at the reader's own
// (filtered) nrows. A row group with no survivors is skipped by the same `rg_rows == 0` test
// that already skipped a physically empty one.
// `per_row_group` is invoked once per non-empty row group with (row_group_array,
// row_group_vals, row_group_nrows, row_offset); the caller writes into its own data/valid_out at
// row_offset (this helper doesn't know the CType/bool/string specifics of what to write).
// Returns the last row group's own array (for mark_read's bookkeeping call), or nullptr if the
// column has zero rows (no row groups at all, or every row group reported zero rows).
template <typename Fn>
static std::shared_ptr<arrow::Array> stream_element_mode_row_groups(
	ParquetReaderHandle *reader_handle, const char *name, int64_t col_size, int64_t nrows, const char *context,
	Fn &&per_row_group)
{
	int64_t row_offset = 0;
	std::shared_ptr<arrow::Array> last_array;
	// Under a sort there are no row groups to stream: sorted row i can come from any of them, so
	// the only coherent source is the whole (filtered and sorted) column. Handling it HERE, rather
	// than in each of the four element-mode entry-point families, is what keeps the fallback to one
	// place -- every caller's per_row_group body is written against (array, vals, rows, offset) and
	// works unchanged when that is called once for the whole column. Inherent to sorting; see
	// fetch_row_mode_array for row mode's counterpart.
	if (reader_has_sort_permutation(reader_handle))
	{
		if (nrows == 0) return nullptr;
		auto array = get_single_chunk_array(reader_handle, name);
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size, context);
		per_row_group(array, vals_any, nrows, static_cast<int64_t>(0));
		return array;
	}
	for (int64_t rg = 0; rg < reader_handle->num_row_groups; ++rg)
	{
		int64_t rg_rows = row_group_effective_rows(reader_handle, rg + 1);
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
// Decides what nullability a STREAMED column's Arrow field is built with, and enforces that every
// row group agrees about it. Called from every parquet_write_*_column_chunk site, once per chunk.
//
// Three rules, in this order:
//
//   PROTECTED wins, and short-circuits everything else. A protected column may hold no Null at all
//   (enforced Fortran-side by parquet_check_protected, which runs BEFORE this and aborts with a
//   message naming the column), so its field is non-nullable whatever any mask says. Note this also
//   means Rule 2 below never fires for a protected column: Fortran erases an all-.true. mask for
//   one, so both the first chunk and every later chunk look unmasked here. That is deliberate --
//   for a protected column the mask carries no information about nullability, protection having
//   already settled it -- and it is why the guidance ("use the same masked/unmasked form in every
//   row group") is uniform while the enforcement is not.
//
//   ALWAYS-NULLABLE kinds (`always_nullable`): temporal columns and a parquet_string_column carry
//   their null state inside the element rather than in a caller-supplied mask, so "was a mask
//   passed" says nothing about whether a LATER row group will contain a Null. Fixing such a field
//   non-nullable from a null-free first chunk would have Parquet reject a genuine Null in row
//   group 7, so they stay nullable unless protected.
//
//   Otherwise RULE 1 (presence) and RULE 2 (consistency). The field is nullable iff the FIRST
//   chunk carried an is_valid mask -- regardless of that mask's values, since the first row group
//   cannot know what later ones will hold. Every later chunk must then match: masked stays masked,
//   unmasked stays unmasked, and a mismatch is a hard error rather than a silently ignored mask or
//   a Null that cannot be written.
static bool resolve_chunk_nullability(ParquetWriterHandle *writer_handle, const char *name, size_t idx,
	bool first_chunk_ever, bool mask_present, bool always_nullable = false)
{
	if (writer_handle->protected_columns.count(name) != 0) return false;
	if (always_nullable) return true;

	if (first_chunk_ever)
	{
		writer_handle->chunk_mask_present[idx] = mask_present;
		return mask_present;
	}

	auto it = writer_handle->chunk_mask_present.find(idx);
	if (it == writer_handle->chunk_mask_present.end()) return mask_present; // GCOVR_EXCL_LINE
	if (it->second != mask_present)
	{
		report_fatal_error("parquet_write_column_chunk", "column '" + std::string(name) + "': " +
			(mask_present
				? "this row group passes an is_valid mask, but the first row group did not -- a "
				  "streamed column's nullability is fixed by its first row group, so pass an "
				  "all-.true. is_valid mask there too if any later row group may contain a Null"
				: "this row group passes no is_valid mask, but the first row group did -- every row "
				  "group of a streamed column must use the same masked or unmasked form")); // GCOVR_EXCL_LINE
	}
	return it->second;
}

static void stash_temporal_column_chunk(ParquetWriterHandle *writer_handle, const char *name, size_t idx,
	bool first_chunk_ever, const std::shared_ptr<arrow::Array> &array,
	const std::shared_ptr<arrow::DataType> &value_type, int64_t col_size)
{
	// always_nullable: a temporal element carries its own null state, so this chunk having no
	// null says nothing about row group 7. Nullable unless the column is protected, which is the
	// only way a caller can declare a temporal column null-free -- see resolve_chunk_nullability.
	bool nullable = resolve_chunk_nullability(writer_handle, name, idx, first_chunk_ever,
		/*mask_present=*/true, /*always_nullable=*/true);
	if (first_chunk_ever)
	{
		if (writer_handle->fields.size() <= idx) writer_handle->fields.resize(idx + 1);
		writer_handle->fields[idx] = build_field(name, value_type, col_size, nullable);
	}
	if (writer_handle->arrays.size() <= idx) writer_handle->arrays.resize(idx + 1);
	writer_handle->pending_chunk_arrays[static_cast<int>(idx)] = align_array_to_field(writer_handle->fields[idx], array);
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
	// A CONTAINER column reports its PAYLOAD's unit, for the same reason a vector column does: the
	// unit is a property of the values, and a list<timestamp[ns]> has exactly one of them. Without
	// this the query ABORTED for such a column, which meant parquet_table could not record the
	// resolution at classification -- and a list[timestamp[ns]] column read into a table and
	// written back came out as the writer's default microseconds, silently.
	//
	// A MAP reports its VALUE type's unit; its keys are always strings and have none. A map whose
	// value type is not temporal falls through to the caller's own "not a time/timestamp column"
	// abort exactly as before, because map_value_type returns the value type unchanged.
	if (type->id() == arrow::Type::LIST || type->id() == arrow::Type::LARGE_LIST)
	{
		return type->field(0)->type();
	}
	if (type->id() == arrow::Type::MAP)
	{
		return std::static_pointer_cast<arrow::MapType>(type)->item_type();
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
	// One row group when unsorted, the whole column when sorted -- see fetch_row_mode_array.
	int64_t local_row_index = row_index;
	auto array = fetch_row_mode_array(reader_handle, name, row_index, context, local_row_index);
	auto vals_any = get_row_list_values(array, name, local_row_index, col_size, context);
	report_nulls_list_full(array, vals_any, name, 1, col_size, local_row_index - 1, valid_out, context);
	if (unit_out) *unit_out = timestamp_unit_selector_of(vals_any, name, context);
	convert(vals_any, data, col_size, name, context, 1, 0);
	fill_null_default(data, valid_out, col_size);
	mark_read(reader_handle, name, type_name, array);
}

// Shared body for every temporal parquet_read_*_array_element entry point -- the temporal
// counterpart of read_list_primitive_element. Streams row group by row group exactly as that
// function does, filtered or not.
template <typename OutType, typename ConvertFn>
static void read_temporal_element(void *handle, const char *name, int64_t col_index, OutType *data, int64_t nrows,
	int8_t *valid_out, const char *context, const char *type_name, int32_t *unit_out, ConvertFn convert)
{
	auto reader_handle = as_reader_handle(handle);
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
		auto t_dec = std::chrono::steady_clock::now();
		auto array = get_single_chunk_array(reader_handle, name);
		charge_phase(t_dec, g_debug_string_read_decode_nanos);
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
		auto t_copy = std::chrono::steady_clock::now();
		for (int64_t i = 0; i < nrows; ++i)
		{
			auto view = arr.get_view(i);
			copy_string_with_padding(data + i * item_len, item_len, view);
		}
		charge_phase(t_copy, g_debug_string_read_copy_nanos);
		fill_null_default_string(data, item_len, valid_out, nrows);
		mark_read_string(reader_handle, name, item_len, array);
	}

	// Compact counterpart to parquet_read_string_column, above: instead of copying one string at
	// a time into a fixed-width padded Fortran buffer, hands back the decoded column's own
	// offsets/data/validity buffers directly (see extract_string_buffers), for the caller
	// (parquet_read.f90) to bulk-append straight into a parquet_string_column via its own
	// append_buffers. For a plain (non-struct) column the array is already kept alive indefinitely
	// by get_single_chunk_array's own column_cache; for a dotted struct-field path it is instead a
	// freshly built, uncached array (see unwrap_struct_path) that would otherwise be destroyed the
	// moment this function returns -- reader_handle->last_whole_column_buffers_array pins it either
	// way (cheap, and uniform for both cases), same idea as last_chunk_buffers_array below.
	void parquet_read_string_column_buffers(void *handle, const char *name,
		int64_t *nrows_out, int64_t *nchars_out,
		const void **offsets_out, const void **data_out, const void **validity_out,
		int8_t *offsets_int32_out, int64_t *validity_offset_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		// A STRING_VIEW column has no offsets/data pair to hand over, so it is converted to one
		// rather than refused: the cast result REPLACES the cache entry (see
		// recache_coerced_string_view), so a second compact read of the same column is free.
		array = recache_coerced_string_view(reader_handle, name, array, "parquet_read_column");
		if (!is_offset_string_type(array->type_id()))
		{
			report_fatal_error("parquet_read_column", std::string("type mismatch for column: ") + name +
				" (expected string, got " + array->type()->ToString() + ")"); // GCOVR_EXCL_LINE
		}
		reader_handle->last_whole_column_buffers_array = array;
		extract_string_buffers(array, nrows_out, nchars_out, offsets_out, data_out, validity_out, offsets_int32_out,
			validity_offset_out);
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
		// See fetch_row_mode_array: one row group, or the whole column under a sort.
		int64_t local_row_index = row_index;
		auto array = fetch_row_mode_array(reader_handle, name, row_index, "parquet_read_bool8_array_row", local_row_index);
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
		// See fetch_row_mode_array: one row group, or the whole column under a sort.
		int64_t local_row_index = row_index;
		auto array = fetch_row_mode_array(reader_handle, name, row_index, "parquet_read_string_array_row", local_row_index);
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
	// Streams row group by row group -- see stream_element_mode_row_groups's own comment for why
	// element mode (unlike row_mode) needs every row group, not just one.
	void parquet_read_bool8_array_element(void *handle, const char *name, int64_t col_index, int8_t *data, int64_t nrows, int64_t, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		const char *context = "parquet_read_bool8_array_element";

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
	// output). Streams row group by row group, same as parquet_read_bool8_array_element above.
	void parquet_read_string_array_element(void *handle, const char *name, int64_t col_index, char *data, int64_t item_len, int64_t nrows, int64_t, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		const char *context = "parquet_read_string_array_element";

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
	// whole column. Works on a filtered/sampled reader too: the mask spans the whole physical file
	// in file order and row groups partition those same rows contiguously, so row group N's own
	// mask is just a slice of it (row_group_mask_segment), applied here before the chunk is handed
	// back. A row group whose rows were all filtered away yields a zero-length chunk, which is the
	// normal case under a selective filter rather than an error. Also records `row_group` as read (for
	// parquet_reader_check_complete) and runs per-row-group qc (run_qc_checks -- reusing the exact
	// same whole-column check functions, just scoped to this one row group's own array: a
	// hard-mode violation aborts naming this row group, a soft-mode one warns at most once per
	// column, same throttling as every other read path).
	static std::shared_ptr<arrow::Array> get_row_group_chunk_array(ParquetReaderHandle *reader_handle,
		const char *name, int64_t row_group, const char *context)
	{
		auto resolved = resolve_struct_path(reader_handle->schema, name);
		auto idx = get_column_index(reader_handle, resolved.top_level_name.c_str());
		std::vector<int> leaf_indices;
		resolve_chunk_leaf_indices(reader_handle, static_cast<int>(idx), resolved.child_path, leaf_indices);
		auto result = reader_handle->reader->ReadRowGroup(static_cast<int>(row_group - 1), leaf_indices);
		if (!result.ok())
		{ // GCOVR_EXCL_START -- I/O backstop: row_group is already validated by
		  // resolve_row_group_for_row before this is ever called.
			throw std::runtime_error(result.status().ToString());
		}
		// GCOVR_EXCL_STOP
		std::shared_ptr<arrow::Table> table = result.ValueOrDie();
		auto array = combine_column_chunks(table->column(0), resolved.top_level_name);
		if (!resolved.child_path.empty())
		{
			array = unwrap_struct_path(array, resolved.child_path);
		}
		// Apply this row group's own slice of the mask, so a chunked read on a filtered/sampled
		// reader yields exactly the surviving rows -- the same count parquet_get_chunk_size
		// reports for it. Done before qc, so qc validates only the rows the caller actually
		// receives (feature_table.md D9: QC applies after filtering).
		auto segment = row_group_mask_segment(reader_handle, row_group);
		if (segment)
		{
			ensure_compute_initialized();
			auto coerced = coerce_string_view_to_offset_string(array);
			if (!coerced.ok())
			{ // GCOVR_EXCL_START -- Cast-kernel Status backstop on an already-decoded array.
				report_fatal_error(context, std::string("failed to filter row group for column: ") + name);
			}
			// GCOVR_EXCL_STOP
			auto filtered = arrow::compute::Filter(coerced.ValueOrDie(), segment);
			if (!filtered.ok())
			{ // GCOVR_EXCL_START -- Filter-kernel Status backstop on an already-decoded array.
				report_fatal_error(context, std::string("failed to filter row group for column: ") + name);
			}
			// GCOVR_EXCL_STOP
			array = filtered.ValueOrDie().make_array();
		}
		reader_handle->chunk_read_row_groups[static_cast<int>(idx)].insert(row_group);
		run_qc_checks(reader_handle, name, std::string(name) + " [row group " + std::to_string(row_group) + "]", array);
		return array;
	}


	// ==== Variable-length LIST column reads ====
	//
	// The path a genuinely ragged LIST column takes into a Fortran parquet_list_column, and the
	// only read path in this file whose result is not a flat rectangle. It sits BESIDE
	// get_uniform_list_values rather than replacing it: a list column whose rows all have the
	// same length still reads into a 2-D array through that one, and the caller's chosen output
	// type is what picks the interpretation.
	//
	// TWO CROSSINGS, AND NOTHING ARROW-OWNED CROSSES EITHER OF THEM. Fortran first asks for the
	// shape (row count, element count, payload byte count, element family, temporal unit), which
	// is what lets it allocate its buffers and %init the payload column to the right kind; it
	// then asks for the fill, which COPIES into those Fortran-owned buffers.
	//
	// The alternative -- handing back Arrow's own buffers, as parquet_read_string_column_buffers
	// does -- was considered and rejected. It does not generalise: a payload may need CONVERTING
	// (a list<int8>, a list<uint16> and a list<int32> all read into an int32 payload, because
	// this library reports the narrowest LOSSLESS Fortran kind), so for most families there is no
	// pointer to hand over at all. Copying uniformly is what removes the whole use-after-free
	// class that the string path had to grow last_whole_column_buffers_array to close, and there
	// is nothing here to pin. The extra cost is one int64 offsets array of nrows+1 entries, which
	// is noise beside the values.
	//
	// The two entry points are NOT a matched pair sharing hidden state (contrast sort_key_info /
	// sort_key_fetch). Each is independently correct: the fill re-derives the shape and ABORTS if
	// it disagrees with the arguments it was given, so a caller passing a stale count is told,
	// rather than writing past the end of a buffer.

	// One row group, or the whole column, as a list array -- the single place the LIST read path
	// decides which. `row_group <= 0` means the whole column (get_single_chunk_array, so the row
	// transform applies and filtering/sampling/sorting compose for free); otherwise exactly that
	// one row group (get_row_group_chunk_array, whose own mask segment is already applied).
	static std::shared_ptr<arrow::Array> get_list_source_array(ParquetReaderHandle *reader_handle,
		const char *name, int64_t row_group, const char *context)
	{
		if (row_group <= 0) return get_single_chunk_array(reader_handle, name);
		return get_row_group_chunk_array(reader_handle, name, row_group, context);
	}

	// A list array's three shape facts, resolved in one place: how many rows, how many elements
	// those rows hold between them, and where the elements start in the child array.
	//
	// SLICING IS WHY THE REBASE EXISTS, and it is worth being precise about which offsets are
	// really in play, because the obvious guess is wrong. A list array has three:
	//
	//   * the list array's own data()->offset  -- aligns the ROW validity bitmap
	//   * raw_value_offsets()[0]               -- where this slice's elements start in the child
	//   * the CHILD array's data()->offset     -- aligns the ELEMENT validity bitmap
	//
	// Only the SECOND is handled explicitly here (`base`). The other two are handled by Arrow's
	// own accessors: array->IsValid(i) already accounts for the parent's offset, and slicing the
	// child gives it an offset that child->IsValid(k) accounts for in turn. So the code below
	// reads one offset, not three, and the other two cannot be dropped by an edit to this
	// function at all -- only by replacing an accessor with a raw buffer walk, which is the thing
	// not to do.
	//
	// THE REBASE IS DEFENSIVE ON TODAY'S READ PATHS, and that was measured rather than assumed.
	// arrow::compute::Filter and Take -- what a filtered, sampled or sorted read goes through
	// (apply_row_transform) -- return COMPACT arrays with all three offsets 0, not slices; and
	// nothing on the read side calls Slice() on a column array (the only such call in this file is
	// on the WRITER side). The one route by which a sliced list array could arrive is
	// unwrap_struct_path's StructArray::field() on an already-sliced struct, which
	// extract_string_buffers' own comment records as never yet observed. So no Fortran-side test
	// can reach a nonzero `base`, and a mutation setting it to 0 survives the whole suite --
	// which is a fact about what is reachable, not a coverage gap to be closed with an
	// unbuildable fixture (CLAUDE.md, "Coverage tooling never drives design").
	//
	// It was instead verified OUT OF PROCESS, against a genuinely sliced array carrying a null row
	// and a null element, by replicating this logic exactly: base=10, and all four rows' lengths,
	// values, row nullness and element nullness came back correct. Re-run that check rather than
	// trusting a green suite if this arithmetic is ever changed. See feature_risks.md Risk-154.
	//
	// `child_out` comes back already Sliced to exactly the elements these rows use, so every
	// consumer downstream (the convert_values_to_* family, the string byte copy, the element
	// validity walk) sees an ordinary array and needs no offset awareness of its own.
	struct ListShape
	{
		int64_t nrows = 0;
		int64_t nelems = 0;
		std::shared_ptr<arrow::Array> child; // sliced to exactly [0, nelems)
		std::vector<int64_t> offsets;        // nrows+1 entries, 0-based, offsets[0] == 0
	};

	// Fills a ListShape from `array`, which must be a LIST/LARGE_LIST/FIXED_SIZE_LIST array.
	// FIXED_SIZE_LIST is accepted deliberately: a vector column IS a list whose rows all happen to
	// have the same length, and refusing it would be a third rule about which representation is
	// allowed when -- the very thing the caller's-output-type-decides design exists to avoid.
	static ListShape describe_list_array(const std::shared_ptr<arrow::Array> &array,
		const std::string &name, const char *context)
	{
		ListShape shape;
		shape.nrows = array->length();
		shape.offsets.resize(static_cast<size_t>(shape.nrows) + 1, 0);
		// MAP joins LIST here rather than getting an arm of its own: arrow::MapArray DERIVES from
		// arrow::ListArray (arrow/array/array_nested.h), so the cast below is valid for one, its
		// value_offsets are the map's own, and `values()` is the ENTRIES struct array
		// (struct<key, value>) that map_entry_children splits. Sharing the arm is deliberate: this
		// is where the rebase arithmetic above lives, it was expensive to get right, and no
		// Fortran-side test can reach a nonzero `base` to catch a second copy drifting from it.
		if (array->type_id() == arrow::Type::LIST || array->type_id() == arrow::Type::MAP)
		{
			auto list_arr = std::static_pointer_cast<arrow::ListArray>(array);
			int64_t base = shape.nrows > 0 ? list_arr->value_offset(0) : 0;
			for (int64_t i = 0; i < shape.nrows; ++i)
			{
				shape.offsets[static_cast<size_t>(i) + 1] = list_arr->value_offset(i + 1) - base;
			}
			shape.nelems = shape.offsets[static_cast<size_t>(shape.nrows)];
			shape.child = list_arr->values()->Slice(base, shape.nelems);
			return shape;
		}
		if (array->type_id() == arrow::Type::LARGE_LIST)
		{
			auto list_arr = std::static_pointer_cast<arrow::LargeListArray>(array);
			int64_t base = shape.nrows > 0 ? list_arr->value_offset(0) : 0;
			for (int64_t i = 0; i < shape.nrows; ++i)
			{
				shape.offsets[static_cast<size_t>(i) + 1] = list_arr->value_offset(i + 1) - base;
			}
			shape.nelems = shape.offsets[static_cast<size_t>(shape.nrows)];
			shape.child = list_arr->values()->Slice(base, shape.nelems);
			return shape;
		}
		if (array->type_id() == arrow::Type::FIXED_SIZE_LIST)
		{
			auto list_arr = std::static_pointer_cast<arrow::FixedSizeListArray>(array);
			int64_t width = list_arr->value_length();
			for (int64_t i = 0; i < shape.nrows; ++i)
			{
				shape.offsets[static_cast<size_t>(i) + 1] = (i + 1) * width;
			}
			shape.nelems = shape.nrows * width;
			shape.child = list_arr->values()->Slice(list_arr->value_offset(0), shape.nelems);
			return shape;
		}
		report_fatal_error(context, std::string("type mismatch for column: ") + name +
			" (expected list/large_list/fixed_size_list, got " + array->type()->ToString() + ")"); // GCOVR_EXCL_LINE
	}

	// The value type a list column's elements read into -- its child field's type, taken from the
	// SCHEMA rather than from any decoded array, so a shape query never has to read data it does
	// not need. Returns nullptr when `field` is not a list-typed field at all.
	static std::shared_ptr<arrow::DataType> list_element_type(const std::shared_ptr<arrow::Field> &field)
	{
		auto id = field->type()->id();
		if (id != arrow::Type::LIST && id != arrow::Type::LARGE_LIST && id != arrow::Type::FIXED_SIZE_LIST)
		{
			return nullptr;
		}
		return field->type()->field(0)->type();
	}

	// Writes `array`'s per-ROW validity into `valid_out` (1 = present, 0 = a NULL list). A null
	// ROW and a present-but-EMPTY row are different things and both survive: an empty row is
	// valid here and simply has offsets[i] == offsets[i+1].
	static void write_list_row_validity(const std::shared_ptr<arrow::Array> &array, int64_t nrows, int8_t *valid_out)
	{
		if (!valid_out) return; // GCOVR_EXCL_LINE -- every caller passes a buffer; kept as a guard.
		for (int64_t i = 0; i < nrows; ++i)
		{
			valid_out[i] = array->IsValid(i) ? 1 : 0;
		}
	}

	// Writes the per-ELEMENT validity of an already-sliced child array into `valid_out`. This is
	// the SECOND null level -- an element that is Null inside a row that is itself present -- and
	// is stored separately from row nullness on the Fortran side too, so a read that collapsed
	// the two would still pass every assertion made about either one alone.
	static void write_list_element_validity(const std::shared_ptr<arrow::Array> &child, int64_t nelems, int8_t *valid_out)
	{
		if (!valid_out) return; // GCOVR_EXCL_LINE -- every caller passes a buffer; kept as a guard.
		for (int64_t i = 0; i < nelems; ++i)
		{
			valid_out[i] = child->IsValid(i) ? 1 : 0;
		}
	}

	// The shared body of every parquet_read_list_*_fill entry point: fetch the array, describe it,
	// check the caller's counts against what is really there, and write out the offsets and both
	// validity levels. Returns the child array the caller then converts into its typed buffer.
	//
	// The count check is the reason the two entry points do not have to be a matched pair: a
	// caller that allocated from a stale shape is told so here, before anything writes past the
	// end of its buffers.
	static std::shared_ptr<arrow::Array> fill_list_common(ParquetReaderHandle *reader_handle,
		const char *name, int64_t row_group, int64_t nrows, int64_t nelems,
		int64_t *offsets_out, int8_t *row_valid_out, int8_t *elem_valid_out, const char *context)
	{
		auto array = get_list_source_array(reader_handle, name, row_group, context);
		auto shape = describe_list_array(array, name, context);
		if (shape.nrows != nrows)
		{
			report_fatal_error(context, std::string("nrows mismatch for column: ") + name);
		}
		if (shape.nelems != nelems)
		{
			report_fatal_error(context, std::string("element count mismatch for column: ") + name +
				" (the column changed between the shape and fill calls)"); // GCOVR_EXCL_LINE
		}
		for (int64_t i = 0; i <= nrows; ++i)
		{
			offsets_out[i] = shape.offsets[static_cast<size_t>(i)];
		}
		write_list_row_validity(array, nrows, row_valid_out);
		write_list_element_validity(shape.child, nelems, elem_valid_out);
		return shape.child;
	}

	// The payload byte total of an already-sliced string child array. STRING/LARGE_STRING only:
	// both report it in O(1) from their own offsets, and STRING_VIEW -- which would need a scan --
	// never reaches here, because arrow_leaf_family deliberately does not claim it (see there).
	static int64_t string_child_total_bytes(const std::shared_ptr<arrow::Array> &child,
		const std::string &name, const char *context)
	{
		if (child->type_id() == arrow::Type::LARGE_STRING)
		{
			return std::static_pointer_cast<arrow::LargeStringArray>(child)->total_values_length();
		}
		if (child->type_id() == arrow::Type::STRING)
		{
			return std::static_pointer_cast<arrow::StringArray>(child)->total_values_length();
		}
		report_fatal_error(context, std::string("type mismatch for list values in column: ") + name +
			" (expected string, got " + child->type()->ToString() + ")"); // GCOVR_EXCL_LINE
	}

	// Records a completed whole-column list read against the reader's own bookkeeping, so
	// parquet_reader_check_complete and %print_stat both see the column as read. A chunked read
	// deliberately does not, matching every other _chunk entry point in this file.
	static void mark_list_read(ParquetReaderHandle *reader_handle, const char *name, int64_t row_group,
		int32_t family, const std::shared_ptr<arrow::Array> &array)
	{
		if (row_group > 0) return;
		mark_read(reader_handle, name, elem_family_token(family), array);
	}



// ==== MAP column read helpers (see the entry points further down) ====
//
// A map is physically a LIST of struct<key, value>, and Arrow models that literally --
// arrow::MapArray derives from arrow::ListArray and arrow::MapType from arrow::ListType. So the
// shape half of a map read is describe_list_array's job (it grew one type id, nothing else), and
// what is left is splitting the entries struct into its two children and answering what the
// SCHEMA says the key and value types are.

// The key and value types a map column's entries hold, taken from the SCHEMA rather than from any
// decoded array, so a shape query never has to read data it does not need. Both return nullptr
// when `field` is not a map-typed field at all.
static std::shared_ptr<arrow::DataType> map_key_type(const std::shared_ptr<arrow::Field> &field)
{
	if (field->type()->id() != arrow::Type::MAP) return nullptr;
	return std::static_pointer_cast<arrow::MapType>(field->type())->key_type();
}

static std::shared_ptr<arrow::DataType> map_value_type(const std::shared_ptr<arrow::Field> &field)
{
	if (field->type()->id() != arrow::Type::MAP) return nullptr;
	return std::static_pointer_cast<arrow::MapType>(field->type())->item_type();
}

// Splits a map's entries array into its keys and its values.
//
// `entries` is what describe_list_array put in ListShape::child, already Sliced to exactly the
// entries these rows use -- and StructArray::field() applies the struct's own offset and length to
// each child in turn, so both come back correctly sliced with no offset arithmetic here. That is
// the same property unwrap_struct_path relies on.
static void map_entry_children(const std::shared_ptr<arrow::Array> &entries,
	std::shared_ptr<arrow::Array> &keys_out, std::shared_ptr<arrow::Array> &values_out,
	const std::string &name, const char *context)
{
	if (entries->type_id() != arrow::Type::STRUCT)
	{ // GCOVR_EXCL_START -- Arrow guarantees a map's child is its entries struct.
		report_fatal_error(context, std::string("malformed map column: ") + name +
			" (its entries are " + entries->type()->ToString() + ", not a key/value struct)");
	}
	// GCOVR_EXCL_STOP
	auto sa = std::static_pointer_cast<arrow::StructArray>(entries);
	if (sa->num_fields() != 2)
	{ // GCOVR_EXCL_START -- likewise: a MapType always has exactly two child fields.
		report_fatal_error(context, std::string("malformed map column: ") + name +
			" (its entries struct has " + std::to_string(sa->num_fields()) + " fields, not 2)");
	}
	// GCOVR_EXCL_STOP
	keys_out = sa->field(0);
	values_out = sa->field(1);
}

// The shape half every map fill shares: fetches the column (or one row group of it), checks it
// really is a map, checks the counts against what the caller was told by
// parquet_read_map_column_shape, writes the offsets and the per-ROW validity, and hands back the
// two entry children.
//
// Each fill calls this independently rather than trusting that a matching shape call has just
// happened -- the same rule fill_list_common states, and the reason a caller may interleave the
// keys fill and a value fill in either order.
static void fill_map_common(ParquetReaderHandle *reader_handle, const char *name, int64_t row_group,
	int64_t nrows, int64_t nentries, int64_t *offsets_out, int8_t *row_valid_out,
	std::shared_ptr<arrow::Array> &keys_out, std::shared_ptr<arrow::Array> &values_out,
	const char *context)
{
	auto array = get_list_source_array(reader_handle, name, row_group, context);
	if (array->type_id() != arrow::Type::MAP)
	{
		report_fatal_error(context, std::string("type mismatch for column: ") + name +
			" (expected a map column, got " + array->type()->ToString() + ")"); // GCOVR_EXCL_LINE
	}
	auto shape = describe_list_array(array, name, context);
	if (shape.nrows != nrows)
	{
		report_fatal_error(context, std::string("nrows mismatch for column: ") + name);
	}
	if (shape.nelems != nentries)
	{
		report_fatal_error(context, std::string("entry count mismatch for column: ") + name +
			" (the column changed between the shape and fill calls)"); // GCOVR_EXCL_LINE
	}
	if (offsets_out)
	{
		for (int64_t i = 0; i <= nrows; ++i)
		{
			offsets_out[i] = shape.offsets[static_cast<size_t>(i)];
		}
	}
	if (row_valid_out) write_list_row_validity(array, nrows, row_valid_out);
	map_entry_children(shape.child, keys_out, values_out, name, context);
}

extern "C"
{

	// ==== Chunked (row-group-scoped) column reads ====
	//
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
		int8_t *offsets_int32_out, int64_t *validity_offset_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_row_group_chunk_array(reader_handle, name, row_group, "parquet_read_column_chunk");
		// Same conversion as the whole-column compact read above, but NOT cached: a row-group
		// chunk is not what column_cache holds, so there is nothing to replace -- the pin below
		// is what keeps the freshly cast array alive across the return to Fortran.
		if (array->type_id() == arrow::Type::STRING_VIEW)
		{
			ensure_compute_initialized();
			auto coerced = coerce_string_view_to_offset_string(array);
			if (!coerced.ok())
			{ // GCOVR_EXCL_START -- Cast-kernel Status backstop on an already-decoded array.
				report_fatal_error("parquet_read_column_chunk",
					std::string("failed to convert a string_view column for reading: ") + name);
			}
			// GCOVR_EXCL_STOP
			array = coerced.ValueOrDie();
		}
		if (!is_offset_string_type(array->type_id()))
		{
			report_fatal_error("parquet_read_column_chunk", std::string("type mismatch for column: ") + name +
				" (expected string, got " + array->type()->ToString() + ")"); // GCOVR_EXCL_LINE
		}
		reader_handle->last_chunk_buffers_array = array;
		extract_string_buffers(array, nrows_out, nchars_out, offsets_out, data_out, validity_out, offsets_int32_out,
			validity_offset_out);
	}


	// ==== Variable-length LIST column reads (see the section banner above) ====
	//
	// The FIRST of the two crossings: reports everything Fortran needs in order to allocate its
	// buffers and %init a parquet_list_column to the right payload kind, and writes no data.
	//
	//   nrows_out         rows in this column (or in this row group)
	//   nelems_out        elements those rows hold between them
	//   nchars_out        payload bytes, for a string element family only; 0 otherwise
	//   elem_family_out   the element family (see arrow_leaf_family / the PF_ELEM_* parameters)
	//   unit_out          the temporal unit selector, for a timestamp element family only
	//
	// The family and the unit are resolved from the SCHEMA, so a zero-row column still reports
	// the kind its payload would have had; the two counts need the array. `row_group <= 0` means
	// the whole column. Aborts if `name` is not a list column at all, or if its element type is
	// one this library cannot read -- the second of those is a clean refusal in place of a
	// half-built column, since there is no "unknown" payload kind for a list to hold.
	void parquet_read_list_column_shape(void *handle, const char *name, int64_t row_group,
		int64_t *nrows_out, int64_t *nelems_out, int64_t *nchars_out,
		int32_t *elem_family_out, int32_t *unit_out)
	{
		auto reader_handle = as_reader_handle(handle);
		const char *context = row_group > 0 ? "parquet_read_column_chunk" : "parquet_read_column";
		auto resolved = resolve_struct_path(reader_handle->schema, name);
		auto elem_type = list_element_type(resolved.leaf_field);
		if (!elem_type)
		{
			report_fatal_error(context, std::string("type mismatch for column: ") + name +
				" (expected a list column, got " + resolved.leaf_field->type()->ToString() + ")"); // GCOVR_EXCL_LINE
		}
		int32_t family = arrow_leaf_family(elem_type);
		if (family == kElemFamilyNone)
		{
			report_fatal_error(context, std::string("unsupported list element type for column: ") + name +
				" (" + elem_type->ToString() + ")"); // GCOVR_EXCL_LINE
		}
		*elem_family_out = family;
		*unit_out = 0;
		if (family == kElemFamilyTimestamp)
		{
			*unit_out = arrow_unit_to_temporal_selector(
				std::static_pointer_cast<arrow::TimestampType>(elem_type)->unit());
		}
		auto array = get_list_source_array(reader_handle, name, row_group, context);
		auto shape = describe_list_array(array, name, context);
		*nrows_out = shape.nrows;
		*nelems_out = shape.nelems;
		*nchars_out = 0;
		if (family == kElemFamilyString)
		{
			*nchars_out = string_child_total_bytes(shape.child, name, context);
		}
	}

	// The SECOND crossing, one entry point per element family. Each copies this column's (or this
	// row group's) offsets, both null levels and its values into the caller's own buffers -- see
	// fill_list_common for the shape check that makes each independently safe rather than
	// dependent on a matching shape call having just happened.
	void parquet_read_list_int32_fill(void *handle, const char *name, int64_t row_group,
		int64_t nrows, int64_t nelems, int64_t *offsets, int8_t *row_valid,
		int32_t *values, int8_t *elem_valid)
	{
		auto reader_handle = as_reader_handle(handle);
		const char *context = row_group > 0 ? "parquet_read_column_chunk" : "parquet_read_column";
		auto child = fill_list_common(reader_handle, name, row_group, nrows, nelems, offsets,
			row_valid, elem_valid, context);
		convert_values_to_int32(child, values, nelems, name, context);
		fill_null_default(values, elem_valid, nelems);
		mark_list_read(reader_handle, name, row_group, kElemFamilyInt32, child);
	}

	// Same as parquet_read_list_int32_fill, but for an int64 payload.
	void parquet_read_list_int64_fill(void *handle, const char *name, int64_t row_group,
		int64_t nrows, int64_t nelems, int64_t *offsets, int8_t *row_valid,
		int64_t *values, int8_t *elem_valid)
	{
		auto reader_handle = as_reader_handle(handle);
		const char *context = row_group > 0 ? "parquet_read_column_chunk" : "parquet_read_column";
		auto child = fill_list_common(reader_handle, name, row_group, nrows, nelems, offsets,
			row_valid, elem_valid, context);
		convert_values_to_int64(child, values, nelems, name, context);
		fill_null_default(values, elem_valid, nelems);
		mark_list_read(reader_handle, name, row_group, kElemFamilyInt64, child);
	}

	// Same as parquet_read_list_int32_fill, but for a float32 payload.
	void parquet_read_list_float32_fill(void *handle, const char *name, int64_t row_group,
		int64_t nrows, int64_t nelems, int64_t *offsets, int8_t *row_valid,
		float *values, int8_t *elem_valid)
	{
		auto reader_handle = as_reader_handle(handle);
		const char *context = row_group > 0 ? "parquet_read_column_chunk" : "parquet_read_column";
		auto child = fill_list_common(reader_handle, name, row_group, nrows, nelems, offsets,
			row_valid, elem_valid, context);
		convert_values_to_float32(child, values, nelems, name, context);
		fill_null_default(values, elem_valid, nelems);
		mark_list_read(reader_handle, name, row_group, kElemFamilyFloat32, child);
	}

	// Same as parquet_read_list_int32_fill, but for a float64 payload.
	void parquet_read_list_float64_fill(void *handle, const char *name, int64_t row_group,
		int64_t nrows, int64_t nelems, int64_t *offsets, int8_t *row_valid,
		double *values, int8_t *elem_valid)
	{
		auto reader_handle = as_reader_handle(handle);
		const char *context = row_group > 0 ? "parquet_read_column_chunk" : "parquet_read_column";
		auto child = fill_list_common(reader_handle, name, row_group, nrows, nelems, offsets,
			row_valid, elem_valid, context);
		convert_values_to_float64(child, values, nelems, name, context);
		fill_null_default(values, elem_valid, nelems);
		mark_list_read(reader_handle, name, row_group, kElemFamilyFloat64, child);
	}

	// Same as parquet_read_list_int32_fill, but for a boolean payload (one int8 per element).
	void parquet_read_list_bool8_fill(void *handle, const char *name, int64_t row_group,
		int64_t nrows, int64_t nelems, int64_t *offsets, int8_t *row_valid,
		int8_t *values, int8_t *elem_valid)
	{
		auto reader_handle = as_reader_handle(handle);
		const char *context = row_group > 0 ? "parquet_read_column_chunk" : "parquet_read_column";
		auto child = fill_list_common(reader_handle, name, row_group, nrows, nelems, offsets,
			row_valid, elem_valid, context);
		if (child->type_id() != arrow::Type::BOOL)
		{
			report_fatal_error(context, std::string("type mismatch for list values in column: ") + name +
				" (expected bool, got " + child->type()->ToString() + ")"); // GCOVR_EXCL_LINE
		}
		auto arr = std::static_pointer_cast<arrow::BooleanArray>(child);
		for (int64_t i = 0; i < nelems; ++i)
		{
			values[i] = arr->Value(i) ? 1 : 0;
		}
		fill_null_default(values, elem_valid, nelems);
		mark_list_read(reader_handle, name, row_group, kElemFamilyBool, child);
	}

	// Same as parquet_read_list_int32_fill, but for a date payload (int32 days since the epoch).
	void parquet_read_list_date_fill(void *handle, const char *name, int64_t row_group,
		int64_t nrows, int64_t nelems, int64_t *offsets, int8_t *row_valid,
		int32_t *values, int8_t *elem_valid)
	{
		auto reader_handle = as_reader_handle(handle);
		const char *context = row_group > 0 ? "parquet_read_column_chunk" : "parquet_read_column";
		auto child = fill_list_common(reader_handle, name, row_group, nrows, nelems, offsets,
			row_valid, elem_valid, context);
		convert_date_values(child, values, nelems, name, context);
		fill_null_default(values, elem_valid, nelems);
		mark_list_read(reader_handle, name, row_group, kElemFamilyDate, child);
	}

	// Same as parquet_read_list_int32_fill, but for a time payload (canonical int64 ns-of-day).
	void parquet_read_list_time_fill(void *handle, const char *name, int64_t row_group,
		int64_t nrows, int64_t nelems, int64_t *offsets, int8_t *row_valid,
		int64_t *values, int8_t *elem_valid)
	{
		auto reader_handle = as_reader_handle(handle);
		const char *context = row_group > 0 ? "parquet_read_column_chunk" : "parquet_read_column";
		auto child = fill_list_common(reader_handle, name, row_group, nrows, nelems, offsets,
			row_valid, elem_valid, context);
		convert_time_values(child, values, nelems, name, context);
		fill_null_default(values, elem_valid, nelems);
		mark_list_read(reader_handle, name, row_group, kElemFamilyTime, child);
	}

	// Same as parquet_read_list_int32_fill, but for a timestamp payload (int64 in the column's own
	// unit -- the unit itself comes from parquet_read_list_column_shape's unit_out).
	void parquet_read_list_timestamp_fill(void *handle, const char *name, int64_t row_group,
		int64_t nrows, int64_t nelems, int64_t *offsets, int8_t *row_valid,
		int64_t *values, int8_t *elem_valid)
	{
		auto reader_handle = as_reader_handle(handle);
		const char *context = row_group > 0 ? "parquet_read_column_chunk" : "parquet_read_column";
		auto child = fill_list_common(reader_handle, name, row_group, nrows, nelems, offsets,
			row_valid, elem_valid, context);
		convert_timestamp_values(child, values, nelems, name, context);
		fill_null_default(values, elem_valid, nelems);
		mark_list_read(reader_handle, name, row_group, kElemFamilyTimestamp, child);
	}

	// Same as parquet_read_list_int32_fill, but for a string payload, which needs its own
	// offsets-and-bytes pair rather than one value per element: `str_offsets` gets nelems+1
	// entries starting at 0, and `str_data` gets nchars payload bytes.
	//
	// This one COPIES rather than handing back Arrow's own buffers, unlike
	// parquet_read_string_column_buffers -- deliberately, so that this whole path keeps the one
	// property that makes it safe: nothing Arrow-owned crosses the boundary, so there is nothing
	// to pin and no lifetime to reason about. The extra pass over the payload bytes is the price,
	// and it is one memcpy next to the copy the destination parquet_string_column does anyway.
	void parquet_read_list_string_fill(void *handle, const char *name, int64_t row_group,
		int64_t nrows, int64_t nelems, int64_t nchars, int64_t *offsets, int8_t *row_valid,
		int64_t *str_offsets, char *str_data, int8_t *elem_valid)
	{
		auto reader_handle = as_reader_handle(handle);
		const char *context = row_group > 0 ? "parquet_read_column_chunk" : "parquet_read_column";
		auto child = fill_list_common(reader_handle, name, row_group, nrows, nelems, offsets,
			row_valid, elem_valid, context);
		if (string_child_total_bytes(child, name, context) != nchars)
		{
			report_fatal_error(context, std::string("payload byte count mismatch for column: ") + name +
				" (the column changed between the shape and fill calls)"); // GCOVR_EXCL_LINE
		}
		auto acc = make_string_like_accessor(child);
		int64_t at = 0;
		str_offsets[0] = 0;
		for (int64_t i = 0; i < nelems; ++i)
		{
			auto view = acc.get_view(i);
			if (!view.empty())
			{
				std::memcpy(str_data + at, view.data(), view.size());
			}
			at += static_cast<int64_t>(view.size());
			str_offsets[i + 1] = at;
		}
		mark_list_read(reader_handle, name, row_group, kElemFamilyString, child);
	}


	// ==== STRUCT column reads ====
	//
	// A struct column needs FAR less new plumbing than a list column did, and the reason is worth
	// stating because it is the whole shape of this section. Every field of a struct is already an
	// ordinary column at an ordinary DOTTED PATH -- `person.age` -- which resolve_struct_path has
	// resolved and every per-kind reader has read since long before container columns existed,
	// whole-column and per row group alike. So the field VALUES need no new entry point at all:
	// src/parquet_read_struct.f90 composes `<col>.<field>` and calls the existing
	// parquet_read_column / parquet_read_column_chunk machinery once per field.
	//
	// What it cannot get that way is exactly two things, and they are the two functions below:
	// the struct's FIELD SET (names, element families, temporal units), and the struct's OWN
	// per-row validity.
	//
	// THE SECOND IS NOT DERIVABLE and that is the subtle half. unwrap_struct_path hands a field
	// read its COMBINED mask -- `struct_valid AND field_valid` -- so a row where every field is
	// null is indistinguishable from a row where the struct instance itself is absent, and those
	// are different rows. Only the struct array's own validity bitmap separates them.
	//
	// THE FIELD'S OWN NULLNESS, ON THE OTHER HAND, *IS* THE COMBINED MASK, with no arithmetic --
	// which is the opposite of what the obvious derivation suggests, so it is stated here to stop
	// someone adding the derivation back. For a PRESENT struct row the combination is the
	// identity. For a NULL struct row Parquet has already forced every child null on write: its
	// definition levels cannot encode "the struct is absent but its field is present". Measured
	// against Arrow 25.0.0 -- a struct array built in memory with row 4 null and its `id` child
	// VALID at row 4 reads back with that child INVALID (feature_container_phase4.md's F5). So
	// `combined` is exactly what the file stores for the field, at every row.

	// The FIRST of the two crossings for the field set: reports the counts Fortran needs in order
	// to allocate, and writes no data.
	//
	//   nrows_out        rows in this column (or in this row group)
	//   nfields_out      declared fields of the struct
	//   name_width_out   longest field name, so Fortran can allocate character(len=W) :: names(nf)
	//
	// Everything but nrows comes from the SCHEMA, so a zero-row column still reports its full
	// field set -- which is what lets a reader %init a struct column that has no rows.
	void parquet_read_struct_column_shape(void *handle, const char *name, int64_t row_group,
		int64_t *nrows_out, int32_t *nfields_out, int32_t *name_width_out)
	{
		auto reader_handle = as_reader_handle(handle);
		const char *context = row_group > 0 ? "parquet_read_column_chunk" : "parquet_read_column";
		auto resolved = resolve_struct_path(reader_handle->schema, name);
		if (resolved.leaf_field->type()->id() != arrow::Type::STRUCT)
		{
			report_fatal_error(context, std::string("type mismatch for column: ") + name +
				" (expected a struct column, got " + resolved.leaf_field->type()->ToString() + ")"); // GCOVR_EXCL_LINE
		}
		auto struct_type = std::static_pointer_cast<arrow::StructType>(resolved.leaf_field->type());
		*nfields_out = struct_type->num_fields();
		int32_t width = 1;
		for (int i = 0; i < struct_type->num_fields(); ++i)
		{
			auto len = static_cast<int32_t>(struct_type->field(i)->name().size());
			if (len > width) width = len;
		}
		*name_width_out = width;
		// The field set above is schema-only; the ROW COUNT is not, and must not be, because a
		// filtered, sampled or sorted reader answers about the rows that survived rather than
		// about the file. Taking it from the array is what makes this agree with what
		// parquet_read_struct_row_validity and every per-field read will go on to see. Same rule
		// -- and the same helper -- as parquet_read_list_column_shape. row_group <= 0 means the
		// whole column.
		auto array = get_list_source_array(reader_handle, name, row_group, context);
		*nrows_out = array->length();
	}

	// The SECOND crossing for the field set: the declared field names, blank-padded into one
	// `nfields * name_width` block, plus each field's element family and temporal unit/utc flag.
	//
	// A field whose own type is STRUCT, LIST or MAP reports kElemFamilyNone rather than aborting
	// here, so that src/parquet_read_struct.f90 can name the offending FIELD in its message
	// instead of this function naming only the column. Nesting is Phase 7; refusing it cleanly,
	// with the field named, is Phase 4's whole obligation towards it.
	void parquet_read_struct_column_fields(void *handle, const char *name, int32_t nfields,
		int32_t name_width, char *names_out, int32_t *families_out, int32_t *units_out, int8_t *utc_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto resolved = resolve_struct_path(reader_handle->schema, name);
		auto struct_type = std::static_pointer_cast<arrow::StructType>(resolved.leaf_field->type());
		if (struct_type->num_fields() != nfields)
		{ // GCOVR_EXCL_START -- Fortran passes back what the shape call just reported.
			report_fatal_error("parquet_read_column", std::string("field count changed between calls for column: ") + name);
		}
		// GCOVR_EXCL_STOP
		for (int i = 0; i < nfields; ++i)
		{
			auto field = struct_type->field(i);
			copy_string_with_padding(names_out + static_cast<int64_t>(i) * name_width, name_width, field->name());
			families_out[i] = arrow_leaf_family(field->type());
			units_out[i] = 0;
			utc_out[i] = 0;
			if (families_out[i] == kElemFamilyTimestamp)
			{
				auto ts = std::static_pointer_cast<arrow::TimestampType>(field->type());
				units_out[i] = arrow_unit_to_temporal_selector(ts->unit());
				utc_out[i] = ts->timezone().empty() ? 0 : 1;
			}
			else if (families_out[i] == kElemFamilyTime)
			{
				units_out[i] = field->type()->id() == arrow::Type::TIME32
					? arrow_unit_to_temporal_selector(std::static_pointer_cast<arrow::Time32Type>(field->type())->unit())
					: arrow_unit_to_temporal_selector(std::static_pointer_cast<arrow::Time64Type>(field->type())->unit());
			}
		}
	}

	// The struct's OWN per-row validity: 1 where the struct instance is present, 0 where it is
	// absent. See this section's banner for why nothing else can answer this.
	//
	// Reads the STRUCT array rather than any field, so it goes through the same source-array
	// helper the list reads use and inherits filtering, sampling and sorting unchanged: a row
	// transform only ever removes or reorders rows, and this walks whatever rows survived.
	void parquet_read_struct_row_validity(void *handle, const char *name, int64_t row_group,
		int64_t nrows, int8_t *row_valid)
	{
		auto reader_handle = as_reader_handle(handle);
		const char *context = row_group > 0 ? "parquet_read_column_chunk" : "parquet_read_column";
		auto array = get_list_source_array(reader_handle, name, row_group, context);
		if (array->type_id() != arrow::Type::STRUCT)
		{
			report_fatal_error(context, std::string("type mismatch for column: ") + name +
				" (expected a struct column, got " + array->type()->ToString() + ")"); // GCOVR_EXCL_LINE
		}
		if (array->length() != nrows)
		{
			report_fatal_error(context, std::string("row count mismatch for column: ") + name + // GCOVR_EXCL_LINE
				" (file has " + std::to_string(array->length()) + " rows, caller expects " + // GCOVR_EXCL_LINE
				std::to_string(nrows) + ")"); // GCOVR_EXCL_LINE
		}
		for (int64_t i = 0; i < nrows; ++i)
		{
			row_valid[i] = array->IsValid(i) ? 1 : 0;
		}
	}

	// ==== MAP column reads ====
	//
	// Eleven entry points in the same two-crossing shape the LIST reads use: one SHAPE call that
	// reports the counts and the value family and writes no data, then one KEYS fill and one VALUE
	// fill. Each fill re-derives the shape itself (fill_map_common), so the three may be issued in
	// any order and none depends on another having just run.
	//
	// The keys get their own call rather than being folded into each of the nine value fills. They
	// are always strings, so folding would repeat four key-buffer arguments across nine signatures
	// that are otherwise identical to their list counterparts -- and it is the SAMENESS with the
	// list fills that makes the two reviewable side by side.
	//
	// V1 KEYS ARE STRINGS, and a map keyed by anything else is refused by the shape call rather
	// than coerced. Rendering an int32 key as text would silently change the data (1, 01 and 1.0
	// are different keys) and would make a round trip through this library lossy with nothing to
	// report it. There is no "unknown" key kind for a map to hold, so a clean refusal naming the
	// actual key type is the only truthful answer -- the same one an unsupported LIST element type
	// gets.

	// Writes `name`'s map VALUE type token into `buf` (space-padded to buf_len) and returns 1, if
	// `name` is a map column this library can actually read; otherwise writes "unknown" and
	// returns 0. Schema-only: reads no column data at all.
	//
	// This exists because parquet_reader_get_column_type_name deliberately does NOT unwrap a map --
	// it answers "unknown" for one, which is the right answer for a query whose contract is "what
	// element type would I declare?", since a map cell is not one value. But parquet_open_table's
	// classification pass has to decide, WITHOUT READING, whether a map column is readable at all,
	// and the two things that make one unreadable both live in the schema: a non-string key, and a
	// value type outside the nine families. Both are refused by parquet_read_map_column_shape
	// below -- with report_fatal_error, on the read. Classifying such a column as supported would
	// therefore turn one exotic map into an abort on first touch, which is precisely what
	// table_classify's types= probe exists to prevent for every other unreadable type.
	//
	// "unknown" for a non-map is deliberate rather than an error, matching
	// parquet_reader_get_column_type_name's own contract: a caller that needs to tell "not a map"
	// from "a map I cannot read" asks parquet_reader_get_column_shape_name, and the pairing of the
	// two is what the Fortran doc-comment documents.
	int64_t parquet_reader_get_map_value_type_name(void *handle, const char *name, char *buf, int64_t buf_len)
	{
		auto reader_handle = as_reader_handle(handle);
		auto resolved = resolve_struct_path(reader_handle->schema, name);
		auto key_type = map_key_type(resolved.leaf_field);
		auto value_type = map_value_type(resolved.leaf_field);
		int32_t family = kElemFamilyNone;
		// Both null exactly when the field is not a map at all; the key test is what rejects the
		// int-keyed map that map_payloads.parquet carries for this purpose.
		if (key_type && value_type && is_string_like_type(key_type->id()))
		{
			family = arrow_leaf_family(value_type);
		}
		if (family == kElemFamilyNone)
		{
			copy_string_with_padding(buf, buf_len, std::string("unknown"));
			return 0;
		}
		copy_string_with_padding(buf, buf_len, std::string(elem_family_token(family)));
		return 1;
	}

	// The FIRST crossing: everything Fortran needs in order to allocate its buffers and %init a
	// parquet_map_column to the right value kind. Writes no data.
	//
	//   nrows_out         rows in this column (or in this row group)
	//   nentries_out      key/value pairs those rows hold between them
	//   nkeychars_out     total key bytes (keys are always strings, so this is always meaningful)
	//   value_family_out  the value family (see arrow_leaf_family / the PF_ELEM_* parameters)
	//   unit_out          the temporal unit selector, for a timestamp value family only
	//   nvalchars_out     total value bytes, for a string value family only; 0 otherwise
	//
	// The families and the unit are resolved from the SCHEMA, so a zero-row column still reports
	// the kind its values would have had; the counts need the array. `row_group <= 0` means the
	// whole column.
	void parquet_read_map_column_shape(void *handle, const char *name, int64_t row_group,
		int64_t *nrows_out, int64_t *nentries_out, int64_t *nkeychars_out,
		int32_t *value_family_out, int32_t *unit_out, int64_t *nvalchars_out)
	{
		auto reader_handle = as_reader_handle(handle);
		const char *context = row_group > 0 ? "parquet_read_column_chunk" : "parquet_read_column";
		auto resolved = resolve_struct_path(reader_handle->schema, name);
		auto key_type = map_key_type(resolved.leaf_field);
		if (!key_type)
		{
			report_fatal_error(context, std::string("type mismatch for column: ") + name +
				" (expected a map column, got " + resolved.leaf_field->type()->ToString() + ")"); // GCOVR_EXCL_LINE
		}
		if (!is_string_like_type(key_type->id()))
		{
			report_fatal_error(context, std::string("unsupported map key type for column: ") + name +
				" (" + key_type->ToString() + "); only string keys are supported");
		}
		auto value_type = map_value_type(resolved.leaf_field);
		int32_t family = arrow_leaf_family(value_type);
		if (family == kElemFamilyNone)
		{
			report_fatal_error(context, std::string("unsupported map value type for column: ") + name +
				" (" + value_type->ToString() + ")");
		}
		*value_family_out = family;
		*unit_out = 0;
		if (family == kElemFamilyTimestamp)
		{
			*unit_out = arrow_unit_to_temporal_selector(
				std::static_pointer_cast<arrow::TimestampType>(value_type)->unit());
		}
		std::shared_ptr<arrow::Array> keys, values;
		auto array = get_list_source_array(reader_handle, name, row_group, context);
		auto shape = describe_list_array(array, name, context);
		map_entry_children(shape.child, keys, values, name, context);
		*nrows_out = shape.nrows;
		*nentries_out = shape.nelems;
		*nkeychars_out = string_child_total_bytes(keys, name, context);
		*nvalchars_out = 0;
		if (family == kElemFamilyString)
		{
			*nvalchars_out = string_child_total_bytes(values, name, context);
		}
	}

	// The SECOND crossing, part one: the offsets, the per-ROW validity and the KEYS.
	//
	// Copies the key bytes rather than handing back Arrow's own buffers, exactly as
	// parquet_read_list_string_fill does and for the same reason: nothing Arrow-owned crosses the
	// boundary, so there is nothing to pin and no lifetime to reason about.
	//
	// There is no key VALIDITY argument, and that is not an omission -- Arrow's MapType declares
	// its key field non-nullable and offers no way to change it, so a map has exactly two null
	// levels (the row, and each value) and a key is never one of them.
	void parquet_read_map_keys_fill(void *handle, const char *name, int64_t row_group,
		int64_t nrows, int64_t nentries, int64_t nkeychars, int64_t *offsets, int8_t *row_valid,
		int64_t *key_offsets, char *key_data)
	{
		auto reader_handle = as_reader_handle(handle);
		const char *context = row_group > 0 ? "parquet_read_column_chunk" : "parquet_read_column";
		std::shared_ptr<arrow::Array> keys, values;
		fill_map_common(reader_handle, name, row_group, nrows, nentries, offsets, row_valid,
			keys, values, context);
		if (string_child_total_bytes(keys, name, context) != nkeychars)
		{
			report_fatal_error(context, std::string("key byte count mismatch for column: ") + name +
				" (the column changed between the shape and fill calls)"); // GCOVR_EXCL_LINE
		}
		auto acc = make_string_like_accessor(keys);
		int64_t at = 0;
		key_offsets[0] = 0;
		for (int64_t i = 0; i < nentries; ++i)
		{
			auto view = acc.get_view(i);
			if (!view.empty())
			{
				std::memcpy(key_data + at, view.data(), view.size());
			}
			at += static_cast<int64_t>(view.size());
			key_offsets[i + 1] = at;
		}
	}

	// The SECOND crossing, part two: one entry point per VALUE family. Each copies this column's
	// (or this row group's) values and their validity into the caller's own buffers.
	void parquet_read_map_int32_fill(void *handle, const char *name, int64_t row_group,
		int64_t nrows, int64_t nentries, int32_t *values_out, int8_t *value_valid)
	{
		auto reader_handle = as_reader_handle(handle);
		const char *context = row_group > 0 ? "parquet_read_column_chunk" : "parquet_read_column";
		std::shared_ptr<arrow::Array> keys, values;
		fill_map_common(reader_handle, name, row_group, nrows, nentries, nullptr, nullptr,
			keys, values, context);
		write_list_element_validity(values, nentries, value_valid);
		convert_values_to_int32(values, values_out, nentries, name, context);
		fill_null_default(values_out, value_valid, nentries);
		mark_list_read(reader_handle, name, row_group, kElemFamilyInt32, values);
	}

	// Same as parquet_read_map_int32_fill, but for int64 values.
	void parquet_read_map_int64_fill(void *handle, const char *name, int64_t row_group,
		int64_t nrows, int64_t nentries, int64_t *values_out, int8_t *value_valid)
	{
		auto reader_handle = as_reader_handle(handle);
		const char *context = row_group > 0 ? "parquet_read_column_chunk" : "parquet_read_column";
		std::shared_ptr<arrow::Array> keys, values;
		fill_map_common(reader_handle, name, row_group, nrows, nentries, nullptr, nullptr,
			keys, values, context);
		write_list_element_validity(values, nentries, value_valid);
		convert_values_to_int64(values, values_out, nentries, name, context);
		fill_null_default(values_out, value_valid, nentries);
		mark_list_read(reader_handle, name, row_group, kElemFamilyInt64, values);
	}

	// Same as parquet_read_map_int32_fill, but for float32 values.
	void parquet_read_map_float32_fill(void *handle, const char *name, int64_t row_group,
		int64_t nrows, int64_t nentries, float *values_out, int8_t *value_valid)
	{
		auto reader_handle = as_reader_handle(handle);
		const char *context = row_group > 0 ? "parquet_read_column_chunk" : "parquet_read_column";
		std::shared_ptr<arrow::Array> keys, values;
		fill_map_common(reader_handle, name, row_group, nrows, nentries, nullptr, nullptr,
			keys, values, context);
		write_list_element_validity(values, nentries, value_valid);
		convert_values_to_float32(values, values_out, nentries, name, context);
		fill_null_default(values_out, value_valid, nentries);
		mark_list_read(reader_handle, name, row_group, kElemFamilyFloat32, values);
	}

	// Same as parquet_read_map_int32_fill, but for float64 values.
	void parquet_read_map_float64_fill(void *handle, const char *name, int64_t row_group,
		int64_t nrows, int64_t nentries, double *values_out, int8_t *value_valid)
	{
		auto reader_handle = as_reader_handle(handle);
		const char *context = row_group > 0 ? "parquet_read_column_chunk" : "parquet_read_column";
		std::shared_ptr<arrow::Array> keys, values;
		fill_map_common(reader_handle, name, row_group, nrows, nentries, nullptr, nullptr,
			keys, values, context);
		write_list_element_validity(values, nentries, value_valid);
		convert_values_to_float64(values, values_out, nentries, name, context);
		fill_null_default(values_out, value_valid, nentries);
		mark_list_read(reader_handle, name, row_group, kElemFamilyFloat64, values);
	}

	// Same as parquet_read_map_int32_fill, but for boolean values (one int8 per entry).
	void parquet_read_map_bool8_fill(void *handle, const char *name, int64_t row_group,
		int64_t nrows, int64_t nentries, int8_t *values_out, int8_t *value_valid)
	{
		auto reader_handle = as_reader_handle(handle);
		const char *context = row_group > 0 ? "parquet_read_column_chunk" : "parquet_read_column";
		std::shared_ptr<arrow::Array> keys, values;
		fill_map_common(reader_handle, name, row_group, nrows, nentries, nullptr, nullptr,
			keys, values, context);
		write_list_element_validity(values, nentries, value_valid);
		if (values->type_id() != arrow::Type::BOOL)
		{
			report_fatal_error(context, std::string("type mismatch for map values in column: ") + name +
				" (expected bool, got " + values->type()->ToString() + ")"); // GCOVR_EXCL_LINE
		}
		auto arr = std::static_pointer_cast<arrow::BooleanArray>(values);
		for (int64_t i = 0; i < nentries; ++i)
		{
			values_out[i] = arr->Value(i) ? 1 : 0;
		}
		fill_null_default(values_out, value_valid, nentries);
		mark_list_read(reader_handle, name, row_group, kElemFamilyBool, values);
	}

	// Same as parquet_read_map_int32_fill, but for date values (int32 days since the epoch).
	void parquet_read_map_date_fill(void *handle, const char *name, int64_t row_group,
		int64_t nrows, int64_t nentries, int32_t *values_out, int8_t *value_valid)
	{
		auto reader_handle = as_reader_handle(handle);
		const char *context = row_group > 0 ? "parquet_read_column_chunk" : "parquet_read_column";
		std::shared_ptr<arrow::Array> keys, values;
		fill_map_common(reader_handle, name, row_group, nrows, nentries, nullptr, nullptr,
			keys, values, context);
		write_list_element_validity(values, nentries, value_valid);
		convert_date_values(values, values_out, nentries, name, context);
		fill_null_default(values_out, value_valid, nentries);
		mark_list_read(reader_handle, name, row_group, kElemFamilyDate, values);
	}

	// Same as parquet_read_map_int32_fill, but for time values (canonical int64 ns-of-day).
	void parquet_read_map_time_fill(void *handle, const char *name, int64_t row_group,
		int64_t nrows, int64_t nentries, int64_t *values_out, int8_t *value_valid)
	{
		auto reader_handle = as_reader_handle(handle);
		const char *context = row_group > 0 ? "parquet_read_column_chunk" : "parquet_read_column";
		std::shared_ptr<arrow::Array> keys, values;
		fill_map_common(reader_handle, name, row_group, nrows, nentries, nullptr, nullptr,
			keys, values, context);
		write_list_element_validity(values, nentries, value_valid);
		convert_time_values(values, values_out, nentries, name, context);
		fill_null_default(values_out, value_valid, nentries);
		mark_list_read(reader_handle, name, row_group, kElemFamilyTime, values);
	}

	// Same as parquet_read_map_int32_fill, but for timestamp values (int64 in the column's own
	// unit -- the unit itself comes from parquet_read_map_column_shape's unit_out).
	void parquet_read_map_timestamp_fill(void *handle, const char *name, int64_t row_group,
		int64_t nrows, int64_t nentries, int64_t *values_out, int8_t *value_valid)
	{
		auto reader_handle = as_reader_handle(handle);
		const char *context = row_group > 0 ? "parquet_read_column_chunk" : "parquet_read_column";
		std::shared_ptr<arrow::Array> keys, values;
		fill_map_common(reader_handle, name, row_group, nrows, nentries, nullptr, nullptr,
			keys, values, context);
		write_list_element_validity(values, nentries, value_valid);
		convert_timestamp_values(values, values_out, nentries, name, context);
		fill_null_default(values_out, value_valid, nentries);
		mark_list_read(reader_handle, name, row_group, kElemFamilyTimestamp, values);
	}

	// Same as parquet_read_map_int32_fill, but for string values, which need their own
	// offsets-and-bytes pair rather than one value per entry: `val_offsets` gets nentries+1 entries
	// starting at 0, and `val_data` gets nvalchars payload bytes. Copies, for the same reason the
	// keys fill does.
	void parquet_read_map_string_fill(void *handle, const char *name, int64_t row_group,
		int64_t nrows, int64_t nentries, int64_t nvalchars, int64_t *val_offsets, char *val_data,
		int8_t *value_valid)
	{
		auto reader_handle = as_reader_handle(handle);
		const char *context = row_group > 0 ? "parquet_read_column_chunk" : "parquet_read_column";
		std::shared_ptr<arrow::Array> keys, values;
		fill_map_common(reader_handle, name, row_group, nrows, nentries, nullptr, nullptr,
			keys, values, context);
		write_list_element_validity(values, nentries, value_valid);
		if (string_child_total_bytes(values, name, context) != nvalchars)
		{
			report_fatal_error(context, std::string("value byte count mismatch for column: ") + name +
				" (the column changed between the shape and fill calls)"); // GCOVR_EXCL_LINE
		}
		auto acc = make_string_like_accessor(values);
		int64_t at = 0;
		val_offsets[0] = 0;
		for (int64_t i = 0; i < nentries; ++i)
		{
			auto view = acc.get_view(i);
			if (!view.empty())
			{
				std::memcpy(val_data + at, view.data(), view.size());
			}
			at += static_cast<int64_t>(view.size());
			val_offsets[i + 1] = at;
		}
		mark_list_read(reader_handle, name, row_group, kElemFamilyString, values);
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
			emit_warning_cpp(msg);
		}
	}

	// ==== Schema-less writer: column/table metadata declaration ====
	//
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

	// Updates an already-declared column's col_size/array_size in place -- called by the Fortran
	// write path the first time a MAML col_size: auto/array_size: auto placeholder is resolved
	// from an actual write call's own data shape (see parquet_resolve_or_check_col_size/
	// parquet_resolve_or_check_array_size, parquet_write.f90), so every later consumer of
	// column_metadata (estimate_chunk_size_from_schema, build_file_metadata's VOTable/KV
	// column.*.col_size/array_size text) sees the resolved value rather than the placeholder.
	// A no-op if `name` isn't found -- should never happen for a schema-enforced writer's own
	// declared column.
	void parquet_update_column_metadata_size(void *handle, const char *name, int64_t col_size, int64_t array_size)
	{
		auto writer_handle = as_handle(handle);
		for (auto &col : writer_handle->column_metadata)
		{
			if (col.name == name)
			{
				col.col_size = col_size;
				col.array_size = array_size;
				return;
			}
		}
	}

	// Adds one flat key-value table metadata entry to `handle`. `datatype` is "" for a value
	// that is a string, and a type token ("int32", "float64[]", ...) for one a typed
	// add_metadata overload produced; see TableMetadataEntry::datatype.
	void parquet_add_table_metadata(void *handle, const char *key, const char *value, const char *description,
									const char *datatype)
	{
		auto writer_handle = as_handle(handle);
		writer_handle->table_metadata.push_back(TableMetadataEntry{
			std::string(key),
			std::string(value),
			std::string(description),
			std::string(datatype)});
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
	const int8_t *valid_in, const std::shared_ptr<arrow::DataType> &value_type, bool always_nullable = false)
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

	// Presence, not values: nullable iff this column's FIRST chunk carried an is_valid mask.
	// Checked on every chunk, not only the first, because Rule 2 (every row group must use the
	// same masked/unmasked form) is what stops a later row group's Null meeting a field that
	// cannot hold one. See resolve_chunk_nullability.
	bool nullable = resolve_chunk_nullability(writer_handle, name, idx, first_chunk_ever, valid_in != nullptr,
		always_nullable);
	if (first_chunk_ever)
	{
		if (writer_handle->fields.size() <= idx) writer_handle->fields.resize(idx + 1);
		writer_handle->fields[idx] = build_field(name, value_type, col_size, nullable);
	}
	if (writer_handle->arrays.size() <= idx) writer_handle->arrays.resize(idx + 1);
	writer_handle->pending_chunk_arrays[static_cast<int>(idx)] = align_array_to_field(writer_handle->fields[idx], array);
}

// ==== Variable-length LIST column writes ====
//
// The path a Fortran parquet_list_column takes into a Parquet file, and the mirror image of the
// "Variable-length LIST column reads" section further above. It sits BESIDE the vector
// (FIXED_SIZE_LIST) write rather than replacing it: a 2-D array still writes a fixed-width vector
// column, and the caller's chosen VALUE TYPE is what picks which physical shape the file gets.
//
// ONE CROSSING PER COLUMN, AND NOTHING ARROW-OWNED CROSSES IT. A read needs two crossings because
// the reader cannot know the counts before Arrow has been asked; a write needs none of that --
// Fortran already knows nrows, the element count, the payload kind and the temporal unit, so it
// hands all of them over at once, in buffers it owns and which stay alive for the duration of the
// call. Everything below COPIES out of those buffers into Arrow's own, exactly as every other
// append entry point in this file does.
//
// TWO NULLABILITY LEVELS, NOT ONE. A list column can hold a null ROW (an absent list) and a null
// ELEMENT inside a present row, and the two are independent -- so the Arrow field has a
// `nullable` flag at the outer list level AND on its child, where every other column type here
// has one flag. build_field cannot express that (its col_size > 1 form puts the caller's
// nullability on the CHILD and forces the outer field non-nullable, which is right for a vector
// column and wrong at both levels for a list), so this section has its own build_list_field.
//
// The whole-column path decides both flags FROM THE VALUES, having seen all of them. The streamed
// path cannot -- a parquet_list_column carries its null state inside itself, with no
// caller-supplied mask whose presence could stand in for "might this column contain a Null?" --
// so a streamed list column is in the ALWAYS-NULLABLE class alongside temporal columns and
// parquet_string_column, and is nullable at both levels unless the column is protected. See
// resolve_chunk_nullability.
//
// The SAFETY INVARIANT build_field's own comment states -- a field declared non-nullable must
// never receive an array containing nulls -- holds at both levels by construction: the
// whole-column path derives each flag from the very buffer it is about to build the array from,
// and the streamed path only declares non-nullable under protection, which parquet_write_list.f90
// enforces before calling in (at BOTH levels, so a protected list column may hold neither a null
// row nor a null element).

// Arrow's `list<T>` addresses its child with an int32 offsets buffer, exactly as `utf8` addresses
// its bytes; `large_list<T>` is the int64 form, exactly as `large_utf8` is. So a list column's
// element count is bounded by the same 2^31-1 as a string column's byte count, and the answer is
// the same one parquet_append_string_column already gives: use the NARROW type unless the data
// cannot fit in it, which produces the conventional Arrow type for essentially every real file
// while keeping the int64 escape hatch for the one that does not.
//
// The choice is invisible in the Parquet file itself -- measured: a `list` and a `large_list`
// carrying the same data produce byte-identical schemas, the same leaf path `<col>.list.element`,
// the same max repetition and definition levels. It shows up only in the ARROW:schema metadata
// blob, i.e. in what pyarrow reports and what tools ignoring that blob never see.
//
// A STREAMED list column is always the narrow form and provably so: a row group's element count
// is already capped at kArrowInt32ListElementCountLimit by check_list_chunk_elements_fit_arrow_limit
// below, and that limit is this same 2^31-1. Only a whole-column write can ever produce large_list.
static int64_t effective_list_offset_limit()
{
	return g_debug_list_offset_limit > 0 ? g_debug_list_offset_limit : kArrowInt32OffsetLimit;
}

// The longest row of a list column described by `nrows` and its nrows+1 offsets.
static int64_t list_max_row_length(int64_t nrows, const int64_t *offsets)
{
	int64_t longest = 0;
	for (int64_t i = 0; i < nrows; ++i)
	{
		int64_t len = offsets[i + 1] - offsets[i];
		if (len > longest) longest = len;
	}
	return longest;
}

// Aborts if a SINGLE row of a list column holds more elements than Parquet's own
// repetition/definition-level generation can address (see kArrowInt32ListElementCountLimit). This
// is the one case no row-group size can rescue -- a row cannot be split across row groups -- so it
// is checked where the array is built rather than at close, and it is the list column's
// counterpart to check_col_size_fits_arrow_limit for a vector column.
static void check_list_row_length_fits_arrow_limit(int64_t max_row_len, const std::string &name, const char *context)
{
	int64_t limit = g_debug_list_element_count_limit > 0 ? g_debug_list_element_count_limit : kArrowInt32ListElementCountLimit;
	if (max_row_len > limit)
	{
		report_fatal_error(context, "column '" + name + "': one row holds " + std::to_string(max_row_len) + // GCOVR_EXCL_LINE
			" elements, exceeding " + std::to_string(kArrowInt32ListElementCountLimit) + // GCOVR_EXCL_LINE
			", the maximum per-row-group element count Arrow/Parquet's list-column level generation " // GCOVR_EXCL_LINE
			"supports -- no row-group size can accommodate this, since a row is never split across " // GCOVR_EXCL_LINE
			"row groups"); // GCOVR_EXCL_LINE
	}
}

// Aborts if ONE STREAMED ROW GROUP's element count exceeds the same limit. The caller chose this
// row group's row count through parquet_new_row_group, so -- exactly like an explicit chunk_size
// on the batch path -- it is validated rather than silently overridden, and the message names the
// thing to make smaller. The whole-column path has no equivalent call site: there the row-group
// boundaries are not known until close, where close_parquet_writer checks them instead.
static void check_list_chunk_elements_fit_arrow_limit(int64_t nelems, const std::string &name, const char *context)
{
	int64_t limit = g_debug_list_element_count_limit > 0 ? g_debug_list_element_count_limit : kArrowInt32ListElementCountLimit;
	if (nelems > limit)
	{
		report_fatal_error(context, "column '" + name + "': this row group holds " + std::to_string(nelems) + // GCOVR_EXCL_LINE
			" elements, exceeding " + std::to_string(kArrowInt32ListElementCountLimit) + // GCOVR_EXCL_LINE
			", the maximum per-row-group element count Arrow/Parquet's list-column level generation " // GCOVR_EXCL_LINE
			"supports -- pass a smaller nrows to parquet_new_row_group"); // GCOVR_EXCL_LINE
	}
}

// The field a list column is written with. Deliberately NOT build_field: see this section's
// banner for why a list needs two independent nullability flags where every other column here
// needs one.
//
// The child field is named "item", which is what arrow::list()/arrow::large_list()'s own
// DataType-taking constructors supply. That name is invisible in the written file -- measured:
// Arrow normalises every list child to `element` in the Parquet schema's leaf paths, whatever the
// Arrow field is called, for fixed_size_list and list alike -- so the choice is free, and matching
// build_field is the only reason to prefer one spelling. What the name DOES govern is
// arrow::DataType::Equals, which compares the child's name and nullability as well as its type:
// the array assembled below is stamped with THIS type, so the two agree by construction and
// align_array_to_field is the no-op it is designed to be.
static std::shared_ptr<arrow::Field> build_list_field(const std::string &name,
	const std::shared_ptr<arrow::DataType> &elem_type, bool row_nullable, bool elem_nullable, bool large)
{
	auto item = arrow::field("item", elem_type, elem_nullable);
	auto type = large ? arrow::large_list(item) : arrow::list(item);
	return arrow::field(name, type, row_nullable);
}

// Assembles the list array itself from Fortran's offsets, its per-ROW validity and an already-built
// child array. `list_type` is the field's own type, so the offsets buffer's width follows the field
// rather than being recomputed -- which is what keeps a STREAMED column's later row groups
// consistent with the field its first row group locked in.
//
// A null ROW whose offsets still span elements is left exactly as Fortran handed it over.
// parquet_list_column's %set_null leaves a nulled row's elements physically present and
// unreachable, and rebuilding the column to remove them here would be an O(nelems) pass to buy
// back memory the caller can already reclaim with %gather_rows. Measured: Arrow accepts such an
// array (ValidateFull and Table::Validate both pass), writes it, and reads it back as a null row
// with the unreachable elements dropped from the child -- Parquet emits a definition level for the
// null row and never visits its values.
static std::shared_ptr<arrow::Array> assemble_list_array(const std::shared_ptr<arrow::DataType> &list_type,
	int64_t nrows, int64_t nelems, const int64_t *offsets, const int8_t *row_valid,
	const std::shared_ptr<arrow::Array> &child, const std::string &name, const char *context)
{
	bool large = list_type->id() == arrow::Type::LARGE_LIST;
	if (!large && nelems > kArrowInt32OffsetLimit)
	{ // GCOVR_EXCL_START -- unreachable safety net: the whole-column path picks large_list above
	  // this many elements, and the streamed path cannot reach it at all (a row group is capped at
	  // the same 2^31-1 by check_list_chunk_elements_fit_arrow_limit).
		report_fatal_error(context, "column '" + name + "': " + std::to_string(nelems) +
			" elements do not fit an int32 list offsets buffer");
	}
	// GCOVR_EXCL_STOP

	std::shared_ptr<arrow::Buffer> offsets_buf;
	if (large)
	{
		offsets_buf = arrow::AllocateBuffer((nrows + 1) * static_cast<int64_t>(sizeof(int64_t))).ValueOrDie();
		std::memcpy(offsets_buf->mutable_data(), offsets, static_cast<size_t>(nrows + 1) * sizeof(int64_t));
	}
	else
	{
		offsets_buf = arrow::AllocateBuffer((nrows + 1) * static_cast<int64_t>(sizeof(int32_t))).ValueOrDie();
		auto p = reinterpret_cast<int32_t *>(offsets_buf->mutable_data());
		for (int64_t i = 0; i <= nrows; ++i) p[i] = static_cast<int32_t>(offsets[i]);
	}

	std::shared_ptr<arrow::Buffer> null_bitmap;
	int64_t null_count = 0;
	if (row_valid != nullptr)
	{
		for (int64_t i = 0; i < nrows; ++i)
		{
			if (row_valid[i] == 0) ++null_count;
		}
		if (null_count > 0)
		{
			null_bitmap = arrow::AllocateEmptyBitmap(nrows).ValueOrDie();
			for (int64_t i = 0; i < nrows; ++i)
			{
				if (row_valid[i] != 0) arrow::bit_util::SetBit(null_bitmap->mutable_data(), i);
			}
		}
	}

	auto data = arrow::ArrayData::Make(list_type, nrows, {null_bitmap, offsets_buf}, {child->data()}, null_count);
	return arrow::MakeArray(data);
}

// Builds the CHILD array of a list column from Fortran's flat value buffer and its per-ELEMENT
// validity, for every family whose Arrow builder takes plain AppendValues (int32, int64, float32,
// float64, bool8, date). Time, timestamp and string each need their own construction and go
// through build_time_array/build_timestamp_array/build_list_string_child instead.
//
// Declared outside extern "C" for the same reason append_typed_column is: a function template
// cannot have C language linkage.
template <typename BuilderType, typename ValueType>
static std::shared_ptr<arrow::Array> build_list_child_array(const ValueType *values, int64_t nelems,
	const int8_t *elem_valid)
{
	BuilderType builder;
	auto status = builder.AppendValues(values, nelems, reinterpret_cast<const uint8_t *>(elem_valid));
	if (!status.ok())
		throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE
	std::shared_ptr<arrow::Array> array;
	status = builder.Finish(&array);
	if (!status.ok())
		throw std::runtime_error(status.ToString()); // GCOVR_EXCL_LINE
	return array;
}

// The string family's child array, built from the same packed offsets+bytes layout a
// parquet_string_column stores natively (nelems+1 int64 offsets over `data`'s nchars bytes).
// Picks arrow::utf8() or arrow::large_utf8() by the same rule parquet_append_string_column uses
// for a whole string column -- narrow unless the byte payload cannot fit it.
static std::shared_ptr<arrow::Array> build_list_string_child(const int64_t *str_offsets, const char *data,
	int64_t nelems, int64_t nchars, const int8_t *elem_valid)
{
	int64_t limit = g_debug_string_offset_limit > 0 ? g_debug_string_offset_limit : kArrowInt32OffsetLimit;
	bool use_large = nchars > limit;

	auto append_all = [&](auto &builder) -> std::shared_ptr<arrow::Array>
	{
		auto status = arrow::Status::OK();
		for (int64_t i = 0; i < nelems; ++i)
		{
			if (elem_valid != nullptr && elem_valid[i] == 0)
			{
				status = builder.AppendNull();
			}
			else
			{
				auto lo = str_offsets[i];
				status = builder.Append(data + lo, static_cast<int32_t>(str_offsets[i + 1] - lo));
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

	if (use_large)
	{
		arrow::LargeStringBuilder builder;
		return append_all(builder);
	}
	arrow::StringBuilder builder;
	return append_all(builder);
}

// The shared tail of every whole-column list append: check the one ceiling a row-group size cannot
// rescue, decide both nullability flags from the values, build the field and the array, and store
// them. `child` has already been built by the caller's own family-specific step.
static void append_list_column_common(void *handle, const char *name, int64_t nrows, int64_t nelems,
	const int64_t *offsets, const int8_t *row_valid, const int8_t *elem_valid,
	const std::shared_ptr<arrow::Array> &child, const char *context)
{
	auto writer_handle = as_handle(handle);
	check_list_row_length_fits_arrow_limit(list_max_row_length(nrows, offsets), name, context);
	bool large = nelems > effective_list_offset_limit();
	auto field = build_list_field(name, child->type(), has_any_null(row_valid, nrows),
		has_any_null(elem_valid, nelems), large);
	auto array = assemble_list_array(field->type(), nrows, nelems, offsets, row_valid, child, name, context);
	append_column(writer_handle, name, field, array);
}

// Streaming counterpart to append_list_column_common: same array, stashed into
// pending_chunk_arrays instead of arrays, with the field built once on the column's first chunk.
//
// Both nullability flags come from ONE resolve_chunk_nullability call with always_nullable set,
// because for a list column the two answers are the same answer: nullable at both levels unless
// the column is protected, in which case neither level may hold a Null and both are non-nullable.
// There is no first-chunk consistency rule to enforce, because the answer does not depend on the
// chunk -- which is exactly what always_nullable means.
static void append_list_column_chunk_common(void *handle, const char *name, int64_t nrows, int64_t nelems,
	const int64_t *offsets, const int8_t *row_valid, const std::shared_ptr<arrow::Array> &child,
	const char *context)
{
	auto writer_handle = as_handle(handle);
	bool first_chunk_ever;
	size_t idx = check_column_chunk_write_preconditions(writer_handle, name, first_chunk_ever);
	check_list_row_length_fits_arrow_limit(list_max_row_length(nrows, offsets), name, context);
	check_list_chunk_elements_fit_arrow_limit(nelems, name, context);

	bool nullable = resolve_chunk_nullability(writer_handle, name, idx, first_chunk_ever,
		/*mask_present=*/true, /*always_nullable=*/true);
	if (first_chunk_ever)
	{
		if (writer_handle->fields.size() <= idx) writer_handle->fields.resize(idx + 1);
		writer_handle->fields[idx] = build_list_field(name, child->type(), nullable, nullable,
			nelems > effective_list_offset_limit());
	}
	if (writer_handle->arrays.size() <= idx) writer_handle->arrays.resize(idx + 1);
	auto array = assemble_list_array(writer_handle->fields[idx]->type(), nrows, nelems, offsets, row_valid,
		child, name, context);
	writer_handle->pending_chunk_arrays[static_cast<int>(idx)] =
		align_array_to_field(writer_handle->fields[idx], array);
}

// ==== STRUCT column writes (staged: begin, push one field at a time, finish) ====
//
// A struct column with M fields of arbitrary kinds cannot cross a fixed bind(C) signature in one
// call, so a struct write is STAGED on the writer handle: parquet_struct_begin opens the staging,
// one parquet_struct_field_<kind> call per field builds that field's child array and pushes it,
// and parquet_append_struct_column (or its _chunk twin) assembles the StructArray, builds the
// field and stores it. Eleven entry points, of which the nine pushes are shared between the
// whole-column and the streamed path -- which is why this is eleven where the list write needed
// eighteen.
//
// EVERY ONE OF THE NINE PUSHES REUSES THE LIST WRITE'S OWN CHILD BUILDERS. A struct field's child
// array is exactly a list column's child array -- the same flat Fortran buffer plus the same int8
// validity buffer -- so build_list_child_array / build_list_string_child / build_time_array /
// build_timestamp_array serve both, and a second copy of any of them would be a second place for
// a null convention to drift.
//
// NULLABILITY HAS 1 + M LEVELS: the struct row's own, plus one per field. build_field decides one
// flag and build_list_field two, so a struct gets its own build_struct_field for the same reason
// the list write did. The rule per path is the list write's rule exactly:
//
//   * a WHOLE-COLUMN write decides every flag FROM THE VALUES, having seen all of them;
//   * a STREAMED write is in the ALWAYS-NULLABLE class -- a parquet_struct_column carries its
//     null state inside itself, so there is no caller-supplied mask whose presence could stand in
//     for "might this column contain a Null?", and the first row group locks the schema long
//     before the later ones exist;
//   * a PROTECTED column is non-nullable at EVERY level, which is the only way to declare a
//     streamed struct column null-free. src/parquet_write_struct.f90 enforces that before calling
//     in, at both levels, so a protected struct column may hold neither a null row nor a null
//     field value.
//
// THERE IS NO REPETITION-LEVEL CEILING HERE, and that was measured rather than assumed: a written
// struct<id:int32, nm:string> has leaf paths `s.id`/`s.nm` with max_repetition_level 0 and
// max_definition_level 2, so apache/arrow#33188 -- which cost the list write three guards and a
// debug hook -- cannot bite for a non-nested struct. **Phase 7 reopens this**: the moment a struct
// field is a list or a map, maxrep becomes 1 and the list write's guard is back in play, reached
// THROUGH the struct. See feature_container_phase4.md's F7 and D9.

// The field a struct column is written with. Deliberately NOT build_field: see this section's
// banner for why a struct needs 1 + M independent nullability flags where every other column here
// needs one.
//
// The child fields take their types from the already-built child arrays, so the type this stamps
// and the type the StructArray carries agree by construction and align_array_to_field is the
// no-op it is designed to be -- which matters more here than for a list, because
// arrow::DataType::Equals compares every child's name and nullability as well as its type, and a
// struct has M chances to disagree rather than one.
static std::shared_ptr<arrow::Field> build_struct_field(const std::string &name,
	const std::vector<std::string> &field_names,
	const std::vector<std::shared_ptr<arrow::Array>> &children,
	bool row_nullable, const std::vector<bool> &field_nullable)
{
	std::vector<std::shared_ptr<arrow::Field>> fields;
	fields.reserve(children.size());
	for (size_t i = 0; i < children.size(); ++i)
	{
		fields.push_back(arrow::field(field_names[i], children[i]->type(), field_nullable[i]));
	}
	return arrow::field(name, arrow::struct_(fields), row_nullable);
}

// Assembles the struct array itself from the staged children and Fortran's per-ROW validity.
//
// `struct_type` is the field's own type, so the child fields' declared nullability follows the
// FIELD rather than being recomputed -- which is what keeps a STREAMED column's later row groups
// consistent with the field its first row group locked in, exactly as assemble_list_array does
// with the offsets buffer's width.
//
// A null ROW whose fields still hold values is left exactly as Fortran handed it over.
// parquet_struct_column's %set_null leaves a nulled row's field values physically present and
// unreachable, and rebuilding to clear them here would be an O(nrows * nfields) pass to buy back
// nothing: Parquet emits a definition level for the null row and never visits its values, which
// is also why such values do not survive a round trip.
static std::shared_ptr<arrow::Array> assemble_struct_array(const std::shared_ptr<arrow::DataType> &struct_type,
	int64_t nrows, const int8_t *row_valid,
	const std::vector<std::shared_ptr<arrow::Array>> &children, const std::string &name, const char *context)
{
	for (size_t i = 0; i < children.size(); ++i)
	{
		if (children[i]->length() != nrows)
		{
			report_fatal_error(context, "column '" + name + "': field " + std::to_string(i + 1) + // GCOVR_EXCL_LINE
				" holds " + std::to_string(children[i]->length()) + " values but the struct has " + // GCOVR_EXCL_LINE
				std::to_string(nrows) + " rows"); // GCOVR_EXCL_LINE
		}
	}
	std::shared_ptr<arrow::Buffer> null_bitmap;
	int64_t null_count = 0;
	if (row_valid != nullptr)
	{
		for (int64_t i = 0; i < nrows; ++i)
		{
			if (row_valid[i] == 0) ++null_count;
		}
	}
	if (null_count > 0)
	{
		auto alloc = arrow::AllocateEmptyBitmap(nrows);
		if (!alloc.ok())
		{ // GCOVR_EXCL_START -- real allocation-failure backstop, not fixture-triggerable
			report_fatal_error(context, "column '" + name + "': failed to allocate a struct row validity bitmap");
		}
		// GCOVR_EXCL_STOP
		null_bitmap = alloc.ValueOrDie();
		for (int64_t i = 0; i < nrows; ++i)
		{
			arrow::bit_util::SetBitTo(null_bitmap->mutable_data(), i, row_valid[i] != 0);
		}
	}
	std::vector<std::shared_ptr<arrow::ArrayData>> child_data;
	child_data.reserve(children.size());
	for (const auto &c : children) child_data.push_back(c->data());
	auto data = arrow::ArrayData::Make(struct_type, nrows, {null_bitmap}, child_data, null_count);
	return arrow::MakeArray(data);
}

// The guard every finisher shares: staging is open, it is for THIS column, and it received
// exactly the fields it was promised.
//
// The field-count check is what turns a forgotten push into a message naming the column, instead
// of an Arrow schema mismatch at close time naming a field index -- and a MISSING push is the
// failure mode a staged protocol makes easy, so it is checked rather than trusted.
//
// **EVERY CHECK HERE IS DEFENSIVE AND UNREACHABLE FROM FORTRAN, and that was established rather
// than assumed.** src/parquet_write_struct.f90 is the only caller: it opens staging, loops over
// every declared field, and finishes, with no path that returns in between -- the two error stops
// inside that loop abort the process rather than leaving staging open. A test cannot drive the
// boundary directly either, because `parquet_writer%handle` is a PRIVATE component, so there is
// no way to obtain the writer handle these functions take. A mutation removing the field-count
// check therefore survives the whole suite; that is a fact about what is reachable, not a
// coverage gap to be closed with a contrived hook. See feature_risks.md Risk-156 and CLAUDE.md,
// "If a mutation cannot be caught by any fixture this repository can build, the branch is
// defensive".
static void check_struct_staging(ParquetWriterHandle *writer_handle, const char *name, int64_t nrows,
	const char *context)
{
	if (writer_handle->struct_staging_name.empty())
	{
		report_fatal_error(context, std::string("column '") + name + // GCOVR_EXCL_LINE
			"': no struct column staging is open -- parquet_struct_begin must be called first"); // GCOVR_EXCL_LINE
	}
	if (writer_handle->struct_staging_name != name)
	{
		report_fatal_error(context, std::string("column '") + name + // GCOVR_EXCL_LINE
			"': struct staging is open for a different column ('" + // GCOVR_EXCL_LINE
			writer_handle->struct_staging_name + "')"); // GCOVR_EXCL_LINE
	}
	if (static_cast<int32_t>(writer_handle->struct_staging_children.size()) != writer_handle->struct_staging_nfields)
	{
		report_fatal_error(context, std::string("column '") + name + "': " + // GCOVR_EXCL_LINE
			std::to_string(writer_handle->struct_staging_children.size()) + " field(s) were pushed but " + // GCOVR_EXCL_LINE
			std::to_string(writer_handle->struct_staging_nfields) + " were declared"); // GCOVR_EXCL_LINE
	}
	if (writer_handle->struct_staging_nrows != nrows)
	{
		report_fatal_error(context, std::string("column '") + name + "': the finisher was given " + // GCOVR_EXCL_LINE
			std::to_string(nrows) + " rows but staging was opened for " + // GCOVR_EXCL_LINE
			std::to_string(writer_handle->struct_staging_nrows)); // GCOVR_EXCL_LINE
	}
}

// Pushes one already-built child array into the open staging, checking that staging is open at
// all and that this push does not exceed the declared field count.
static void push_struct_child(void *handle, const char *field_name, const std::shared_ptr<arrow::Array> &child)
{
	auto writer_handle = as_handle(handle);
	if (writer_handle->struct_staging_name.empty())
	{
		report_fatal_error("parquet_write_column", std::string("field '") + field_name + // GCOVR_EXCL_LINE
			"': no struct column staging is open -- parquet_struct_begin must be called first"); // GCOVR_EXCL_LINE
	}
	if (static_cast<int32_t>(writer_handle->struct_staging_children.size()) >= writer_handle->struct_staging_nfields)
	{
		report_fatal_error("parquet_write_column", std::string("column '") + // GCOVR_EXCL_LINE
			writer_handle->struct_staging_name + "': more fields pushed than the " + // GCOVR_EXCL_LINE
			std::to_string(writer_handle->struct_staging_nfields) + " declared"); // GCOVR_EXCL_LINE
	}
	writer_handle->struct_staging_field_names.push_back(std::string(field_name));
	writer_handle->struct_staging_children.push_back(child);
}

// Drops whatever is staged, so that a finished (or refused) write cannot leak into the next one.
static void clear_struct_staging(ParquetWriterHandle *writer_handle)
{
	writer_handle->struct_staging_name.clear();
	writer_handle->struct_staging_nrows = 0;
	writer_handle->struct_staging_nfields = 0;
	writer_handle->struct_staging_field_names.clear();
	writer_handle->struct_staging_children.clear();
}

// ==== MAP column write helpers (see the entry points inside the extern "C" block below) ====
// The int32 offsets ceiling for a map column, with the test-only override applied.
// See g_debug_map_offset_limit for why a map has no `large` escape hatch to widen into.
static int64_t effective_map_offset_limit()
{
	return g_debug_map_offset_limit > 0 ? g_debug_map_offset_limit : kArrowInt32OffsetLimit;
}

// Refuses a map column whose entries do not fit an int32 offsets buffer, BEFORE the narrowing
// cast in assemble_map_array rather than after it -- a silent wrap there would produce a file
// whose offsets are garbage and whose reader sees plausible, wrong rows.
//
// Unlike the list and string equivalents this is a dead end rather than a fork: there is no
// large_map to switch to, so the only truthful outcomes are "it fits" and "this cannot be
// written". See CLAUDE.md's "Guarding a hard Arrow int32-only ceiling".
static void check_map_entries_fit_arrow_limit(int64_t nentries, const std::string &name, const char *context)
{
	int64_t limit = effective_map_offset_limit();
	if (nentries <= limit) return;
	report_fatal_error(context, "column '" + name + "': " + std::to_string(nentries) +
		" map entries exceed " + std::to_string(kArrowInt32OffsetLimit) +
		", the maximum an int32 map offsets buffer can address; Arrow has no large_map to widen"
		" into, so this column cannot be written (split it across more row groups)");
}

// The map field: `map<key_type, value_type>`, with the ROW and the VALUE nullability decided by
// the caller and the KEY always non-nullable -- arrow::MapType constructs its own key field that
// way and offers no way to change it, which is why parquet_map_column has only two null levels.
//
// The entries struct and its two children are named by MapType itself ("entries"/"key"/"value").
// Those names never reach the written file -- measured: Arrow normalises a map's Parquet leaf
// paths to `<col>.key_value.key` and `<col>.key_value.value` whatever the Arrow fields are
// called, exactly as it normalises a list's child to `element`. What they DO govern is
// arrow::DataType::Equals, and the array assembled below is stamped with THIS type, so the two
// agree by construction and align_array_to_field is the no-op it is designed to be.
static std::shared_ptr<arrow::Field> build_map_field(const std::string &name,
	const std::shared_ptr<arrow::DataType> &key_type,
	const std::shared_ptr<arrow::DataType> &value_type,
	bool row_nullable, bool value_nullable)
{
	auto item = arrow::field("value", value_type, value_nullable);
	auto type = std::make_shared<arrow::MapType>(key_type, item);
	return arrow::field(name, type, row_nullable);
}

// Assembles the map array from Fortran's offsets, its per-ROW validity and the already-staged
// key and value children. Structurally identical to assemble_list_array -- a MapArray's
// ArrayData is {validity, offsets} plus one child -- with the child being the entries struct
// rather than the values directly.
//
// The entries struct is built with NO validity of its own: a map entry is always present. Its
// key may not be null (MapType forbids it) and its value's nullness lives in the value child.
static std::shared_ptr<arrow::Array> assemble_map_array(const std::shared_ptr<arrow::DataType> &map_type,
	int64_t nrows, int64_t nentries, const int64_t *offsets, const int8_t *row_valid,
	const std::vector<std::shared_ptr<arrow::Array>> &children, const std::string &name,
	const char *context)
{
	check_map_entries_fit_arrow_limit(nentries, name, context);
	auto entries_type = std::static_pointer_cast<arrow::MapType>(map_type)->value_type();
	auto entries = assemble_struct_array(entries_type, nentries, nullptr, children, name, context);

	// Declared as a shared_ptr FIRST rather than with `auto`: AllocateBuffer yields a
	// unique_ptr, which cannot enter ArrayData::Make's braced buffer list. Same shape as
	// assemble_list_array.
	std::shared_ptr<arrow::Buffer> offsets_buf;
	offsets_buf = arrow::AllocateBuffer((nrows + 1) * static_cast<int64_t>(sizeof(int32_t))).ValueOrDie();
	auto op = reinterpret_cast<int32_t *>(offsets_buf->mutable_data());
	for (int64_t i = 0; i <= nrows; ++i) op[i] = static_cast<int32_t>(offsets[i]);

	std::shared_ptr<arrow::Buffer> null_bitmap;
	int64_t null_count = 0;
	if (row_valid != nullptr)
	{
		for (int64_t i = 0; i < nrows; ++i)
		{
			if (row_valid[i] == 0) ++null_count;
		}
		if (null_count > 0)
		{
			null_bitmap = arrow::AllocateEmptyBitmap(nrows).ValueOrDie();
			for (int64_t i = 0; i < nrows; ++i)
			{
				if (row_valid[i] != 0) arrow::bit_util::SetBit(null_bitmap->mutable_data(), i);
			}
		}
	}

	auto data = arrow::ArrayData::Make(map_type, nrows, {null_bitmap, offsets_buf}, {entries->data()}, null_count);
	return arrow::MakeArray(data);
}

// Shared body of the two finishers: validates the staging, builds the field with the caller's
// two nullability answers, assembles the array and clears the staging whatever happens.
static std::shared_ptr<arrow::Array> finish_map_staging(ParquetWriterHandle *writer_handle,
	const char *name, int64_t nrows, int64_t nentries, const int64_t *offsets, const int8_t *row_valid,
	bool row_nullable, bool value_nullable, std::shared_ptr<arrow::Field> &field_out,
	const char *context)
{
	check_struct_staging(writer_handle, name, nentries, context);
	auto &children = writer_handle->struct_staging_children;
	field_out = build_map_field(name, children[0]->type(), children[1]->type(),
		row_nullable, value_nullable);
	auto array = assemble_map_array(field_out->type(), nrows, nentries, offsets, row_valid,
		children, name, context);
	clear_struct_staging(writer_handle);
	return array;
}


extern "C"
{

	// Opens struct-column staging for `name`, discarding nothing: a second begin without an
	// intervening finish is a caller bug (an abandoned write) and is refused, because leaving the
	// previous fields staged would silently write a mixture of two columns.
	void parquet_struct_begin(void *handle, const char *name, int64_t nrows, int32_t nfields)
	{
		auto writer_handle = as_handle(handle);
		if (!writer_handle->struct_staging_name.empty())
		{
			report_fatal_error("parquet_write_column", std::string("column '") + name + // GCOVR_EXCL_LINE
				"': struct staging is already open for column '" + // GCOVR_EXCL_LINE
				writer_handle->struct_staging_name + "' -- the previous write did not finish"); // GCOVR_EXCL_LINE
		}
		if (nfields < 1)
		{
			report_fatal_error("parquet_write_column", std::string("column '") + name + // GCOVR_EXCL_LINE
				"': a struct column must declare at least one field"); // GCOVR_EXCL_LINE
		}
		clear_struct_staging(writer_handle);
		writer_handle->struct_staging_name = name;
		writer_handle->struct_staging_nrows = nrows;
		writer_handle->struct_staging_nfields = nfields;
	}

	// The nine field pushes. Each builds one field's child array from Fortran's flat value buffer
	// and its per-row validity, exactly as the matching list child builder does, and stages it.
	void parquet_struct_field_int32(void *handle, const char *field_name, const int32_t *values, int64_t nrows,
		const int8_t *valid)
	{
		push_struct_child(handle, field_name, build_list_child_array<arrow::Int32Builder>(values, nrows, valid));
	}

	void parquet_struct_field_int64(void *handle, const char *field_name, const int64_t *values, int64_t nrows,
		const int8_t *valid)
	{
		push_struct_child(handle, field_name, build_list_child_array<arrow::Int64Builder>(values, nrows, valid));
	}

	void parquet_struct_field_float32(void *handle, const char *field_name, const float *values, int64_t nrows,
		const int8_t *valid)
	{
		push_struct_child(handle, field_name, build_list_child_array<arrow::FloatBuilder>(values, nrows, valid));
	}

	void parquet_struct_field_float64(void *handle, const char *field_name, const double *values, int64_t nrows,
		const int8_t *valid)
	{
		push_struct_child(handle, field_name, build_list_child_array<arrow::DoubleBuilder>(values, nrows, valid));
	}

	void parquet_struct_field_bool8(void *handle, const char *field_name, const int8_t *values, int64_t nrows,
		const int8_t *valid)
	{
		push_struct_child(handle, field_name,
			build_list_child_array<arrow::BooleanBuilder>(reinterpret_cast<const uint8_t *>(values), nrows, valid));
	}

	void parquet_struct_field_date(void *handle, const char *field_name, const int32_t *values, int64_t nrows,
		const int8_t *valid)
	{
		push_struct_child(handle, field_name, build_list_child_array<arrow::Date32Builder>(values, nrows, valid));
	}

	void parquet_struct_field_time(void *handle, const char *field_name, const int64_t *values, int64_t nrows,
		const int8_t *valid, int32_t unit)
	{
		push_struct_child(handle, field_name,
			build_time_array(values, nrows, 1, unit, valid, field_name, "parquet_write_column"));
	}

	void parquet_struct_field_timestamp(void *handle, const char *field_name, const int64_t *values, int64_t nrows,
		const int8_t *valid, int32_t unit, int32_t is_utc)
	{
		push_struct_child(handle, field_name,
			build_timestamp_array(values, nrows, 1, unit, is_utc, valid, field_name, "parquet_write_column"));
	}

	void parquet_struct_field_string(void *handle, const char *field_name, const int64_t *offsets, const char *data,
		int64_t nrows, int64_t nchars, const int8_t *valid)
	{
		push_struct_child(handle, field_name, build_list_string_child(offsets, data, nrows, nchars, valid));
	}

	// Finishes a WHOLE-COLUMN struct write: decides every nullability flag from the values,
	// assembles the array and stores it. Staging is cleared whether or not this succeeds.
	void parquet_append_struct_column(void *handle, const char *name, int64_t nrows, const int8_t *row_valid)
	{
		auto writer_handle = as_handle(handle);
		check_struct_staging(writer_handle, name, nrows, "parquet_write_column");
		bool row_nullable = has_any_null(row_valid, nrows);
		std::vector<bool> field_nullable(writer_handle->struct_staging_children.size());
		for (size_t i = 0; i < writer_handle->struct_staging_children.size(); ++i)
		{
			field_nullable[i] = writer_handle->struct_staging_children[i]->null_count() > 0;
		}
		if (writer_handle->protected_columns.count(name) != 0)
		{
			// A protected column is non-nullable at EVERY level. Fortran has already refused a
			// null row or a null field value for such a column, so this cannot produce a field
			// that receives nulls -- the invariant build_field's own comment states.
			row_nullable = false;
			for (size_t i = 0; i < field_nullable.size(); ++i) field_nullable[i] = false;
		}
		auto field = build_struct_field(name, writer_handle->struct_staging_field_names,
			writer_handle->struct_staging_children, row_nullable, field_nullable);
		auto array = assemble_struct_array(field->type(), nrows, row_valid,
			writer_handle->struct_staging_children, name, "parquet_write_column");
		clear_struct_staging(writer_handle);
		append_column(writer_handle, name, field, array);
	}

	// Streaming counterpart: same array, stashed into pending_chunk_arrays instead of arrays,
	// with the field built once on the column's first chunk.
	//
	// Every flag comes from ONE resolve_chunk_nullability call with always_nullable set, because
	// for a struct column the 1 + M answers are one answer: nullable at every level unless the
	// column is protected, in which case no level may hold a Null and every one is non-nullable.
	// There is no first-chunk consistency rule to enforce, because the answer does not depend on
	// the chunk -- which is exactly what always_nullable means.
	void parquet_append_struct_column_chunk(void *handle, const char *name, int64_t nrows, const int8_t *row_valid)
	{
		auto writer_handle = as_handle(handle);
		check_struct_staging(writer_handle, name, nrows, "parquet_write_column_chunk");
		bool first_chunk_ever;
		size_t idx = check_column_chunk_write_preconditions(writer_handle, name, first_chunk_ever);
		bool nullable = resolve_chunk_nullability(writer_handle, name, idx, first_chunk_ever,
			/*mask_present=*/true, /*always_nullable=*/true);
		if (first_chunk_ever)
		{
			if (writer_handle->fields.size() <= idx) writer_handle->fields.resize(idx + 1);
			std::vector<bool> field_nullable(writer_handle->struct_staging_children.size(), nullable);
			writer_handle->fields[idx] = build_struct_field(name, writer_handle->struct_staging_field_names,
				writer_handle->struct_staging_children, nullable, field_nullable);
		}
		if (writer_handle->arrays.size() <= idx) writer_handle->arrays.resize(idx + 1);
		auto array = assemble_struct_array(writer_handle->fields[idx]->type(), nrows, row_valid,
			writer_handle->struct_staging_children, name, "parquet_write_column_chunk");
		clear_struct_staging(writer_handle);
		writer_handle->pending_chunk_arrays[static_cast<int>(idx)] =
			align_array_to_field(writer_handle->fields[idx], array);
	}

	// ==== MAP column writes ====
	//
	// A map's entries ARE a two-field struct, so this section reuses the struct staging registry
	// above wholesale rather than building a second one: parquet_write_map.f90 calls
	// parquet_struct_begin(name, NENTRIES, 2), pushes the keys through parquet_struct_field_string
	// and the values through whichever parquet_struct_field_<family> matches, and then calls one of
	// the two finishers below. The staged struct has `nentries` rows, which is exactly what
	// check_struct_staging wants to be told, so no new staging state and no new validation exist
	// here at all -- the finisher simply passes nentries where the struct finisher passes nrows.
	//
	// The cost of that reuse is that this path inherits feature_risks.md Risk-156: the
	// "staging already open" / "no staging open" guards are not reachable from any test, because
	// parquet_writer%handle is a private component and no public API can call these entry points
	// out of order. Phase 5 neither improves nor worsens that, and must not claim otherwise.

	// Finishes a WHOLE-COLUMN map write: decides both nullability flags from the values, assembles
	// the array and stores it. Staging is cleared whether or not this succeeds.
	void parquet_append_map_column(void *handle, const char *name, int64_t nrows, int64_t nentries,
		const int64_t *offsets, const int8_t *row_valid)
	{
		auto writer_handle = as_handle(handle);
		bool row_nullable = has_any_null(row_valid, nrows);
		bool value_nullable = false;
		if (writer_handle->struct_staging_children.size() == 2)
		{
			value_nullable = writer_handle->struct_staging_children[1]->null_count() > 0;
		}
		if (writer_handle->protected_columns.count(name) != 0)
		{
			// A protected column is non-nullable at EVERY level it has. Fortran has already refused
			// a null row or a null value for such a column, so this cannot produce a field that
			// receives nulls -- the invariant build_field's own comment states.
			row_nullable = false;
			value_nullable = false;
		}
		std::shared_ptr<arrow::Field> field;
		auto array = finish_map_staging(writer_handle, name, nrows, nentries, offsets, row_valid,
			row_nullable, value_nullable, field, "parquet_write_column");
		append_column(writer_handle, name, field, array);
	}

	// Streaming counterpart: same array, stashed into pending_chunk_arrays instead of arrays, with
	// the field built once on the column's first chunk.
	//
	// Both flags come from ONE resolve_chunk_nullability call with always_nullable set, exactly as
	// the struct chunk write does: a parquet_map_column carries its null state inside itself, with
	// no caller-supplied mask whose presence could stand in for "might this column contain a Null?",
	// so a streamed map column is in the ALWAYS-NULLABLE class and is nullable at both levels
	// unless the column is protected.
	void parquet_append_map_column_chunk(void *handle, const char *name, int64_t nrows, int64_t nentries,
		const int64_t *offsets, const int8_t *row_valid)
	{
		auto writer_handle = as_handle(handle);
		bool first_chunk_ever;
		size_t idx = check_column_chunk_write_preconditions(writer_handle, name, first_chunk_ever);
		bool nullable = resolve_chunk_nullability(writer_handle, name, idx, first_chunk_ever,
			/*mask_present=*/true, /*always_nullable=*/true);
		std::shared_ptr<arrow::Field> field;
		auto array = finish_map_staging(writer_handle, name, nrows, nentries, offsets, row_valid,
			nullable, nullable, field, "parquet_write_column_chunk");
		if (first_chunk_ever)
		{
			if (writer_handle->fields.size() <= idx) writer_handle->fields.resize(idx + 1);
			writer_handle->fields[idx] = field;
		}
		if (writer_handle->arrays.size() <= idx) writer_handle->arrays.resize(idx + 1);
		writer_handle->pending_chunk_arrays[static_cast<int>(idx)] =
			align_array_to_field(writer_handle->fields[idx], array);
	}


}

extern "C"
{

	// ==== Whole-column append (writer, one row group's full column at a time) ====
	//
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

		// has_any_null, not the default: this call used to omit the argument entirely, which was
		// harmless only because build_field ignored it for col_size > 1. Now that build_field
		// applies it to the child field, omitting it would declare the elements non-nullable while
		// the loop above happily appends nulls into them -- the exact invariant break build_field's
		// own comment warns about. Matches its scalar sibling and the temporal columns.
		append_column(writer_handle, name, build_field(name, use_large ? arrow::large_utf8() : arrow::utf8(), col_size,
			has_any_null(valid_in, nrows * col_size)), array);
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
		if (nrows > 0 && nchars != offsets[nrows])
		{ // GCOVR_EXCL_START -- defensive backstop: the only caller (parquet_strings.f90's
		  // raw_buffers) always derives nchars/offsets from the same column state, so this cannot
		  // currently be triggered through the public API; guards against a future caller/refactor
		  // passing an inconsistent pair instead of trusting offsets silently.
			report_fatal_error("parquet_write_column", "column '" + std::string(name) + "': nchars (" +
				std::to_string(nchars) + ") does not match offsets[nrows] (" +
				std::to_string(offsets[nrows]) + ")");
		}
		// GCOVR_EXCL_STOP

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

	// ==== Variable-length LIST column writes (see the section banner above) ====
	//
	// One entry point per element family, mirroring parquet_read_list_<family>_fill's argument
	// order and vocabulary exactly, so the two halves of this feature read side by side:
	//
	//   nrows       rows in this column
	//   nelems      elements those rows hold between them
	//   offsets     nrows+1 int64 entries, 0-based, offsets[0] == 0
	//   row_valid   per-ROW validity (1 = present, 0 = a NULL list), or null for a null-free column
	//   values      the flattened elements, in row order
	//   elem_valid  per-ELEMENT validity, or null when no element is Null
	//
	// row_valid and elem_valid are POINTERS rather than the read side's plain buffers, because on
	// the write side absence is meaningful: a null row_valid is what declares the outer field
	// non-nullable, exactly as valid_in does for every other append entry point here.
	void parquet_append_list_int32_column(void *handle, const char *name, int64_t nrows, int64_t nelems,
		const int64_t *offsets, const int8_t *row_valid, const int32_t *values, const int8_t *elem_valid)
	{
		auto child = build_list_child_array<arrow::Int32Builder>(values, nelems, elem_valid);
		append_list_column_common(handle, name, nrows, nelems, offsets, row_valid, elem_valid, child,
			"parquet_write_column");
	}

	// Same as parquet_append_list_int32_column, but for an int64 payload.
	void parquet_append_list_int64_column(void *handle, const char *name, int64_t nrows, int64_t nelems,
		const int64_t *offsets, const int8_t *row_valid, const int64_t *values, const int8_t *elem_valid)
	{
		auto child = build_list_child_array<arrow::Int64Builder>(values, nelems, elem_valid);
		append_list_column_common(handle, name, nrows, nelems, offsets, row_valid, elem_valid, child,
			"parquet_write_column");
	}

	// Same as parquet_append_list_int32_column, but for a float32 payload.
	void parquet_append_list_float32_column(void *handle, const char *name, int64_t nrows, int64_t nelems,
		const int64_t *offsets, const int8_t *row_valid, const float *values, const int8_t *elem_valid)
	{
		auto child = build_list_child_array<arrow::FloatBuilder>(values, nelems, elem_valid);
		append_list_column_common(handle, name, nrows, nelems, offsets, row_valid, elem_valid, child,
			"parquet_write_column");
	}

	// Same as parquet_append_list_int32_column, but for a float64 payload.
	void parquet_append_list_float64_column(void *handle, const char *name, int64_t nrows, int64_t nelems,
		const int64_t *offsets, const int8_t *row_valid, const double *values, const int8_t *elem_valid)
	{
		auto child = build_list_child_array<arrow::DoubleBuilder>(values, nelems, elem_valid);
		append_list_column_common(handle, name, nrows, nelems, offsets, row_valid, elem_valid, child,
			"parquet_write_column");
	}

	// Same as parquet_append_list_int32_column, but for a boolean payload (one int8 per element).
	void parquet_append_list_bool8_column(void *handle, const char *name, int64_t nrows, int64_t nelems,
		const int64_t *offsets, const int8_t *row_valid, const int8_t *values, const int8_t *elem_valid)
	{
		auto child = build_list_child_array<arrow::BooleanBuilder>(
			reinterpret_cast<const uint8_t *>(values), nelems, elem_valid);
		append_list_column_common(handle, name, nrows, nelems, offsets, row_valid, elem_valid, child,
			"parquet_write_column");
	}

	// Same as parquet_append_list_int32_column, but for a date payload (int32 days since the epoch).
	void parquet_append_list_date_column(void *handle, const char *name, int64_t nrows, int64_t nelems,
		const int64_t *offsets, const int8_t *row_valid, const int32_t *values, const int8_t *elem_valid)
	{
		auto child = build_list_child_array<arrow::Date32Builder>(values, nelems, elem_valid);
		append_list_column_common(handle, name, nrows, nelems, offsets, row_valid, elem_valid, child,
			"parquet_write_column");
	}

	// Same as parquet_append_list_int32_column, but for a time payload -- values are canonical
	// nanoseconds-of-day and build_time_array scales them down to `unit`, aborting on a value with
	// finer precision than the column's declared unit, exactly as a scalar time column's write does.
	void parquet_append_list_time_column(void *handle, const char *name, int64_t nrows, int64_t nelems,
		const int64_t *offsets, const int8_t *row_valid, const int64_t *values, const int8_t *elem_valid,
		int32_t unit)
	{
		auto child = build_time_array(values, nelems, 1, unit, elem_valid, name, "parquet_write_column");
		append_list_column_common(handle, name, nrows, nelems, offsets, row_valid, elem_valid, child,
			"parquet_write_column");
	}

	// Same as parquet_append_list_int32_column, but for a timestamp payload -- values are already
	// expressed in `unit`'s own unit (Fortran did the conversion and its range check), and `is_utc`
	// selects the UTC-adjusted vs timezone-naive Arrow type.
	void parquet_append_list_timestamp_column(void *handle, const char *name, int64_t nrows, int64_t nelems,
		const int64_t *offsets, const int8_t *row_valid, const int64_t *values, const int8_t *elem_valid,
		int32_t unit, int32_t is_utc)
	{
		auto child = build_timestamp_array(values, nelems, 1, unit, is_utc, elem_valid, name,
			"parquet_write_column");
		append_list_column_common(handle, name, nrows, nelems, offsets, row_valid, elem_valid, child,
			"parquet_write_column");
	}

	// Same as parquet_append_list_int32_column, but for a string payload. The elements arrive in the
	// same packed offsets+bytes layout a parquet_string_column stores natively: `str_offsets` is
	// nelems+1 int64 entries over `data`'s `nchars` bytes.
	void parquet_append_list_string_column(void *handle, const char *name, int64_t nrows, int64_t nelems,
		int64_t nchars, const int64_t *offsets, const int8_t *row_valid, const int64_t *str_offsets,
		const char *data, const int8_t *elem_valid)
	{
		auto child = build_list_string_child(str_offsets, data, nelems, nchars, elem_valid);
		append_list_column_common(handle, name, nrows, nelems, offsets, row_valid, elem_valid, child,
			"parquet_write_column");
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
		// always_nullable, like every other temporal kind: a parquet_date carries its own null
		// state, so `valid_in` here reflects THIS chunk's elements rather than a caller's choice
		// to pass a mask, and a null-free first row group says nothing about row group 7. A date
		// column reaches the generic template rather than stash_temporal_column_chunk only
		// because it is int32-backed -- see resolve_chunk_nullability.
		append_typed_column_chunk<arrow::Date32Builder>(handle, name, data, col_size, valid_in, arrow::date32(),
			/*always_nullable=*/true);
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

		bool nullable = resolve_chunk_nullability(writer_handle, name, idx, first_chunk_ever, valid_in != nullptr);
		if (first_chunk_ever)
		{
			if (writer_handle->fields.size() <= idx) writer_handle->fields.resize(idx + 1);
			writer_handle->fields[idx] = build_field(name, arrow::large_utf8(), 1, nullable);
		}
		if (writer_handle->arrays.size() <= idx) writer_handle->arrays.resize(idx + 1);
		writer_handle->pending_chunk_arrays[static_cast<int>(idx)] = align_array_to_field(writer_handle->fields[idx], array);
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

		bool nullable = resolve_chunk_nullability(writer_handle, name, idx, first_chunk_ever, valid_in != nullptr);
		if (first_chunk_ever)
		{
			if (writer_handle->fields.size() <= idx) writer_handle->fields.resize(idx + 1);
			writer_handle->fields[idx] = build_field(name, arrow::large_utf8(), col_size, nullable);
		}
		if (writer_handle->arrays.size() <= idx) writer_handle->arrays.resize(idx + 1);
		writer_handle->pending_chunk_arrays[static_cast<int>(idx)] = align_array_to_field(writer_handle->fields[idx], array);
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
		if (nrows > 0 && nchars != offsets[nrows])
		{ // GCOVR_EXCL_START -- defensive backstop: the only caller (parquet_strings.f90's
		  // raw_buffers) always derives nchars/offsets from the same column state, so this cannot
		  // currently be triggered through the public API; guards against a future caller/refactor
		  // passing an inconsistent pair instead of trusting offsets silently.
			report_fatal_error("parquet_write_column_chunk", "column '" + std::string(name) + "': nchars (" +
				std::to_string(nchars) + ") does not match offsets[nrows] (" +
				std::to_string(offsets[nrows]) + ")");
		}
		// GCOVR_EXCL_STOP
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

		// always_nullable: a parquet_string_column's nulls live in the container, not in a mask,
		// so "was a mask passed" cannot predict whether a later row group holds one. Nullable
		// unless the column is protected -- see resolve_chunk_nullability.
		bool nullable = resolve_chunk_nullability(writer_handle, name, idx, first_chunk_ever,
			/*mask_present=*/true, /*always_nullable=*/true);
		if (first_chunk_ever)
		{
			if (writer_handle->fields.size() <= idx) writer_handle->fields.resize(idx + 1);
			writer_handle->fields[idx] = build_field(name, arrow::large_utf8(), 1, nullable);
		}
		if (writer_handle->arrays.size() <= idx) writer_handle->arrays.resize(idx + 1);
		writer_handle->pending_chunk_arrays[static_cast<int>(idx)] = align_array_to_field(writer_handle->fields[idx], array);
	}

	// Ends the currently-open row group: verifies every column known so far has data for it
	// (either a slice of a whole parquet_write_column array, or a pending chunk -- see
	// check_column_chunk_write_preconditions), lazily opens the underlying row-group-oriented
	// FileWriter on the very first call (locking the file's schema from every column
	// established by then -- see the comment on ParquetWriterHandle::row_group_writer), then
	// writes one column chunk per column, in schema order, as parquet::arrow::FileWriter::
	// WriteColumnChunk requires.

	// ==== Variable-length LIST column writes, streamed (see the section banner further above) ====
	//
	// Row-group-scoped counterparts of parquet_append_list_<family>_column, taking exactly the same
	// arguments -- each call covers one row group's rows rather than the whole column. See
	// append_list_column_chunk_common for the nullability rule, which differs from the whole-column
	// path's: a streamed list column is nullable at both levels unless it is protected.
	void parquet_append_list_int32_column_chunk(void *handle, const char *name, int64_t nrows, int64_t nelems,
		const int64_t *offsets, const int8_t *row_valid, const int32_t *values, const int8_t *elem_valid)
	{
		auto child = build_list_child_array<arrow::Int32Builder>(values, nelems, elem_valid);
		append_list_column_chunk_common(handle, name, nrows, nelems, offsets, row_valid, child,
			"parquet_write_column_chunk");
	}

	// Same as parquet_append_list_int32_column_chunk, but for an int64 payload.
	void parquet_append_list_int64_column_chunk(void *handle, const char *name, int64_t nrows, int64_t nelems,
		const int64_t *offsets, const int8_t *row_valid, const int64_t *values, const int8_t *elem_valid)
	{
		auto child = build_list_child_array<arrow::Int64Builder>(values, nelems, elem_valid);
		append_list_column_chunk_common(handle, name, nrows, nelems, offsets, row_valid, child,
			"parquet_write_column_chunk");
	}

	// Same as parquet_append_list_int32_column_chunk, but for a float32 payload.
	void parquet_append_list_float32_column_chunk(void *handle, const char *name, int64_t nrows, int64_t nelems,
		const int64_t *offsets, const int8_t *row_valid, const float *values, const int8_t *elem_valid)
	{
		auto child = build_list_child_array<arrow::FloatBuilder>(values, nelems, elem_valid);
		append_list_column_chunk_common(handle, name, nrows, nelems, offsets, row_valid, child,
			"parquet_write_column_chunk");
	}

	// Same as parquet_append_list_int32_column_chunk, but for a float64 payload.
	void parquet_append_list_float64_column_chunk(void *handle, const char *name, int64_t nrows, int64_t nelems,
		const int64_t *offsets, const int8_t *row_valid, const double *values, const int8_t *elem_valid)
	{
		auto child = build_list_child_array<arrow::DoubleBuilder>(values, nelems, elem_valid);
		append_list_column_chunk_common(handle, name, nrows, nelems, offsets, row_valid, child,
			"parquet_write_column_chunk");
	}

	// Same as parquet_append_list_int32_column_chunk, but for a boolean payload.
	void parquet_append_list_bool8_column_chunk(void *handle, const char *name, int64_t nrows, int64_t nelems,
		const int64_t *offsets, const int8_t *row_valid, const int8_t *values, const int8_t *elem_valid)
	{
		auto child = build_list_child_array<arrow::BooleanBuilder>(
			reinterpret_cast<const uint8_t *>(values), nelems, elem_valid);
		append_list_column_chunk_common(handle, name, nrows, nelems, offsets, row_valid, child,
			"parquet_write_column_chunk");
	}

	// Same as parquet_append_list_int32_column_chunk, but for a date payload.
	void parquet_append_list_date_column_chunk(void *handle, const char *name, int64_t nrows, int64_t nelems,
		const int64_t *offsets, const int8_t *row_valid, const int32_t *values, const int8_t *elem_valid)
	{
		auto child = build_list_child_array<arrow::Date32Builder>(values, nelems, elem_valid);
		append_list_column_chunk_common(handle, name, nrows, nelems, offsets, row_valid, child,
			"parquet_write_column_chunk");
	}

	// Same as parquet_append_list_int32_column_chunk, but for a time payload.
	void parquet_append_list_time_column_chunk(void *handle, const char *name, int64_t nrows, int64_t nelems,
		const int64_t *offsets, const int8_t *row_valid, const int64_t *values, const int8_t *elem_valid,
		int32_t unit)
	{
		auto child = build_time_array(values, nelems, 1, unit, elem_valid, name, "parquet_write_column_chunk");
		append_list_column_chunk_common(handle, name, nrows, nelems, offsets, row_valid, child,
			"parquet_write_column_chunk");
	}

	// Same as parquet_append_list_int32_column_chunk, but for a timestamp payload.
	void parquet_append_list_timestamp_column_chunk(void *handle, const char *name, int64_t nrows, int64_t nelems,
		const int64_t *offsets, const int8_t *row_valid, const int64_t *values, const int8_t *elem_valid,
		int32_t unit, int32_t is_utc)
	{
		auto child = build_timestamp_array(values, nelems, 1, unit, is_utc, elem_valid, name,
			"parquet_write_column_chunk");
		append_list_column_chunk_common(handle, name, nrows, nelems, offsets, row_valid, child,
			"parquet_write_column_chunk");
	}

	// Same as parquet_append_list_int32_column_chunk, but for a string payload.
	void parquet_append_list_string_column_chunk(void *handle, const char *name, int64_t nrows, int64_t nelems,
		int64_t nchars, const int64_t *offsets, const int8_t *row_valid, const int64_t *str_offsets,
		const char *data, const int8_t *elem_valid)
	{
		auto child = build_list_string_child(str_offsets, data, nelems, nchars, elem_valid);
		append_list_column_chunk_common(handle, name, nrows, nelems, offsets, row_valid, child,
			"parquet_write_column_chunk");
	}

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
			parquet::WriterProperties::Builder writer_props_builder;
			writer_props_builder.compression(writer_handle->compression_codec)
				->compression_level(writer_handle->compression_level);
			apply_float_byte_stream_split(writer_props_builder, writer_handle->fields);
			auto writer_properties = writer_props_builder.build();

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

	// ==== Test-only debug hooks (error-scenario/fixture support, never public API) ====
	//
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

	// Test-only: overrides g_debug_list_offset_limit (see its own comment) so an error scenario can
	// exercise a variable-length LIST column's arrow::large_list() write/read path with a tiny
	// fixture instead of needing a genuine 2-billion-element column. Safe as a process-global for
	// the same reason as the string one above: the scenario that sets it runs as its own isolated
	// subprocess. <= 0 restores the real limit.
	void parquet_debug_set_list_offset_limit(int64_t n)
	{
		g_debug_list_offset_limit = n;
	}

	// Test-only: overrides g_debug_map_offset_limit (see its own comment) so an error scenario can
	// reach check_map_entries_fit_arrow_limit's REFUSAL with a tiny fixture instead of a genuinely
	// 2-billion-entry column. Note what makes the map case different from the list one above: there
	// the override exercises a WIDENING (large_list), here it exercises an abort, because Arrow has
	// no large_map to widen into. Safe as a process-global for the same reason: the scenario that
	// sets it runs as its own isolated subprocess. <= 0 restores the real limit.
	void parquet_debug_set_map_offset_limit(int64_t n)
	{
		g_debug_map_offset_limit = n;
	}

	// Test-only: overrides g_debug_col_size_limit (see its own comment) so
	// test/error_scenarios.f90's scenario_col_size_overflow can exercise the
	// check_col_size_fits_arrow_limit abort path with a tiny fixture instead of a genuinely
	// oversized vector column. Same process-global/subprocess-isolation reasoning as
	// parquet_debug_set_string_offset_limit, above. Pass n<=0 to restore the real production limit.
	// GCOVR_EXCL'd: scenario_col_size_overflow always ends by aborting via
	// check_col_size_fits_arrow_limit's report_fatal_error, which discards the whole process's
	// gcov data -- so this setter, though genuinely called every time, never shows as covered
	// either. Collateral of the same fatal-exit-discards-coverage mechanism, not a separate gap.
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

	// Test-only: arms (and zeroes) the SortRowLess comparison counter. Exists because a partial sort
	// that is not actually partial is INVISIBLE to every correctness test -- returning the first n of
	// a full sort is correct and merely slower. A comparison count is what distinguishes them, and it
	// is deterministic where a wall-clock benchmark is not.
	//
	// Note for anyone writing such a test: the counting fast path performs ZERO comparisons, so a
	// low-cardinality integer key reports 0 on both the partial and the full path. Use a key the
	// counting path declines (a real key, or high-cardinality integers), or turn it off first with
	// parquet_set_sort_counting_path(.false.).
	void parquet_debug_set_count_sort_comparisons(int enable)
	{
		g_debug_count_sort_comparisons = (enable != 0);
		g_debug_sort_comparison_count = 0;
	}

	// Test-only: comparisons counted since the last parquet_debug_set_count_sort_comparisons call.
	int64_t parquet_debug_get_sort_comparisons(void)
	{
		return g_debug_sort_comparison_count;
	}

	// Test-only: how many threads the last threaded sort actually put to work, the calling thread
	// included. 1 means it ran serially, whatever `threads=` asked for.
	//
	// **This is the whole reason a `threads=` argument is testable at all.** A parallel sort produces
	// a permutation IDENTICAL to the serial one -- that identity is what makes the feature safe, and
	// it is also what makes a `threads=` that is silently ignored pass every correctness test ever
	// written for it. Zero parallelism is a passing test, exactly as zero comparisons was for the
	// partial sort above (feature_risks.md Risk-35).
	int64_t parquet_debug_get_sort_threads_used(void)
	{
		return g_debug_sort_threads_used;
	}

	// Test-only: shrinks the smallest output range the co-ranked merge will give its own thread, so a
	// test-sized array reaches the co-rank at all. Pass 0 (or less) to restore the real floor.
	//
	// **This is what makes the merge testable, and its absence hid two real defects.** At the real
	// floor no pair below 32768 elements is ever segmented, so a dense small-size sweep exercises the
	// unsegmented path -- which is the old, correct, pairwise merge -- and passes while calling
	// sort_corank zero times. Two deliberate co-rank mutations survived the whole suite before this
	// existed. Same reasoning as parquet_debug_set_col_size_limit: a constant that only a
	// prohibitively large input can cross needs a way down for tests.
	void parquet_debug_set_sort_merge_min_segment(int64_t n)
	{
		g_debug_sort_merge_min_segment = (n > 0) ? n : -1;
	}

	// Test-only: lowers the C++ engine's threading floor so a small fixture can reach the parallel
	// path. Replaces what parquet_set_sort_parallel_min_rows did for tests before that setting was
	// retired; <= 0 restores the built-in kSortParallelMinRows.
	void parquet_debug_set_sort_parallel_min_rows(int64_t n)
	{
		g_debug_sort_parallel_min_rows = (n > 0) ? n : -1;
	}

	// Test-only: how many threads worked the last threaded sort's FINAL merge round, the calling
	// thread included. 1 means that round ran serially.
	//
	// **This is the only thing that can tell a co-ranked merge from a decorative one.** Every thread
	// count returns the same permutation -- that identity is what makes `threads=` safe -- so a merge
	// that silently stopped splitting its final round would pass every correctness assertion in the
	// suite while putting the O(n) serial tail straight back. See g_debug_sort_merge_threads_used,
	// and feature_risks.md Risk-49.
	int64_t parquet_debug_get_sort_merge_threads_used(void)
	{
		return g_debug_sort_merge_threads_used;
	}

	// Maintainer diagnostic: nanoseconds the last threaded sort spent in phase 0 (the per-chunk
	// std::sorts), phase 1 (every merge round but the last) and phase 2 (the last round alone).
	// Anything else answers 0. See g_debug_sort_phase_ns for why the last round is split out: it is
	// the single-threaded tail whose share decides whether a co-ranked parallel merge earns its
	// complexity, and bench/benchmark_table.f90's --mode=argsort is what reads it.
	int64_t parquet_debug_get_sort_phase_ns(int phase)
	{
		if (phase < 0 || phase > 2) return 0;
		return g_debug_sort_phase_ns[phase];
	}

	// Test-only: how many row groups the most recent screen ruled out, in this process. Without it
	// every equality test would pass just as happily against a screen that never prunes anything,
	// so the F4 tests assert this alongside the results -- nonzero where pruning is expected, and
	// zero for every case the screen is supposed to decline.
	//
	// Process-global rather than per-reader because parquet_reader's components are private, so a
	// test cannot pass a handle in (see CLAUDE.md's "A new reader query that a sibling module
	// needs has to be PUBLIC parquet API" -- and a pruned-row-group count is a diagnostic, not
	// something to add to the public surface for). The consequence is that the suite asserting on
	// it must not run its tests concurrently: test/run_tester.f90's suite_is_safe_to_parallelize
	// excludes "filter_screen" for exactly this reason.
	int64_t parquet_debug_get_row_groups_pruned(void)
	{
		return g_debug_row_groups_pruned;
	}


	// Test-only: overrides g_debug_force_sample_mask_error (see its own comment, next to
	// parquet_reader_set_sample) so test/error_scenarios.f90 can exercise parquet_reader_set_sample's
	// failure return -- and the Fortran-side error stop that surfaces it (parquet_apply_sample,
	// parquet_read.f90) -- on a tiny fixture, without needing a genuine BooleanBuilder allocation
	// failure. Pass 0 to restore normal (non-forced-error) behavior.
	void parquet_debug_set_force_sample_mask_error(int enable)
	{
		g_debug_force_sample_mask_error = (enable != 0);
	}

	// Forces parquet_reader_set_sample's keep_len guard to reject a length that is in fact correct
	// (it compares against total_nrows + 1 while set). The guard is UNREACHABLE through the public
	// API -- parquet_apply_sample sizes the mask from the same handle's total_nrows, so the two
	// values come from one source and cannot differ -- and this hook is what makes it testable
	// rather than defensive code no fixture can reach. See error_scenarios.f90's
	// sample_mask_length_mismatch.
	void parquet_debug_set_force_sample_len_mismatch(int enable)
	{
		g_debug_force_sample_len_mismatch = (enable != 0);
	}

	// Test-only: returns g_debug_physical_column_read_count (see its own comment) -- lets
	// test/error_scenarios.f90's scenario_nested_struct_shares_cached_read prove that reading two
	// different leaf paths under the same top-level struct column only triggers one genuine disk
	// read of that struct, i.e. that struct-path resolution shares get_single_chunk_array's
	// existing column_cache rather than re-reading per leaf path.
	int64_t parquet_debug_get_physical_column_read_count()
	{
		return g_debug_physical_column_read_count.load();
	}

	// Test-only: resets g_debug_physical_column_read_count to 0, so a scenario can zero the
	// counter right before the specific reads it wants to measure.
	void parquet_debug_reset_physical_column_read_count()
	{
		g_debug_physical_column_read_count.store(0);
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

	// Test-only: writes a tiny fixture file with one BINARY (byte-array) column, a physical type
	// this library's own writer never produces and its reader deliberately does not support --
	// same convention as parquet_debug_write_string_view_fixture above.
	//
	// It exists for one scenario: the "column has a type that filtering does not support" branch
	// of eval_filter_clause. Every other physical type this project has a fixture for is now
	// filterable (the nine canonical types plus the extended int/uint/half-float/decimal read
	// types), so without a genuinely unsupported column that branch would have no test at all.
	void parquet_debug_write_binary_fixture(const char *path, const char *column_name)
	{
		arrow::BinaryBuilder builder;
		auto check = [](const arrow::Status &st)
		{
			if (!st.ok()) throw std::runtime_error("parquet_debug_write_binary_fixture: " + st.ToString());
		};
		check(builder.Append(std::string("\x01\x02\x03", 3)));
		check(builder.Append(std::string("\xff", 1)));
		check(builder.AppendNull());

		std::shared_ptr<arrow::Array> array;
		check(builder.Finish(&array));

		auto field = arrow::field(column_name, arrow::binary());
		auto schema = arrow::schema({field});
		auto table = arrow::Table::Make(schema, {array});

		// file-I/O backstop, not fixture-triggerable.
		auto outfile_result = arrow::io::FileOutputStream::Open(path);
		if (!outfile_result.ok())
			throw std::runtime_error("parquet_debug_write_binary_fixture: failed to open '" + // GCOVR_EXCL_LINE
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

	// Frees a writer's handle without finalizing/writing the actual output -- used by the
	// Fortran-side writer_finalize FINAL procedure (parquet_write.f90) as the safety net for a
	// writer whose variable goes out of scope, is reassigned, or is re-opened while still open,
	// instead of close_parquet_writer. Unlike close_parquet_writer (and close_streaming_writer),
	// this never builds/writes the final table, never checks that every declared column was
	// written, and never checks that every row group opened via parquet_new_row_group was
	// finished -- all three of those checks throw a std::runtime_error on failure, and since an
	// implicit finalizer has no Fortran-level error-stop message to attach it to, that exception
	// crosses the extern "C" boundary uncaught, causing std::terminate()/process abort instead of
	// a clean diagnostic. The underlying C++ object (and, via its own destructor, its output file
	// handle) is simply released; the resulting output file is not guaranteed to be a complete or
	// valid parquet file -- callers should always prefer an explicit parquet_close_writer call,
	// which does perform those checks.
	void abandon_parquet_writer(void *handle)
	{
		auto writer_handle = as_handle(handle);
		delete writer_handle.release();
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
		// than a flat row count. chunk_size_from_bytes_per_row (far above)
		// is that arithmetic and the ONLY place it lives -- this path and
		// estimate_chunk_size_from_schema's are the two callers, and they
		// serve different writers (the whole-table write here, the
		// streaming parquet_new_row_group path there). Restating the
		// arithmetic inline again would silently give one of the two its
		// own copy of the byte target, so parquet_set_target_row_group_bytes
		// would govern only one kind of write -- see feature_risks.md
		// Risk-43 and tools/check_source_conventions.py's
		// check_row_group_sizing_not_duplicated, which enforces this.
		//
		// The two steps below that are NOT part of the shared arithmetic
		// stay here: a row group can never exceed the table's own row
		// count, and the result is floored at 1.
		auto effective_chunk_size = writer_handle->chunk_size;
		if (effective_chunk_size <= 0)
		{
			auto num_rows = table->num_rows();
			auto total_bytes = arrow::util::TotalBufferSize(*table);
			if (num_rows > 0 && total_bytes > 0)
			{
				double bytes_per_row = static_cast<double>(total_bytes) / static_cast<double>(num_rows);
				effective_chunk_size = std::min(chunk_size_from_bytes_per_row(bytes_per_row), num_rows);
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

			// The same ceiling for a VARIABLE-LENGTH list column, whose per-row element count is
			// the data's rather than a declared col_size -- so this one has to read the offsets.
			// See max_list_row_length for why the clamp uses the longest row (conservative, and
			// clamping may only ever be conservative) while the explicit-chunk_size branch below
			// walks the actual row-group windows instead (exact, and aborting must be).
			auto max_list_len = max_list_row_length(writer_handle->arrays);
			if (max_list_len > 1)
			{
				int64_t limit = g_debug_list_element_count_limit > 0 ? g_debug_list_element_count_limit : kArrowInt32ListElementCountLimit;
				effective_chunk_size = std::min(effective_chunk_size, std::max<int64_t>(limit / max_list_len, 1));
			}
		}
		else
		{
			check_explicit_chunk_size_fits_arrow_limit(effective_chunk_size, writer_handle->fields, "close_parquet_writer");
			check_explicit_chunk_size_fits_list_limit(effective_chunk_size, writer_handle->fields,
				writer_handle->arrays, "close_parquet_writer");
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
		parquet::WriterProperties::Builder writer_props_builder;
		writer_props_builder.compression(writer_handle->compression_codec)
			->compression_level(writer_handle->compression_level)
			->max_row_group_length(effective_chunk_size);
		apply_float_byte_stream_split(writer_props_builder, writer_handle->fields);
		auto writer_properties = writer_props_builder.build();

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
