#if __cplusplus < 202002L
#error "parquet-fortran requires C++20 (Arrow/Parquet headers use std::span unconditionally, regardless of Arrow version). " \
	"Set FPM_CXXFLAGS to include -std=c++20 (see README.md) before running fpm build/test."
#endif

#include <arrow/api.h>
#include <arrow/array/concatenate.h>
#include <arrow/array/util.h>
#include <arrow/io/api.h>
#include <arrow/util/thread_pool.h>
#include <parquet/arrow/reader.h>
#include <parquet/arrow/writer.h>

#include <atomic>
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
#include <unordered_map>
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
		int64_t nrows = 0;
		std::unordered_map<int, std::shared_ptr<arrow::Array>> column_cache;
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

		auto array = combine_column_chunks(chunked, name);
		reader_handle->column_cache.emplace(static_cast<int>(idx), array);
		return array;
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

	static std::shared_ptr<arrow::Array> get_uniform_list_values(const std::shared_ptr<arrow::Array> &array,
		const std::string &name, int64_t nrows, int64_t col_size)
	{
		if (array->type_id() == arrow::Type::FIXED_SIZE_LIST)
		{
			auto list_arr = std::static_pointer_cast<arrow::FixedSizeListArray>(array);
			if (list_arr->length() != nrows || static_cast<int64_t>(list_arr->value_length()) != col_size)
			{
				throw std::runtime_error(std::string("shape mismatch for column: ") + name);
			}
			return list_arr->values()->Slice(list_arr->value_offset(0), nrows * col_size);
		}
		if (array->type_id() == arrow::Type::LIST)
		{
			auto list_arr = std::static_pointer_cast<arrow::ListArray>(array);
			if (list_arr->length() != nrows)
			{
				throw std::runtime_error(std::string("nrows mismatch for column: ") + name);
			}
			for (int64_t i = 0; i < nrows; ++i)
			{
				if (static_cast<int64_t>(list_arr->value_length(i)) != col_size)
				{
					throw std::runtime_error(std::string("shape mismatch for column: ") + name);
				}
			}
			return list_arr->values()->Slice(list_arr->value_offset(0), nrows * col_size);
		}
		if (array->type_id() == arrow::Type::LARGE_LIST)
		{
			auto list_arr = std::static_pointer_cast<arrow::LargeListArray>(array);
			if (list_arr->length() != nrows)
			{
				throw std::runtime_error(std::string("nrows mismatch for column: ") + name);
			}
			for (int64_t i = 0; i < nrows; ++i)
			{
				if (list_arr->value_length(i) != col_size)
				{
					throw std::runtime_error(std::string("shape mismatch for column: ") + name);
				}
			}
			return list_arr->values()->Slice(list_arr->value_offset(0), nrows * col_size);
		}
		throw std::runtime_error(std::string("type mismatch for column: ") + name +
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
		handle->outfile = arrow::io::FileOutputStream::Open(filename).ValueOrDie();
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
		auto infile = arrow::io::ReadableFile::Open(filename).ValueOrDie();
		parquet::arrow::FileReaderBuilder builder;
		auto status = builder.Open(infile);
		if (!status.ok())
		{
			delete handle;
			throw std::runtime_error(status.ToString());
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
			throw std::runtime_error(status.ToString());
		}

		status = handle->reader->GetSchema(&handle->schema);
		if (!status.ok())
		{
			delete handle;
			throw std::runtime_error(status.ToString());
		}

		handle->nrows = handle->reader->parquet_reader()->metadata()->num_rows();
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
	void parquet_reader_prefetch_columns(void *handle, const char *names_packed, int64_t item_len, int64_t n)
	{
		auto reader_handle = as_reader_handle(handle);
		if (n <= 0) return;

		std::vector<int> indices;
		std::vector<std::string> names;
		indices.reserve(static_cast<size_t>(n));
		names.reserve(static_cast<size_t>(n));
		for (int64_t i = 0; i < n; ++i)
		{
			std::string name(names_packed + i * item_len, static_cast<size_t>(item_len));
			name = trim_right_spaces_and_nuls(name);
			indices.push_back(static_cast<int>(get_column_index(reader_handle, name.c_str())));
			names.push_back(name);
		}

		std::shared_ptr<arrow::Table> table;
		auto status = reader_handle->reader->ReadTable(indices, &table);
		if (!status.ok())
		{
			throw std::runtime_error(status.ToString());
		}

		for (int64_t i = 0; i < n; ++i)
		{
			auto chunked = table->column(static_cast<int>(i));
			auto array = combine_column_chunks(chunked, names[static_cast<size_t>(i)]);
			reader_handle->column_cache[indices[static_cast<size_t>(i)]] = array;
		}
	}

	int64_t parquet_reader_get_nrows(void *handle)
	{
		auto reader_handle = as_reader_handle(handle);
		return reader_handle->nrows;
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

}

	// This library has no representation for a per-element missing value
	// unless the caller opts in via a validity-output buffer (`valid_out`,
	// nullable): if valid_out is null and the array contains any Parquet
	// Nulls, throws immediately (the default, strict behavior) rather than
	// silently copying out whatever undefined bit pattern Arrow happens to
	// leave in a null slot's data buffer. If valid_out is non-null, no
	// exception is thrown regardless of nulls: valid_out[i] is filled with
	// 1 (valid) / 0 (Null) for every i. The caller is then responsible for
	// overwriting the corresponding data[i] with a safe default wherever
	// valid_out[i] == 0 (see fill_null_default/fill_null_default_string
	// below) -- these two responsibilities are deliberately kept separate
	// so this function stays a simple, type-agnostic yes/no null report.
	static void check_or_report_nulls(const std::shared_ptr<arrow::Array> &array, const std::string &name, int8_t *valid_out)
	{
		if (valid_out == nullptr)
		{
			if (array->null_count() != 0)
			{
				throw std::runtime_error(std::string("column contains Null value(s), which is not supported: ") + name);
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
	// way data[] is (k = i * col_size + j). Same throw-vs-report contract
	// as check_or_report_nulls.
	static void report_nulls_list_full(
		const std::shared_ptr<arrow::Array> &list_array,
		const std::shared_ptr<arrow::Array> &vals_any,
		const std::string &name,
		int64_t nrows, int64_t col_size, int64_t row_offset,
		int8_t *valid_out)
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
			throw std::runtime_error(std::string("column contains Null value(s), which is not supported: ") + name);
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
		int8_t *valid_out)
	{
		bool any_null = false;
		for (int64_t i = 0; i < nrows; ++i)
		{
			if (!list_array->IsValid(i) || !vals_any->IsValid(i * col_size + offset)) { any_null = true; break; }
		}
		if (!any_null) return;

		if (valid_out == nullptr)
		{
			throw std::runtime_error(std::string("column contains Null value(s), which is not supported: ") + name);
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

	template <typename ArrowArrayType, typename CType>
	static void read_scalar_primitive(void *handle, const char *name, CType *data, int64_t nrows, arrow::Type::type expected_type, const char *expected_name)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		if (array->type_id() != expected_type)
		{
			throw std::runtime_error(std::string("type mismatch for column: ") + name +
				" (expected " + expected_name +
				", got " + array->type()->ToString() + ")");
		}
		auto arr = std::static_pointer_cast<ArrowArrayType>(array);
		if (arr->length() != nrows)
		{
			throw std::runtime_error(std::string("nrows mismatch for column: ") + name);
		}
		check_no_nulls(arr, name);
		for (int64_t i = 0; i < nrows; ++i)
		{
			data[i] = static_cast<CType>(arr->Value(i));
		}
	}

	static std::shared_ptr<arrow::Array> get_row_list_values(const std::shared_ptr<arrow::Array> &array,
		const std::string &name, int64_t row_index, int64_t col_size)
	{
		if (array->type_id() == arrow::Type::FIXED_SIZE_LIST)
		{
			auto list_arr = std::static_pointer_cast<arrow::FixedSizeListArray>(array);
			if (row_index < 1 || row_index > list_arr->length())
			{
				throw std::runtime_error("row_index out of bounds");
			}
			if (static_cast<int64_t>(list_arr->value_length()) != col_size)
			{
				throw std::runtime_error(std::string("col_size mismatch for column: ") + name);
			}
			return list_arr->values()->Slice(list_arr->value_offset(row_index - 1), col_size);
		}
		if (array->type_id() == arrow::Type::LIST)
		{
			auto list_arr = std::static_pointer_cast<arrow::ListArray>(array);
			if (row_index < 1 || row_index > list_arr->length())
			{
				throw std::runtime_error("row_index out of bounds");
			}
			if (static_cast<int64_t>(list_arr->value_length(row_index - 1)) != col_size)
			{
				throw std::runtime_error(std::string("col_size mismatch for column: ") + name);
			}
			return list_arr->values()->Slice(list_arr->value_offset(row_index - 1), col_size);
		}
		if (array->type_id() == arrow::Type::LARGE_LIST)
		{
			auto list_arr = std::static_pointer_cast<arrow::LargeListArray>(array);
			if (row_index < 1 || row_index > list_arr->length())
			{
				throw std::runtime_error("row_index out of bounds");
			}
			if (list_arr->value_length(row_index - 1) != col_size)
			{
				throw std::runtime_error(std::string("col_size mismatch for column: ") + name);
			}
			return list_arr->values()->Slice(list_arr->value_offset(row_index - 1), col_size);
		}
		throw std::runtime_error(std::string("type mismatch for column: ") + name +
			" (expected fixed_size_list/list/large_list, got " + array->type()->ToString() + ")");
	}

	static double numeric_value_at(const std::shared_ptr<arrow::Array> &array, const std::string &name, int64_t idx)
	{
		switch (array->type_id())
		{
		case arrow::Type::DOUBLE:
			return std::static_pointer_cast<arrow::DoubleArray>(array)->Value(idx);
		case arrow::Type::FLOAT:
			return static_cast<double>(std::static_pointer_cast<arrow::FloatArray>(array)->Value(idx));
		case arrow::Type::INT32:
			return static_cast<double>(std::static_pointer_cast<arrow::Int32Array>(array)->Value(idx));
		case arrow::Type::INT64:
			return static_cast<double>(std::static_pointer_cast<arrow::Int64Array>(array)->Value(idx));
		default:
			throw std::runtime_error(std::string("type mismatch for list values in column: ") + name +
				" (expected numeric, got " + array->type()->ToString() + ")");
		}
	}

	template <typename ArrowArrayType, typename CType>
	static void read_list_primitive_row(void *handle, const char *name, int64_t row_index, CType *data, int64_t col_size, arrow::Type::type expected_value_type, const char *expected_name, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto vals_any = get_row_list_values(array, name, row_index, col_size);
		report_nulls_list_full(array, vals_any, name, 1, col_size, row_index - 1, valid_out);
		for (int64_t j = 0; j < col_size; ++j)
		{
			data[j] = static_cast<CType>(numeric_value_at(vals_any, name, j));
		}
		fill_null_default(data, valid_out, col_size);
	}

	template <typename ArrowArrayType, typename CType>
	static void read_list_primitive_element(void *handle, const char *name, int64_t col_index, CType *data, int64_t nrows, arrow::Type::type expected_value_type, const char *expected_name, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto col_size = get_col_size(array);
		if (col_index < 1 || col_index > col_size)
		{
			throw std::runtime_error("col_index out of bounds");
		}
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size);
		auto offset = col_index - 1;
		report_nulls_list_element(array, vals_any, name, nrows, col_size, offset, valid_out);
		for (int64_t i = 0; i < nrows; ++i)
		{
			data[i] = static_cast<CType>(numeric_value_at(vals_any, name, i * col_size + offset));
		}
		fill_null_default(data, valid_out, nrows);
	}

extern "C"
{

	void parquet_read_int32_column(void *handle, const char *name, int32_t *data, int64_t nrows, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		if (array->length() != nrows)
		{
			throw std::runtime_error(std::string("nrows mismatch for column: ") + name);
		}
		check_or_report_nulls(array, name, valid_out);

		switch (array->type_id())
		{
		case arrow::Type::INT32:
		{
			auto arr = std::static_pointer_cast<arrow::Int32Array>(array);
			for (int64_t i = 0; i < nrows; ++i)
			{
				data[i] = arr->Value(i);
			}
			break;
		}
		case arrow::Type::INT64:
		{
			auto arr = std::static_pointer_cast<arrow::Int64Array>(array);
			for (int64_t i = 0; i < nrows; ++i)
			{
				auto v = arr->Value(i);
				if (v < std::numeric_limits<int32_t>::min() || v > std::numeric_limits<int32_t>::max())
				{
					throw std::runtime_error(std::string("int64->int32 overflow for column: ") + name);
				}
				data[i] = static_cast<int32_t>(v);
			}
			break;
		}
		default:
			throw std::runtime_error(std::string("type mismatch for column: ") + name +
				" (expected int32/int64, got " + array->type()->ToString() + ")");
		}
		fill_null_default(data, valid_out, nrows);
	}

	void parquet_read_int64_column(void *handle, const char *name, int64_t *data, int64_t nrows, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		if (array->length() != nrows)
		{
			throw std::runtime_error(std::string("nrows mismatch for column: ") + name);
		}
		check_or_report_nulls(array, name, valid_out);

		switch (array->type_id())
		{
		case arrow::Type::INT64:
		{
			auto arr = std::static_pointer_cast<arrow::Int64Array>(array);
			for (int64_t i = 0; i < nrows; ++i)
			{
				data[i] = arr->Value(i);
			}
			break;
		}
		case arrow::Type::INT32:
		{
			auto arr = std::static_pointer_cast<arrow::Int32Array>(array);
			for (int64_t i = 0; i < nrows; ++i)
			{
				data[i] = static_cast<int64_t>(arr->Value(i));
			}
			break;
		}
		default:
			throw std::runtime_error(std::string("type mismatch for column: ") + name +
				" (expected int64/int32, got " + array->type()->ToString() + ")");
		}
		fill_null_default(data, valid_out, nrows);
	}

	void parquet_read_float32_column(void *handle, const char *name, float *data, int64_t nrows, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		if (array->length() != nrows)
		{
			throw std::runtime_error(std::string("nrows mismatch for column: ") + name);
		}
		check_or_report_nulls(array, name, valid_out);

		switch (array->type_id())
		{
		case arrow::Type::FLOAT:
		{
			auto arr = std::static_pointer_cast<arrow::FloatArray>(array);
			for (int64_t i = 0; i < nrows; ++i)
			{
				data[i] = arr->Value(i);
			}
			break;
		}
		case arrow::Type::DOUBLE:
		{
			auto arr = std::static_pointer_cast<arrow::DoubleArray>(array);
			for (int64_t i = 0; i < nrows; ++i)
			{
				data[i] = static_cast<float>(arr->Value(i));
			}
			break;
		}
		case arrow::Type::INT32:
		{
			auto arr = std::static_pointer_cast<arrow::Int32Array>(array);
			for (int64_t i = 0; i < nrows; ++i)
			{
				data[i] = static_cast<float>(arr->Value(i));
			}
			break;
		}
		case arrow::Type::INT64:
		{
			auto arr = std::static_pointer_cast<arrow::Int64Array>(array);
			for (int64_t i = 0; i < nrows; ++i)
			{
				data[i] = static_cast<float>(arr->Value(i));
			}
			break;
		}
		default:
			throw std::runtime_error(std::string("type mismatch for column: ") + name +
				" (expected float32/float64/int32/int64, got " + array->type()->ToString() + ")");
		}
		fill_null_default(data, valid_out, nrows);
	}

	void parquet_read_float64_column(void *handle, const char *name, double *data, int64_t nrows, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		if (array->length() != nrows)
		{
			throw std::runtime_error(std::string("nrows mismatch for column: ") + name);
		}
		check_or_report_nulls(array, name, valid_out);

		switch (array->type_id())
		{
		case arrow::Type::DOUBLE:
		{
			auto arr = std::static_pointer_cast<arrow::DoubleArray>(array);
			for (int64_t i = 0; i < nrows; ++i)
			{
				data[i] = arr->Value(i);
			}
			break;
		}
		case arrow::Type::FLOAT:
		{
			auto arr = std::static_pointer_cast<arrow::FloatArray>(array);
			for (int64_t i = 0; i < nrows; ++i)
			{
				data[i] = static_cast<double>(arr->Value(i));
			}
			break;
		}
		case arrow::Type::INT32:
		{
			auto arr = std::static_pointer_cast<arrow::Int32Array>(array);
			for (int64_t i = 0; i < nrows; ++i)
			{
				data[i] = static_cast<double>(arr->Value(i));
			}
			break;
		}
		case arrow::Type::INT64:
		{
			auto arr = std::static_pointer_cast<arrow::Int64Array>(array);
			for (int64_t i = 0; i < nrows; ++i)
			{
				data[i] = static_cast<double>(arr->Value(i));
			}
			break;
		}
		default:
			throw std::runtime_error(std::string("type mismatch for column: ") + name +
				" (expected float64/float32/int32/int64, got " + array->type()->ToString() + ")");
		}
		fill_null_default(data, valid_out, nrows);
	}

	void parquet_read_bool8_column(void *handle, const char *name, int8_t *data, int64_t nrows, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		if (array->type_id() != arrow::Type::BOOL)
		{
			throw std::runtime_error(std::string("type mismatch for column: ") + name +
				" (expected bool, got " + array->type()->ToString() + ")");
		}
		auto arr = std::static_pointer_cast<arrow::BooleanArray>(array);
		if (arr->length() != nrows)
		{
			throw std::runtime_error(std::string("nrows mismatch for column: ") + name);
		}
		check_or_report_nulls(arr, name, valid_out);
		for (int64_t i = 0; i < nrows; ++i)
		{
			data[i] = arr->Value(i) ? 1 : 0;
		}
		fill_null_default(data, valid_out, nrows);
	}

	void parquet_read_string_column(void *handle, const char *name, char *data, int64_t item_len, int64_t nrows, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		if (array->type_id() != arrow::Type::STRING)
		{
			throw std::runtime_error(std::string("type mismatch for column: ") + name +
				" (expected string, got " + array->type()->ToString() + ")");
		}
		auto arr = std::static_pointer_cast<arrow::StringArray>(array);
		if (arr->length() != nrows)
		{
			throw std::runtime_error(std::string("nrows mismatch for column: ") + name);
		}
		check_or_report_nulls(arr, name, valid_out);
		for (int64_t i = 0; i < nrows; ++i)
		{
			auto view = arr->GetView(i);
			copy_string_with_padding(data + i * item_len, item_len, view);
		}
		fill_null_default_string(data, item_len, valid_out, nrows);
	}

	void parquet_read_int32_array_column(void *handle, const char *name, int32_t *data, int64_t nrows, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size);
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out);
		int64_t total = nrows * col_size;
		switch (vals_any->type_id())
		{
		case arrow::Type::INT32:
		{
			auto vals = std::static_pointer_cast<arrow::Int32Array>(vals_any);
			for (int64_t i = 0; i < total; ++i) data[i] = vals->Value(i);
			break;
		}
		case arrow::Type::INT64:
		{
			auto vals = std::static_pointer_cast<arrow::Int64Array>(vals_any);
			for (int64_t i = 0; i < total; ++i)
			{
				auto v = vals->Value(i);
				if (v < std::numeric_limits<int32_t>::min() || v > std::numeric_limits<int32_t>::max())
				{
					throw std::runtime_error(std::string("int64->int32 overflow for column: ") + name);
				}
				data[i] = static_cast<int32_t>(v);
			}
			break;
		}
		default:
			throw std::runtime_error(std::string("type mismatch for list values in column: ") + name +
				" (expected int32/int64, got " + vals_any->type()->ToString() + ")");
		}
		fill_null_default(data, valid_out, total);
	}

	void parquet_read_int64_array_column(void *handle, const char *name, int64_t *data, int64_t nrows, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size);
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out);
		int64_t total = nrows * col_size;
		switch (vals_any->type_id())
		{
		case arrow::Type::INT64:
		{
			auto vals = std::static_pointer_cast<arrow::Int64Array>(vals_any);
			for (int64_t i = 0; i < total; ++i) data[i] = vals->Value(i);
			break;
		}
		case arrow::Type::INT32:
		{
			auto vals = std::static_pointer_cast<arrow::Int32Array>(vals_any);
			for (int64_t i = 0; i < total; ++i) data[i] = static_cast<int64_t>(vals->Value(i));
			break;
		}
		default:
			throw std::runtime_error(std::string("type mismatch for list values in column: ") + name +
				" (expected int64/int32, got " + vals_any->type()->ToString() + ")");
		}
		fill_null_default(data, valid_out, total);
	}

	void parquet_read_float32_array_column(void *handle, const char *name, float *data, int64_t nrows, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size);
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out);
		int64_t total = nrows * col_size;
		switch (vals_any->type_id())
		{
		case arrow::Type::FLOAT:
		{
			auto vals = std::static_pointer_cast<arrow::FloatArray>(vals_any);
			for (int64_t i = 0; i < total; ++i) data[i] = vals->Value(i);
			break;
		}
		case arrow::Type::DOUBLE:
		{
			auto vals = std::static_pointer_cast<arrow::DoubleArray>(vals_any);
			for (int64_t i = 0; i < total; ++i) data[i] = static_cast<float>(vals->Value(i));
			break;
		}
		case arrow::Type::INT32:
		{
			auto vals = std::static_pointer_cast<arrow::Int32Array>(vals_any);
			for (int64_t i = 0; i < total; ++i) data[i] = static_cast<float>(vals->Value(i));
			break;
		}
		case arrow::Type::INT64:
		{
			auto vals = std::static_pointer_cast<arrow::Int64Array>(vals_any);
			for (int64_t i = 0; i < total; ++i) data[i] = static_cast<float>(vals->Value(i));
			break;
		}
		default:
			throw std::runtime_error(std::string("type mismatch for list values in column: ") + name +
				" (expected float32/float64/int32/int64, got " + vals_any->type()->ToString() + ")");
		}
		fill_null_default(data, valid_out, total);
	}

	void parquet_read_float64_array_column(void *handle, const char *name, double *data, int64_t nrows, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size);
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out);
		int64_t total = nrows * col_size;
		switch (vals_any->type_id())
		{
		case arrow::Type::DOUBLE:
		{
			auto vals = std::static_pointer_cast<arrow::DoubleArray>(vals_any);
			for (int64_t i = 0; i < total; ++i) data[i] = vals->Value(i);
			break;
		}
		case arrow::Type::FLOAT:
		{
			auto vals = std::static_pointer_cast<arrow::FloatArray>(vals_any);
			for (int64_t i = 0; i < total; ++i) data[i] = static_cast<double>(vals->Value(i));
			break;
		}
		case arrow::Type::INT32:
		{
			auto vals = std::static_pointer_cast<arrow::Int32Array>(vals_any);
			for (int64_t i = 0; i < total; ++i) data[i] = static_cast<double>(vals->Value(i));
			break;
		}
		case arrow::Type::INT64:
		{
			auto vals = std::static_pointer_cast<arrow::Int64Array>(vals_any);
			for (int64_t i = 0; i < total; ++i) data[i] = static_cast<double>(vals->Value(i));
			break;
		}
		default:
			throw std::runtime_error(std::string("type mismatch for list values in column: ") + name +
				" (expected float64/float32/int32/int64, got " + vals_any->type()->ToString() + ")");
		}
		fill_null_default(data, valid_out, total);
	}

	void parquet_read_bool8_array_column(void *handle, const char *name, int8_t *data, int64_t nrows, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size);
		if (vals_any->type_id() != arrow::Type::BOOL)
		{
			throw std::runtime_error(std::string("type mismatch for list values in column: ") + name +
				" (expected bool, got " + vals_any->type()->ToString() + ")");
		}
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out);
		auto vals = std::static_pointer_cast<arrow::BooleanArray>(vals_any);
		for (int64_t i = 0; i < nrows * col_size; ++i)
		{
			data[i] = vals->Value(i) ? 1 : 0;
		}
		fill_null_default(data, valid_out, nrows * col_size);
	}

	void parquet_read_string_array_column(void *handle, const char *name, char *data, int64_t item_len, int64_t nrows, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size);
		if (vals_any->type_id() != arrow::Type::STRING)
		{
			throw std::runtime_error(std::string("type mismatch for list values in column: ") + name +
				" (expected string, got " + vals_any->type()->ToString() + ")");
		}
		report_nulls_list_full(array, vals_any, name, nrows, col_size, 0, valid_out);
		auto vals = std::static_pointer_cast<arrow::StringArray>(vals_any);
		for (int64_t i = 0; i < nrows * col_size; ++i)
		{
			copy_string_with_padding(data + i * item_len, item_len, vals->GetView(i));
		}
		fill_null_default_string(data, item_len, valid_out, nrows * col_size);
	}

	void parquet_read_int32_array_row(void *handle, const char *name, int64_t row_index, int32_t *data, int64_t col_size, int8_t *valid_out)
	{
		read_list_primitive_row<arrow::Int32Array, int32_t>(handle, name, row_index, data, col_size, arrow::Type::INT32, "int32", valid_out);
	}

	void parquet_read_int64_array_row(void *handle, const char *name, int64_t row_index, int64_t *data, int64_t col_size, int8_t *valid_out)
	{
		read_list_primitive_row<arrow::Int64Array, int64_t>(handle, name, row_index, data, col_size, arrow::Type::INT64, "int64", valid_out);
	}

	void parquet_read_float32_array_row(void *handle, const char *name, int64_t row_index, float *data, int64_t col_size, int8_t *valid_out)
	{
		read_list_primitive_row<arrow::FloatArray, float>(handle, name, row_index, data, col_size, arrow::Type::FLOAT, "float32", valid_out);
	}

	void parquet_read_float64_array_row(void *handle, const char *name, int64_t row_index, double *data, int64_t col_size, int8_t *valid_out)
	{
		read_list_primitive_row<arrow::DoubleArray, double>(handle, name, row_index, data, col_size, arrow::Type::DOUBLE, "float64", valid_out);
	}

	void parquet_read_bool8_array_row(void *handle, const char *name, int64_t row_index, int8_t *data, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto vals_any = get_row_list_values(array, name, row_index, col_size);
		if (vals_any->type_id() != arrow::Type::BOOL)
			throw std::runtime_error(std::string("type mismatch for list values in column: ") + name +
				" (expected bool, got " + vals_any->type()->ToString() + ")");
		report_nulls_list_full(array, vals_any, name, 1, col_size, row_index - 1, valid_out);

		auto vals = std::static_pointer_cast<arrow::BooleanArray>(vals_any);
		for (int64_t j = 0; j < col_size; ++j)
		{
			data[j] = vals->Value(j) ? 1 : 0;
		}
		fill_null_default(data, valid_out, col_size);
	}

	void parquet_read_string_array_row(void *handle, const char *name, int64_t row_index, char *data, int64_t item_len, int64_t col_size, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto vals_any = get_row_list_values(array, name, row_index, col_size);
		if (vals_any->type_id() != arrow::Type::STRING)
			throw std::runtime_error(std::string("type mismatch for list values in column: ") + name +
				" (expected string, got " + vals_any->type()->ToString() + ")");
		report_nulls_list_full(array, vals_any, name, 1, col_size, row_index - 1, valid_out);

		auto vals = std::static_pointer_cast<arrow::StringArray>(vals_any);
		for (int64_t j = 0; j < col_size; ++j)
		{
			copy_string_with_padding(data + j * item_len, item_len, vals->GetView(j));
		}
		fill_null_default_string(data, item_len, valid_out, col_size);
	}

	void parquet_read_int32_array_element(void *handle, const char *name, int64_t col_index, int32_t *data, int64_t nrows, int64_t unused_col_size, int8_t *valid_out)
	{
		read_list_primitive_element<arrow::Int32Array, int32_t>(handle, name, col_index, data, nrows, arrow::Type::INT32, "int32", valid_out);
	}

	void parquet_read_int64_array_element(void *handle, const char *name, int64_t col_index, int64_t *data, int64_t nrows, int64_t unused_col_size, int8_t *valid_out)
	{
		read_list_primitive_element<arrow::Int64Array, int64_t>(handle, name, col_index, data, nrows, arrow::Type::INT64, "int64", valid_out);
	}

	void parquet_read_float32_array_element(void *handle, const char *name, int64_t col_index, float *data, int64_t nrows, int64_t unused_col_size, int8_t *valid_out)
	{
		read_list_primitive_element<arrow::FloatArray, float>(handle, name, col_index, data, nrows, arrow::Type::FLOAT, "float32", valid_out);
	}

	void parquet_read_float64_array_element(void *handle, const char *name, int64_t col_index, double *data, int64_t nrows, int64_t unused_col_size, int8_t *valid_out)
	{
		read_list_primitive_element<arrow::DoubleArray, double>(handle, name, col_index, data, nrows, arrow::Type::DOUBLE, "float64", valid_out);
	}

	void parquet_read_bool8_array_element(void *handle, const char *name, int64_t col_index, int8_t *data, int64_t nrows, int64_t, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto col_size = get_col_size(array);
		if (col_index < 1 || col_index > col_size)
			throw std::runtime_error("col_index out of bounds");
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size);
		if (vals_any->type_id() != arrow::Type::BOOL)
			throw std::runtime_error(std::string("type mismatch for list values in column: ") + name +
				" (expected bool, got " + vals_any->type()->ToString() + ")");
		auto offset = col_index - 1;
		report_nulls_list_element(array, vals_any, name, nrows, col_size, offset, valid_out);

		auto vals = std::static_pointer_cast<arrow::BooleanArray>(vals_any);
		for (int64_t i = 0; i < nrows; ++i)
		{
			data[i] = vals->Value(i * col_size + offset) ? 1 : 0;
		}
		fill_null_default(data, valid_out, nrows);
	}

	void parquet_read_string_array_element(void *handle, const char *name, int64_t col_index, char *data, int64_t item_len, int64_t nrows, int64_t, int8_t *valid_out)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto col_size = get_col_size(array);
		if (col_index < 1 || col_index > col_size)
			throw std::runtime_error("col_index out of bounds");
		auto vals_any = get_uniform_list_values(array, name, nrows, col_size);
		if (vals_any->type_id() != arrow::Type::STRING)
			throw std::runtime_error(std::string("type mismatch for list values in column: ") + name +
				" (expected string, got " + vals_any->type()->ToString() + ")");
		auto offset = col_index - 1;
		report_nulls_list_element(array, vals_any, name, nrows, col_size, offset, valid_out);

		auto vals = std::static_pointer_cast<arrow::StringArray>(vals_any);
		for (int64_t i = 0; i < nrows; ++i)
		{
			copy_string_with_padding(data + i * item_len, item_len, vals->GetView(i * col_size + offset));
		}
		fill_null_default_string(data, item_len, valid_out, nrows);
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

	void parquet_append_int32_column(void *handle, const char *name, const int32_t *data, int64_t nrows, int64_t col_size, const int8_t *valid_in)
	{
		auto writer_handle = as_handle(handle);

		std::shared_ptr<arrow::Array> array;
		auto value_type = arrow::int32();
		auto valid_bytes = reinterpret_cast<const uint8_t *>(valid_in);

		if (col_size > 1)
		{
			auto value_builder = std::make_shared<arrow::Int32Builder>();
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
			arrow::Int32Builder builder;
			auto status = builder.AppendValues(data, nrows, valid_bytes);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
			status = builder.Finish(&array);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
		}

		append_column(writer_handle, name, build_field(name, value_type, col_size, has_any_null(valid_in, nrows * col_size)), array);
	}

	void parquet_append_int64_column(void *handle, const char *name, const int64_t *data, int64_t nrows, int64_t col_size, const int8_t *valid_in)
	{
		auto writer_handle = as_handle(handle);

		std::shared_ptr<arrow::Array> array;
		auto value_type = arrow::int64();
		auto valid_bytes = reinterpret_cast<const uint8_t *>(valid_in);

		if (col_size > 1)
		{
			auto value_builder = std::make_shared<arrow::Int64Builder>();
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
			arrow::Int64Builder builder;
			auto status = builder.AppendValues(data, nrows, valid_bytes);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
			status = builder.Finish(&array);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
		}

		append_column(writer_handle, name, build_field(name, value_type, col_size, has_any_null(valid_in, nrows * col_size)), array);
	}

	void parquet_append_float32_column(void *handle, const char *name, const float *data, int64_t nrows, int64_t col_size, const int8_t *valid_in)
	{
		auto writer_handle = as_handle(handle);

		std::shared_ptr<arrow::Array> array;
		auto value_type = arrow::float32();
		auto valid_bytes = reinterpret_cast<const uint8_t *>(valid_in);

		if (col_size > 1)
		{
			auto value_builder = std::make_shared<arrow::FloatBuilder>();
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
			arrow::FloatBuilder builder;
			auto status = builder.AppendValues(data, nrows, valid_bytes);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
			status = builder.Finish(&array);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
		}

		append_column(writer_handle, name, build_field(name, value_type, col_size, has_any_null(valid_in, nrows * col_size)), array);
	}

	void parquet_append_float64_column(void *handle, const char *name, const double *data, int64_t nrows, int64_t col_size, const int8_t *valid_in)
	{
		auto writer_handle = as_handle(handle);

		std::shared_ptr<arrow::Array> array;
		auto value_type = arrow::float64();
		auto valid_bytes = reinterpret_cast<const uint8_t *>(valid_in);

		if (col_size > 1)
		{
			auto value_builder = std::make_shared<arrow::DoubleBuilder>();
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
			arrow::DoubleBuilder builder;
			auto status = builder.AppendValues(data, nrows, valid_bytes);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
			status = builder.Finish(&array);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
		}

		append_column(writer_handle, name, build_field(name, value_type, col_size, has_any_null(valid_in, nrows * col_size)), array);
	}

	void parquet_append_bool8_column(void *handle, const char *name, const int8_t *data, int64_t nrows, int64_t col_size, const int8_t *valid_in)
	{
		auto writer_handle = as_handle(handle);

		std::shared_ptr<arrow::Array> array;
		auto value_type = arrow::boolean();
		auto valid_bytes = reinterpret_cast<const uint8_t *>(valid_in);
		auto values_bytes = reinterpret_cast<const uint8_t *>(data);

		if (col_size > 1)
		{
			auto value_builder = std::make_shared<arrow::BooleanBuilder>();
			arrow::FixedSizeListBuilder list_builder(arrow::default_memory_pool(), value_builder, static_cast<int32_t>(col_size));
			auto status = list_builder.AppendValues(nrows);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
			status = value_builder->AppendValues(values_bytes, nrows * col_size, valid_bytes);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
			status = list_builder.Finish(&array);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
		}
		else
		{
			arrow::BooleanBuilder builder;
			auto status = builder.AppendValues(values_bytes, nrows, valid_bytes);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
			status = builder.Finish(&array);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
		}

		append_column(writer_handle, name, build_field(name, value_type, col_size, has_any_null(valid_in, nrows * col_size)), array);
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
		// final row count is known, rather than using a single fixed
		// constant regardless of file size. One row group is enough for a
		// small table (avoids the per-row-group overhead -- its own footer
		// entry, dictionary reset, compression context reset -- of splitting
		// a small file into many tiny groups for no benefit); a large table
		// is capped at kAutoChunkSizeCap rows per group so it doesn't end up
		// as one enormous row group either.
		auto effective_chunk_size = writer_handle->chunk_size;
		if (effective_chunk_size <= 0)
		{
			static constexpr int64_t kAutoChunkSizeCap = 500000;
			effective_chunk_size = std::min(table->num_rows(), kAutoChunkSizeCap);
			if (effective_chunk_size < 1) effective_chunk_size = 1;
		}

		parquet::ArrowWriterProperties::Builder arrow_writer_builder;
		arrow_writer_builder.store_schema();
		// See create_parquet_reader for why this defaults to true (Arrow's own
		// default is false); callers can opt out via
		// parquet_open_writer(..., use_threads=.false.).
		arrow_writer_builder.set_use_threads(writer_handle->use_threads);
		auto arrow_writer_properties = arrow_writer_builder.build();
		auto writer_properties = parquet::WriterProperties::Builder()
			.compression(writer_handle->compression_codec)
			->compression_level(writer_handle->compression_level)
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
