#include <arrow/api.h>
#include <arrow/io/api.h>
#include <parquet/arrow/reader.h>
#include <parquet/arrow/writer.h>

#include <ctime>
#include <cstring>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

extern "C"
{
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

	struct ParquetWriterHandle
	{
		std::shared_ptr<arrow::io::FileOutputStream> outfile;
		std::vector<std::shared_ptr<arrow::Field>> fields;
		std::vector<std::shared_ptr<arrow::Array>> arrays;
		std::vector<ColumnMetadata> column_metadata;
		std::vector<std::pair<std::string, std::string>> table_metadata;
	};

	struct ParquetReaderHandle
	{
		std::shared_ptr<arrow::Table> table;
	};

	static ParquetWriterHandle *as_handle(void *handle)
	{
		return static_cast<ParquetWriterHandle *>(handle);
	}

	static ParquetReaderHandle *as_reader_handle(void *handle)
	{
		return static_cast<ParquetReaderHandle *>(handle);
	}

	static int64_t get_column_index(const ParquetReaderHandle *reader_handle, const char *name)
	{
		auto idx = reader_handle->table->schema()->GetFieldIndex(name);
		if (idx < 0)
		{
			throw std::runtime_error(std::string("Column not found: ") + name);
		}
		return static_cast<int64_t>(idx);
	}

	static std::shared_ptr<arrow::Array> get_single_chunk_array(const ParquetReaderHandle *reader_handle, const char *name)
	{
		auto idx = get_column_index(reader_handle, name);
		auto chunked = reader_handle->table->column(static_cast<int>(idx));
		if (chunked->num_chunks() != 1)
		{
			throw std::runtime_error(std::string("Expected one chunk for column: ") + name);
		}
		return chunked->chunk(0);
	}

	static int64_t get_array_size(const std::shared_ptr<arrow::Array> &array)
	{
		if (array->type_id() == arrow::Type::FIXED_SIZE_LIST)
		{
			auto list_arr = std::static_pointer_cast<arrow::FixedSizeListArray>(array);
			return static_cast<int64_t>(list_arr->value_length());
		}
		return 1;
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
									const std::vector<std::pair<std::string, std::string>> &table_metadata,
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
				<< xml_escape(kv.first)
				<< "\" value=\""
				<< xml_escape(kv.second)
				<< "\"/>\n";
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

	static std::shared_ptr<arrow::Field> build_field(
		const std::string &name,
		const std::shared_ptr<arrow::DataType> &value_type,
		int64_t array_size)
	{
		if (array_size > 1)
		{
			return arrow::field(name, arrow::fixed_size_list(value_type, static_cast<int32_t>(array_size)), false);
		}
		return arrow::field(name, value_type, false);
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
		const std::vector<std::pair<std::string, std::string>> &table_metadata)
	{
		auto date = current_utc_timestamp();
		auto votable_xml = build_votable_xml("table", column_metadata, table_metadata, date);

		std::vector<std::string> keys{
			"IVOA.VOTable-Parquet.content",
			"IVOA.VOTable-Parquet.version",
			"DATE",
			"name"};
		std::vector<std::string> values{votable_xml, "1.0", date, "table"};

		for (const auto &kv : table_metadata)
		{
			keys.push_back(kv.first);
			values.push_back(kv.second);
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

	void *create_parquet_reader(const char *filename)
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
		std::unique_ptr<parquet::arrow::FileReader> reader;
		status = builder.Build(&reader);
		if (!status.ok())
		{
			delete handle;
			throw std::runtime_error(status.ToString());
		}

		status = reader->ReadTable(&handle->table);
		if (!status.ok())
		{
			delete handle;
			throw std::runtime_error(status.ToString());
		}

		auto combined = handle->table->CombineChunks(arrow::default_memory_pool());
		if (!combined.ok())
		{
			delete handle;
			throw std::runtime_error(combined.status().ToString());
		}
		handle->table = combined.ValueOrDie();
		return handle;
	}

	void close_parquet_reader(void *handle)
	{
		auto reader_handle = as_reader_handle(handle);
		delete reader_handle;
	}

	int64_t parquet_reader_get_nrows(void *handle)
	{
		auto reader_handle = as_reader_handle(handle);
		return reader_handle->table->num_rows();
	}

	int64_t parquet_reader_get_column_array_size(void *handle, const char *name)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		return get_array_size(array);
	}

	int64_t parquet_reader_get_column_total_elements(void *handle, const char *name)
	{
		auto reader_handle = as_reader_handle(handle);
		auto nrows = reader_handle->table->num_rows();
		auto asize = parquet_reader_get_column_array_size(handle, name);
		return nrows * asize;
	}

	int64_t parquet_reader_get_string_length(void *handle, const char *name)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		int64_t max_len = 0;

		if (array->type_id() == arrow::Type::STRING)
		{
			auto sarr = std::static_pointer_cast<arrow::StringArray>(array);
			for (int64_t i = 0; i < sarr->length(); ++i)
			{
				if (sarr->IsNull(i))
					continue;
				max_len = std::max(max_len, static_cast<int64_t>(sarr->GetView(i).size()));
			}
			return max_len;
		}

		if (array->type_id() == arrow::Type::FIXED_SIZE_LIST)
		{
			auto larr = std::static_pointer_cast<arrow::FixedSizeListArray>(array);
			auto vals = std::static_pointer_cast<arrow::StringArray>(larr->values());
			for (int64_t i = 0; i < vals->length(); ++i)
			{
				if (vals->IsNull(i))
					continue;
				max_len = std::max(max_len, static_cast<int64_t>(vals->GetView(i).size()));
			}
			return max_len;
		}

		throw std::runtime_error(std::string("Column is not string-like: ") + name);
	}

}

	template <typename ArrowArrayType, typename CType>
	static void read_scalar_primitive(void *handle, const char *name, CType *data, int64_t nrows)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto arr = std::static_pointer_cast<ArrowArrayType>(array);
		if (arr->length() != nrows)
		{
			throw std::runtime_error(std::string("nrows mismatch for column: ") + name);
		}
		for (int64_t i = 0; i < nrows; ++i)
		{
			data[i] = static_cast<CType>(arr->Value(i));
		}
	}

	template <typename ArrowArrayType, typename CType>
	static void read_list_primitive_full(void *handle, const char *name, CType *data, int64_t nrows, int64_t array_size)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto list_arr = std::static_pointer_cast<arrow::FixedSizeListArray>(array);
		auto value_len = static_cast<int64_t>(list_arr->value_length());
		if (value_len != array_size)
		{
			throw std::runtime_error(std::string("array_size mismatch for column: ") + name);
		}
		if (list_arr->length() != nrows)
		{
			throw std::runtime_error(std::string("nrows mismatch for column: ") + name);
		}
		auto vals = std::static_pointer_cast<ArrowArrayType>(list_arr->values());
		for (int64_t i = 0; i < nrows * array_size; ++i)
		{
			data[i] = static_cast<CType>(vals->Value(i));
		}
	}

	template <typename ArrowArrayType, typename CType>
	static void read_list_primitive_row(void *handle, const char *name, int64_t row_index, CType *data, int64_t array_size)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto list_arr = std::static_pointer_cast<arrow::FixedSizeListArray>(array);
		auto nrows = list_arr->length();
		if (row_index < 1 || row_index > nrows)
		{
			throw std::runtime_error("row_index out of bounds");
		}
		auto value_len = static_cast<int64_t>(list_arr->value_length());
		if (value_len != array_size)
		{
			throw std::runtime_error(std::string("array_size mismatch for column: ") + name);
		}
		auto vals = std::static_pointer_cast<ArrowArrayType>(list_arr->values());
		auto start = (row_index - 1) * array_size;
		for (int64_t j = 0; j < array_size; ++j)
		{
			data[j] = static_cast<CType>(vals->Value(start + j));
		}
	}

	template <typename ArrowArrayType, typename CType>
	static void read_list_primitive_element(void *handle, const char *name, int64_t col_index, CType *data, int64_t nrows)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto list_arr = std::static_pointer_cast<arrow::FixedSizeListArray>(array);
		auto array_size = static_cast<int64_t>(list_arr->value_length());
		if (col_index < 1 || col_index > array_size)
		{
			throw std::runtime_error("col_index out of bounds");
		}
		if (list_arr->length() != nrows)
		{
			throw std::runtime_error(std::string("nrows mismatch for column: ") + name);
		}
		auto vals = std::static_pointer_cast<ArrowArrayType>(list_arr->values());
		auto offset = col_index - 1;
		for (int64_t i = 0; i < nrows; ++i)
		{
			data[i] = static_cast<CType>(vals->Value(i * array_size + offset));
		}
	}

extern "C"
{

	void parquet_read_int32_column(void *handle, const char *name, int32_t *data, int64_t nrows)
	{
		read_scalar_primitive<arrow::Int32Array, int32_t>(handle, name, data, nrows);
	}

	void parquet_read_int64_column(void *handle, const char *name, int64_t *data, int64_t nrows)
	{
		read_scalar_primitive<arrow::Int64Array, int64_t>(handle, name, data, nrows);
	}

	void parquet_read_float32_column(void *handle, const char *name, float *data, int64_t nrows)
	{
		read_scalar_primitive<arrow::FloatArray, float>(handle, name, data, nrows);
	}

	void parquet_read_float64_column(void *handle, const char *name, double *data, int64_t nrows)
	{
		read_scalar_primitive<arrow::DoubleArray, double>(handle, name, data, nrows);
	}

	void parquet_read_bool8_column(void *handle, const char *name, int8_t *data, int64_t nrows)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto arr = std::static_pointer_cast<arrow::BooleanArray>(array);
		if (arr->length() != nrows)
		{
			throw std::runtime_error(std::string("nrows mismatch for column: ") + name);
		}
		for (int64_t i = 0; i < nrows; ++i)
		{
			data[i] = arr->Value(i) ? 1 : 0;
		}
	}

	void parquet_read_string_column(void *handle, const char *name, char *data, int64_t item_len, int64_t nrows)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto arr = std::static_pointer_cast<arrow::StringArray>(array);
		if (arr->length() != nrows)
		{
			throw std::runtime_error(std::string("nrows mismatch for column: ") + name);
		}
		for (int64_t i = 0; i < nrows; ++i)
		{
			auto view = arr->GetView(i);
			copy_string_with_padding(data + i * item_len, item_len, view);
		}
	}

	void parquet_read_int32_array_column(void *handle, const char *name, int32_t *data, int64_t nrows, int64_t array_size)
	{
		read_list_primitive_full<arrow::Int32Array, int32_t>(handle, name, data, nrows, array_size);
	}

	void parquet_read_int64_array_column(void *handle, const char *name, int64_t *data, int64_t nrows, int64_t array_size)
	{
		read_list_primitive_full<arrow::Int64Array, int64_t>(handle, name, data, nrows, array_size);
	}

	void parquet_read_float32_array_column(void *handle, const char *name, float *data, int64_t nrows, int64_t array_size)
	{
		read_list_primitive_full<arrow::FloatArray, float>(handle, name, data, nrows, array_size);
	}

	void parquet_read_float64_array_column(void *handle, const char *name, double *data, int64_t nrows, int64_t array_size)
	{
		read_list_primitive_full<arrow::DoubleArray, double>(handle, name, data, nrows, array_size);
	}

	void parquet_read_bool8_array_column(void *handle, const char *name, int8_t *data, int64_t nrows, int64_t array_size)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto list_arr = std::static_pointer_cast<arrow::FixedSizeListArray>(array);
		if (list_arr->length() != nrows || static_cast<int64_t>(list_arr->value_length()) != array_size)
		{
			throw std::runtime_error(std::string("shape mismatch for column: ") + name);
		}
		auto vals = std::static_pointer_cast<arrow::BooleanArray>(list_arr->values());
		for (int64_t i = 0; i < nrows * array_size; ++i)
		{
			data[i] = vals->Value(i) ? 1 : 0;
		}
	}

	void parquet_read_string_array_column(void *handle, const char *name, char *data, int64_t item_len, int64_t nrows, int64_t array_size)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto list_arr = std::static_pointer_cast<arrow::FixedSizeListArray>(array);
		if (list_arr->length() != nrows || static_cast<int64_t>(list_arr->value_length()) != array_size)
		{
			throw std::runtime_error(std::string("shape mismatch for column: ") + name);
		}
		auto vals = std::static_pointer_cast<arrow::StringArray>(list_arr->values());
		for (int64_t i = 0; i < nrows * array_size; ++i)
		{
			copy_string_with_padding(data + i * item_len, item_len, vals->GetView(i));
		}
	}

	void parquet_read_int32_array_row(void *handle, const char *name, int64_t row_index, int32_t *data, int64_t array_size)
	{
		read_list_primitive_row<arrow::Int32Array, int32_t>(handle, name, row_index, data, array_size);
	}

	void parquet_read_int64_array_row(void *handle, const char *name, int64_t row_index, int64_t *data, int64_t array_size)
	{
		read_list_primitive_row<arrow::Int64Array, int64_t>(handle, name, row_index, data, array_size);
	}

	void parquet_read_float32_array_row(void *handle, const char *name, int64_t row_index, float *data, int64_t array_size)
	{
		read_list_primitive_row<arrow::FloatArray, float>(handle, name, row_index, data, array_size);
	}

	void parquet_read_float64_array_row(void *handle, const char *name, int64_t row_index, double *data, int64_t array_size)
	{
		read_list_primitive_row<arrow::DoubleArray, double>(handle, name, row_index, data, array_size);
	}

	void parquet_read_bool8_array_row(void *handle, const char *name, int64_t row_index, int8_t *data, int64_t array_size)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto list_arr = std::static_pointer_cast<arrow::FixedSizeListArray>(array);
		auto nrows = list_arr->length();
		if (row_index < 1 || row_index > nrows)
			throw std::runtime_error("row_index out of bounds");
		if (static_cast<int64_t>(list_arr->value_length()) != array_size)
			throw std::runtime_error(std::string("array_size mismatch for column: ") + name);
		auto vals = std::static_pointer_cast<arrow::BooleanArray>(list_arr->values());
		auto start = (row_index - 1) * array_size;
		for (int64_t j = 0; j < array_size; ++j)
		{
			data[j] = vals->Value(start + j) ? 1 : 0;
		}
	}

	void parquet_read_string_array_row(void *handle, const char *name, int64_t row_index, char *data, int64_t item_len, int64_t array_size)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto list_arr = std::static_pointer_cast<arrow::FixedSizeListArray>(array);
		auto nrows = list_arr->length();
		if (row_index < 1 || row_index > nrows)
			throw std::runtime_error("row_index out of bounds");
		if (static_cast<int64_t>(list_arr->value_length()) != array_size)
			throw std::runtime_error(std::string("array_size mismatch for column: ") + name);
		auto vals = std::static_pointer_cast<arrow::StringArray>(list_arr->values());
		auto start = (row_index - 1) * array_size;
		for (int64_t j = 0; j < array_size; ++j)
		{
			copy_string_with_padding(data + j * item_len, item_len, vals->GetView(start + j));
		}
	}

	void parquet_read_int32_array_element(void *handle, const char *name, int64_t col_index, int32_t *data, int64_t nrows, int64_t)
	{
		read_list_primitive_element<arrow::Int32Array, int32_t>(handle, name, col_index, data, nrows);
	}

	void parquet_read_int64_array_element(void *handle, const char *name, int64_t col_index, int64_t *data, int64_t nrows, int64_t)
	{
		read_list_primitive_element<arrow::Int64Array, int64_t>(handle, name, col_index, data, nrows);
	}

	void parquet_read_float32_array_element(void *handle, const char *name, int64_t col_index, float *data, int64_t nrows, int64_t)
	{
		read_list_primitive_element<arrow::FloatArray, float>(handle, name, col_index, data, nrows);
	}

	void parquet_read_float64_array_element(void *handle, const char *name, int64_t col_index, double *data, int64_t nrows, int64_t)
	{
		read_list_primitive_element<arrow::DoubleArray, double>(handle, name, col_index, data, nrows);
	}

	void parquet_read_bool8_array_element(void *handle, const char *name, int64_t col_index, int8_t *data, int64_t nrows, int64_t)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto list_arr = std::static_pointer_cast<arrow::FixedSizeListArray>(array);
		auto array_size = static_cast<int64_t>(list_arr->value_length());
		if (col_index < 1 || col_index > array_size)
			throw std::runtime_error("col_index out of bounds");
		if (list_arr->length() != nrows)
			throw std::runtime_error(std::string("nrows mismatch for column: ") + name);
		auto vals = std::static_pointer_cast<arrow::BooleanArray>(list_arr->values());
		auto offset = col_index - 1;
		for (int64_t i = 0; i < nrows; ++i)
		{
			data[i] = vals->Value(i * array_size + offset) ? 1 : 0;
		}
	}

	void parquet_read_string_array_element(void *handle, const char *name, int64_t col_index, char *data, int64_t item_len, int64_t nrows, int64_t)
	{
		auto reader_handle = as_reader_handle(handle);
		auto array = get_single_chunk_array(reader_handle, name);
		auto list_arr = std::static_pointer_cast<arrow::FixedSizeListArray>(array);
		auto array_size = static_cast<int64_t>(list_arr->value_length());
		if (col_index < 1 || col_index > array_size)
			throw std::runtime_error("col_index out of bounds");
		if (list_arr->length() != nrows)
			throw std::runtime_error(std::string("nrows mismatch for column: ") + name);
		auto vals = std::static_pointer_cast<arrow::StringArray>(list_arr->values());
		auto offset = col_index - 1;
		for (int64_t i = 0; i < nrows; ++i)
		{
			copy_string_with_padding(data + i * item_len, item_len, vals->GetView(i * array_size + offset));
		}
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

	void parquet_add_table_metadata(void *handle, const char *key, const char *value)
	{
		auto writer_handle = as_handle(handle);
		writer_handle->table_metadata.emplace_back(
			std::string(key),
			std::string(value));
	}

	void parquet_append_int32_column(void *handle, const char *name, const int32_t *data, int64_t nrows, int64_t array_size)
	{
		auto writer_handle = as_handle(handle);

		std::shared_ptr<arrow::Array> array;
		auto value_type = arrow::int32();

		if (array_size > 1)
		{
			auto value_builder = std::make_shared<arrow::Int32Builder>();
			arrow::FixedSizeListBuilder list_builder(arrow::default_memory_pool(), value_builder, static_cast<int32_t>(array_size));
			auto status = list_builder.AppendValues(nrows);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
			status = value_builder->AppendValues(data, nrows * array_size);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
			status = list_builder.Finish(&array);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
		}
		else
		{
			arrow::Int32Builder builder;
			auto status = builder.AppendValues(data, nrows);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
			status = builder.Finish(&array);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
		}

		append_column(writer_handle, name, build_field(name, value_type, array_size), array);
	}

	void parquet_append_int64_column(void *handle, const char *name, const int64_t *data, int64_t nrows, int64_t array_size)
	{
		auto writer_handle = as_handle(handle);

		std::shared_ptr<arrow::Array> array;
		auto value_type = arrow::int64();

		if (array_size > 1)
		{
			auto value_builder = std::make_shared<arrow::Int64Builder>();
			arrow::FixedSizeListBuilder list_builder(arrow::default_memory_pool(), value_builder, static_cast<int32_t>(array_size));
			auto status = list_builder.AppendValues(nrows);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
			status = value_builder->AppendValues(data, nrows * array_size);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
			status = list_builder.Finish(&array);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
		}
		else
		{
			arrow::Int64Builder builder;
			auto status = builder.AppendValues(data, nrows);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
			status = builder.Finish(&array);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
		}

		append_column(writer_handle, name, build_field(name, value_type, array_size), array);
	}

	void parquet_append_float32_column(void *handle, const char *name, const float *data, int64_t nrows, int64_t array_size)
	{
		auto writer_handle = as_handle(handle);

		std::shared_ptr<arrow::Array> array;
		auto value_type = arrow::float32();

		if (array_size > 1)
		{
			auto value_builder = std::make_shared<arrow::FloatBuilder>();
			arrow::FixedSizeListBuilder list_builder(arrow::default_memory_pool(), value_builder, static_cast<int32_t>(array_size));
			auto status = list_builder.AppendValues(nrows);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
			status = value_builder->AppendValues(data, nrows * array_size);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
			status = list_builder.Finish(&array);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
		}
		else
		{
			arrow::FloatBuilder builder;
			auto status = builder.AppendValues(data, nrows);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
			status = builder.Finish(&array);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
		}

		append_column(writer_handle, name, build_field(name, value_type, array_size), array);
	}

	void parquet_append_float64_column(void *handle, const char *name, const double *data, int64_t nrows, int64_t array_size)
	{
		auto writer_handle = as_handle(handle);

		std::shared_ptr<arrow::Array> array;
		auto value_type = arrow::float64();

		if (array_size > 1)
		{
			auto value_builder = std::make_shared<arrow::DoubleBuilder>();
			arrow::FixedSizeListBuilder list_builder(arrow::default_memory_pool(), value_builder, static_cast<int32_t>(array_size));
			auto status = list_builder.AppendValues(nrows);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
			status = value_builder->AppendValues(data, nrows * array_size);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
			status = list_builder.Finish(&array);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
		}
		else
		{
			arrow::DoubleBuilder builder;
			auto status = builder.AppendValues(data, nrows);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
			status = builder.Finish(&array);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
		}

		append_column(writer_handle, name, build_field(name, value_type, array_size), array);
	}

	void parquet_append_bool8_column(void *handle, const char *name, const int8_t *data, int64_t nrows, int64_t array_size)
	{
		auto writer_handle = as_handle(handle);

		std::shared_ptr<arrow::Array> array;
		auto value_type = arrow::boolean();

		if (array_size > 1)
		{
			auto value_builder = std::make_shared<arrow::BooleanBuilder>();
			arrow::FixedSizeListBuilder list_builder(arrow::default_memory_pool(), value_builder, static_cast<int32_t>(array_size));
			auto status = list_builder.AppendValues(nrows);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
			for (int64_t i = 0; i < nrows * array_size; ++i)
			{
				status = value_builder->Append(data[i] != 0);
				if (!status.ok())
					throw std::runtime_error(status.ToString());
			}
			status = list_builder.Finish(&array);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
		}
		else
		{
			arrow::BooleanBuilder builder;
			auto status = arrow::Status::OK();
			for (int64_t i = 0; i < nrows; ++i)
			{
				status = builder.Append(data[i] != 0);
				if (!status.ok())
					throw std::runtime_error(status.ToString());
			}
			status = builder.Finish(&array);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
		}

		append_column(writer_handle, name, build_field(name, value_type, array_size), array);
	}

	void parquet_append_string_column(void *handle, const char *name, const char *data, int64_t item_len, int64_t nrows)
	{
		auto writer_handle = as_handle(handle);
		arrow::StringBuilder builder;

		auto status = arrow::Status::OK();
		for (int64_t i = 0; i < nrows; ++i)
		{
			const char *raw = data + i * item_len;
			std::string value(raw, static_cast<size_t>(item_len));
			value = trim_right_spaces_and_nuls(value);
			status = builder.Append(value);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
		}

		std::shared_ptr<arrow::Array> array;
		status = builder.Finish(&array);
		if (!status.ok())
			throw std::runtime_error(status.ToString());

		append_column(writer_handle, name, build_field(name, arrow::utf8(), 1), array);
	}

	void parquet_append_string_array_column(void *handle, const char *name, const char *data, int64_t item_len, int64_t nrows, int64_t array_size)
	{
		auto writer_handle = as_handle(handle);
		auto value_builder = std::make_shared<arrow::StringBuilder>();
		arrow::FixedSizeListBuilder list_builder(arrow::default_memory_pool(), value_builder, static_cast<int32_t>(array_size));

		auto status = list_builder.AppendValues(nrows);
		if (!status.ok())
			throw std::runtime_error(status.ToString());

		for (int64_t i = 0; i < nrows * array_size; ++i)
		{
			const char *raw = data + i * item_len;
			std::string value(raw, static_cast<size_t>(item_len));
			value = trim_right_spaces_and_nuls(value);
			status = value_builder->Append(value);
			if (!status.ok())
				throw std::runtime_error(status.ToString());
		}

		std::shared_ptr<arrow::Array> array;
		status = list_builder.Finish(&array);
		if (!status.ok())
			throw std::runtime_error(status.ToString());

		append_column(writer_handle, name, build_field(name, arrow::utf8(), array_size), array);
	}


	void close_parquet_writer(void *handle)
	{
		auto writer_handle = as_handle(handle);

		if (!writer_handle->column_metadata.empty())
		{
			if (writer_handle->fields.size() != writer_handle->column_metadata.size() ||
				writer_handle->arrays.size() != writer_handle->column_metadata.size())
			{
				delete writer_handle;
				throw std::runtime_error("Internal error: schema/data size mismatch before close");
			}

			for (size_t i = 0; i < writer_handle->column_metadata.size(); ++i)
			{
				if (!writer_handle->fields[i] || !writer_handle->arrays[i])
				{
					auto missing = writer_handle->column_metadata[i].name;
					delete writer_handle;
					throw std::runtime_error("Missing column data before close: " + missing);
				}
			}
		}

		auto metadata = build_file_metadata(writer_handle->column_metadata, writer_handle->table_metadata);
		auto schema = arrow::schema(writer_handle->fields, metadata);
		auto table = arrow::Table::Make(schema, writer_handle->arrays);

		parquet::ArrowWriterProperties::Builder arrow_writer_builder;
		arrow_writer_builder.store_schema();
		auto arrow_writer_properties = arrow_writer_builder.build();
		auto writer_properties = parquet::WriterProperties::Builder().build();

		auto status = parquet::arrow::WriteTable(
			*table,
			arrow::default_memory_pool(),
			writer_handle->outfile,
			1024,
			writer_properties,
			arrow_writer_properties);
		if (!status.ok())
		{
			delete writer_handle;
			throw std::runtime_error(status.ToString());
		}

		status = writer_handle->outfile->Close();
		delete writer_handle;
		if (!status.ok())
			throw std::runtime_error(status.ToString());
	}

}