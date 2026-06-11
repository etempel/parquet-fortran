#include <arrow/api.h>
#include <arrow/io/api.h>
#include <parquet/arrow/reader.h>
#include <parquet/arrow/writer.h>

#include <ctime>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>

extern "C"
{
	struct ParquetWriterHandle
	{
		std::unique_ptr<parquet::arrow::FileWriter> writer;
		std::shared_ptr<arrow::Schema> schema;
	};

	static ParquetWriterHandle *as_handle(void *handle)
	{
		return static_cast<ParquetWriterHandle *>(handle);
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
		std::tm tm = *std::gmtime(&now);
		char buffer[32];
		std::strftime(buffer, sizeof(buffer), "%Y-%m-%dT%H:%M:%S", &tm);
		return std::string(buffer);
	}

	static std::string build_votable_xml(const std::string &table_name,
									const std::string &field_name,
									const std::string &unit,
									const std::string &description,
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
			<< "<FIELD datatype=\"double\" name=\"" << xml_escape(field_name)
			<< "\" unit=\"" << xml_escape(unit) << "\">\n"
			<< "<DESCRIPTION>" << xml_escape(description) << "</DESCRIPTION>\n"
			<< "</FIELD>\n"
			<< "<!-- Dummy VOTable - no DATA element -->\n"
			<< "</TABLE>\n"
			<< "</RESOURCE>\n"
			<< "</VOTABLE>\n";
		return xml.str();
	}

	void *create_parquet_double_writer(const char *filename)
	{
		auto outfile = arrow::io::FileOutputStream::Open(filename).ValueOrDie();
		auto field = arrow::field("mycol", arrow::float64(), false);
		auto schema = arrow::schema({field});

		parquet::ArrowWriterProperties::Builder arrow_writer_builder;
		arrow_writer_builder.store_schema();
		auto arrow_writer_properties = arrow_writer_builder.build();
		auto writer_properties = parquet::WriterProperties::Builder().build();

		auto maybe_writer = parquet::arrow::FileWriter::Open(
			*schema,
			arrow::default_memory_pool(),
			outfile,
			writer_properties,
			arrow_writer_properties);
		if (!maybe_writer.ok())
			throw std::runtime_error(maybe_writer.status().ToString());

		auto *handle = new ParquetWriterHandle{};
		handle->writer = std::move(maybe_writer).ValueOrDie();
		handle->schema = schema;
		return handle;
	}

	void write_parquet_double_data(void *handle, const double *data, int64_t n)
	{
		auto writer_handle = as_handle(handle);
		arrow::DoubleBuilder builder;

		auto status = builder.AppendValues(data, n);
		if (!status.ok())
			throw std::runtime_error(status.ToString());

		std::shared_ptr<arrow::Array> array;
		status = builder.Finish(&array);
		if (!status.ok())
			throw std::runtime_error(status.ToString());

		auto table = arrow::Table::Make(writer_handle->schema, {array});
		status = writer_handle->writer->WriteTable(*table, 1024);
		if (!status.ok())
			throw std::runtime_error(status.ToString());
	}

	void write_parquet_votable_metadata(void *handle, const char *unit, const char *description)
	{
		auto writer_handle = as_handle(handle);
		auto date = current_utc_timestamp();
		auto votable_xml = build_votable_xml("table", "mycol", unit, description, date);
		auto file_metadata = std::make_shared<arrow::KeyValueMetadata>(
			std::vector<std::string>{"IVOA.VOTable-Parquet.content", "IVOA.VOTable-Parquet.version", "DATE", "name"},
			std::vector<std::string>{votable_xml, "1.0", date, "table"});

		auto status = writer_handle->writer->AddKeyValueMetadata(file_metadata);
		if (!status.ok())
			throw std::runtime_error(status.ToString());
	}

	void close_parquet_writer(void *handle)
	{
		auto writer_handle = as_handle(handle);
		auto status = writer_handle->writer->Close();
		delete writer_handle;
		if (!status.ok())
			throw std::runtime_error(status.ToString());
	}

	// ---------- READ ----------
	void read_parquet_double(
		const char *filename,
		double *data,
		int64_t *n)
	{
		auto infile = arrow::io::ReadableFile::Open(filename).ValueOrDie();

		auto reader = parquet::arrow::OpenFile(
					  infile,
					  arrow::default_memory_pool())
					  .ValueOrDie();

		auto table = reader->ReadTable().ValueOrDie();

		auto column = table->column(0)->chunk(0);
		auto arr = std::static_pointer_cast<arrow::DoubleArray>(column);

		*n = arr->length();

		for (int64_t i = 0; i < *n; i++)
		{
			data[i] = arr->Value(i);
		}
	}
}