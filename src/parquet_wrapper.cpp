#include <arrow/api.h>
#include <arrow/io/api.h>
#include <parquet/arrow/writer.h>
#include <parquet/arrow/reader.h>

#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>
extern "C"
{

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
			case '\"':
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

	static std::string build_votable_xml(const std::string &table_name,
									const std::string &field_name,
									const std::string &unit,
									const std::string &description)
	{
		std::ostringstream xml;
		xml << "<?xml version='1.0'?>\n"
			<< "<VOTABLE version=\"1.4\" xmlns=\"http://www.ivoa.net/xml/VOTable/v1.3\">\n"
			<< "<RESOURCE>\n"
			<< "<TABLE name=\"" << xml_escape(table_name) << "\">\n"
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

	// ---------- WRITE ----------
	void write_parquet_double_meta(
		const char *filename,
		double *data,
		int64_t n,
		const char *unit,
		const char *description)
	{
		arrow::DoubleBuilder builder;

		auto status = builder.AppendValues(data, n);
		if (!status.ok())
			throw std::runtime_error(status.ToString());

		std::shared_ptr<arrow::Array> array;
		status = builder.Finish(&array);
		if (!status.ok())
			throw std::runtime_error(status.ToString());

		// Keep per-column metadata on the Arrow field.
		std::vector<std::string> keys = {"unit", "description"};
		std::vector<std::string> values = {unit, description};
		auto metadata = std::make_shared<arrow::KeyValueMetadata>(keys, values);

		auto field = arrow::field("mycol", arrow::float64(), false, metadata);

		std::vector<std::string> schema_keys = {"mycol.unit", "mycol.description"};
		std::vector<std::string> schema_values = {unit, description};
		auto schema_metadata = std::make_shared<arrow::KeyValueMetadata>(schema_keys, schema_values);

		auto schema = arrow::schema({field}, schema_metadata);
		auto table = arrow::Table::Make(schema, {array});

		auto outfile = arrow::io::FileOutputStream::Open(filename).ValueOrDie();

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

		auto writer = std::move(maybe_writer).ValueOrDie();

		auto votable_xml = build_votable_xml("table", "mycol", unit, description);
		auto file_metadata = std::make_shared<arrow::KeyValueMetadata>(
			std::vector<std::string>{"IVOA.VOTable-Parquet.content", "IVOA.VOTable-Parquet.version", "name"},
			std::vector<std::string>{votable_xml, "1.0", "table"});

		status = writer->AddKeyValueMetadata(file_metadata);
		if (!status.ok())
			throw std::runtime_error(status.ToString());

		status = writer->WriteTable(*table, 1024);
		if (!status.ok())
			throw std::runtime_error(status.ToString());

		status = writer->Close();

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