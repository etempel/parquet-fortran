
#include <arrow/api.h>
#include <arrow/io/api.h>
#include <parquet/arrow/writer.h>
#include <parquet/arrow/reader.h>

extern "C"
{

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

        // Metadata
        std::vector<std::string> keys = {"unit", "description"};
        std::vector<std::string> values = {unit, description};
        auto metadata = std::make_shared<arrow::KeyValueMetadata>(keys, values);

        auto field = arrow::field("col1", arrow::float64(), false, metadata);
        auto schema = arrow::schema({field});
        auto table = arrow::Table::Make(schema, {array});

        auto outfile = arrow::io::FileOutputStream::Open(filename).ValueOrDie();

        status = parquet::arrow::WriteTable(
            *table,
            arrow::default_memory_pool(),
            outfile,
            1024);

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
