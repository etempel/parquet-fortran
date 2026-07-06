// Generates every hand-built Arrow/Parquet test fixture under test/fixtures/
// that this library's own writer cannot produce itself. Kept as a single
// file (one fixture-generating function per fixture, called from main())
// rather than one .cpp per fixture, so there's exactly one program to build
// and run regardless of how many fixtures exist -- see run_generate_fixtures.sh.
//
// This file lives under tools/, not test/, so fpm's auto-test discovery
// doesn't try to build it (and its own main()) into every test executable.
//
// Rebuild and run via:
//   tools/run_generate_fixtures.sh
// which compiles this with clang++ (using this project's own
// FPM_CXXFLAGS/FPM_LDFLAGS, matching README's "Environment variables"
// section) and runs it from the repository root.
#include <arrow/api.h>
#include <arrow/io/api.h>
#include <parquet/arrow/writer.h>
#include <cstdio>
#include <memory>

// test/fixtures/has_null.parquet: 3 rows with genuine Arrow/Parquet Nulls
// (real validity-bitmap Nulls, not sentinel values) across three columns --
// this library's own writer cannot produce these (it never calls Arrow's
// AppendNull). Columns:
//   id_with_null:  int32 scalar,        null at row 2
//   arr_with_null: fixed-size list<int32, 2>, row 3's second element is null
//                  (inner/child-values-level null) -- a whole-row (outer
//                  list-level) null is deliberately NOT included here: the
//                  Parquet writer rejects FixedSizeList null rows with
//                  non-zero-length null components ("Lists with non-zero
//                  length null components are not supported"), so this
//                  fixture cannot exercise that path against a real file
//   name_with_null: string scalar,      null at row 2
// Used by test/error_scenarios.f90's "read_column_with_nulls" scenario (the
// strict, default-mode Null guard) and test/test_reading.f90's null_value/
// is_valid tolerant-read tests.
static bool generate_has_null_fixture()
{
    arrow::Int32Builder id_builder;
    auto st = id_builder.Append(1);
    st = id_builder.AppendNull();
    st = id_builder.Append(3);
    std::shared_ptr<arrow::Array> id_arr;
    st = id_builder.Finish(&id_arr);

    auto value_builder = std::make_shared<arrow::Int32Builder>();
    arrow::FixedSizeListBuilder list_builder(arrow::default_memory_pool(), value_builder, 2);
    // row 1: [1, 2]
    st = list_builder.Append();
    st = value_builder->Append(1);
    st = value_builder->Append(2);
    // row 2: [3, 4] (no null; a whole-row null cannot be represented for
    // FixedSizeList by the Parquet writer -- see comment above)
    st = list_builder.Append();
    st = value_builder->Append(3);
    st = value_builder->Append(4);
    // row 3: [5, null] (inner/child-values-level null)
    st = list_builder.Append();
    st = value_builder->Append(5);
    st = value_builder->AppendNull();
    std::shared_ptr<arrow::Array> arr_arr;
    st = list_builder.Finish(&arr_arr);

    arrow::StringBuilder name_builder;
    st = name_builder.Append("first");
    st = name_builder.AppendNull();
    st = name_builder.Append("third");
    std::shared_ptr<arrow::Array> name_arr;
    st = name_builder.Finish(&name_arr);

    auto id_field = arrow::field("id_with_null", arrow::int32(), true);
    auto arr_field = arrow::field("arr_with_null", arrow::fixed_size_list(arrow::int32(), 2), true);
    auto name_field = arrow::field("name_with_null", arrow::utf8(), true);
    auto schema = arrow::schema({id_field, arr_field, name_field});
    auto table = arrow::Table::Make(schema, {id_arr, arr_arr, name_arr});

    auto maybe_outfile = arrow::io::FileOutputStream::Open("test/fixtures/has_null.parquet");
    auto outfile = *maybe_outfile;
    auto status = parquet::arrow::WriteTable(*table, arrow::default_memory_pool(), outfile, 3);
    return status.ok();
}

// test/fixtures/unsupported_type.parquet: a file containing one column ("d")
// of Arrow's date32 type, alongside one ordinary int32 column ("id") that
// this library does support. This library's own writer can only ever
// produce the six supported types (see README's "Supported data types"), so
// a fixture with a column outside that set has to be built here too.
//
// Used by test/error_scenarios.f90's "read_unsupported_physical_type"
// scenario, which exercises the documented "harsher failure mode" in
// README's Limitations section: reading a column whose physical Parquet
// type falls outside this library's six supported types.
static bool generate_unsupported_type_fixture()
{
    arrow::Int32Builder id_builder;
    auto st = id_builder.Append(1);
    st = id_builder.Append(2);
    st = id_builder.Append(3);
    std::shared_ptr<arrow::Array> id_arr;
    st = id_builder.Finish(&id_arr);

    arrow::Date32Builder date_builder;
    st = date_builder.Append(0);
    st = date_builder.Append(1);
    st = date_builder.Append(2);
    std::shared_ptr<arrow::Array> date_arr;
    st = date_builder.Finish(&date_arr);

    auto id_field = arrow::field("id", arrow::int32(), false);
    auto date_field = arrow::field("d", arrow::date32(), false);
    auto schema = arrow::schema({id_field, date_field});
    auto table = arrow::Table::Make(schema, {id_arr, date_arr});

    auto maybe_outfile = arrow::io::FileOutputStream::Open("test/fixtures/unsupported_type.parquet");
    auto outfile = *maybe_outfile;
    auto status = parquet::arrow::WriteTable(*table, arrow::default_memory_pool(), outfile, 3);
    return status.ok();
}

int main()
{
    struct Fixture
    {
        const char *name;
        bool (*generate)();
    };

    Fixture fixtures[] = {
        {"test/fixtures/has_null.parquet", generate_has_null_fixture},
        {"test/fixtures/unsupported_type.parquet", generate_unsupported_type_fixture},
    };

    int failures = 0;
    for (const auto &fixture : fixtures)
    {
        if (fixture.generate())
        {
            std::printf("[OK]   %s\n", fixture.name);
        }
        else
        {
            std::printf("[FAIL] %s\n", fixture.name);
            ++failures;
        }
    }

    return failures == 0 ? 0 : 1;
}
