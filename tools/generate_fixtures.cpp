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

// test/fixtures/list_vector.parquet: a vector (per-row array) column stored
// with Arrow's variable-length `list<element: double>` encoding -- the
// standard 3-level Parquet LIST layout
//   optional group spec (List) { repeated group list { optional double element; } }
// -- rather than the `fixed_size_list` this library's own writer always emits
// for vector columns. Both encode a per-row vector; some producers (e.g. the
// file this fixture is modelled on) use plain `list` even when every row has
// the same length. Every row here is deliberately the same length (3), so the
// column is a well-formed uniform vector column that the read side's
// get_col_size/get_uniform_list_values path can consume exactly as it does a
// fixed_size_list -- this fixture exists to prove that alternate on-disk
// schema is read back identically.
//
//   ID:   int32 scalar        [10, 20, 30, 40]
//   ra:   double scalar       [1.5, 2.5, 3.5, 4.5]
//   spec: list<double>, len 3 [[0.1,0.2,0.3],[1.1,1.2,1.3],[2.1,2.2,2.3],[3.1,3.2,3.3]]
//
// Used by test/test_reading.f90's "read list-encoded vector column" test.
static bool generate_list_vector_fixture()
{
    arrow::Int32Builder id_builder;
    auto st = id_builder.AppendValues({10, 20, 30, 40});
    std::shared_ptr<arrow::Array> id_arr;
    st = id_builder.Finish(&id_arr);

    arrow::DoubleBuilder ra_builder;
    st = ra_builder.AppendValues({1.5, 2.5, 3.5, 4.5});
    std::shared_ptr<arrow::Array> ra_arr;
    st = ra_builder.Finish(&ra_arr);

    // Variable-length ListBuilder (not FixedSizeListBuilder): this is what
    // produces the plain `list` Parquet encoding. Each row happens to append
    // exactly 3 elements, so the resulting column is uniform-length.
    auto spec_values = std::make_shared<arrow::DoubleBuilder>();
    arrow::ListBuilder spec_builder(arrow::default_memory_pool(), spec_values);
    for (int row = 0; row < 4; ++row)
    {
        st = spec_builder.Append();
        st = spec_values->Append(row + 0.1);
        st = spec_values->Append(row + 0.2);
        st = spec_values->Append(row + 0.3);
    }
    std::shared_ptr<arrow::Array> spec_arr;
    st = spec_builder.Finish(&spec_arr);

    auto id_field = arrow::field("ID", arrow::int32(), false);
    auto ra_field = arrow::field("ra", arrow::float64(), false);
    // list field itself non-nullable, element nullable -- matches Arrow's
    // default `arrow::list(value_type)` layout used by common producers.
    auto spec_field = arrow::field("spec", arrow::list(arrow::float64()), false);
    auto schema = arrow::schema({id_field, ra_field, spec_field});
    auto table = arrow::Table::Make(schema, {id_arr, ra_arr, spec_arr});

    auto maybe_outfile = arrow::io::FileOutputStream::Open("test/fixtures/list_vector.parquet");
    auto outfile = *maybe_outfile;
    auto status = parquet::arrow::WriteTable(*table, arrow::default_memory_pool(), outfile, 4);
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
        {"test/fixtures/list_vector.parquet", generate_list_vector_fixture},
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
