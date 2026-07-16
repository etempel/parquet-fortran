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
#include <arrow/array/builder_decimal.h>
#include <arrow/io/api.h>
#include <arrow/util/decimal.h>
#include <arrow/util/float16.h>
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

// test/fixtures/extended_types.parquet: exercises the read-time widening
// support for Arrow physical types this library's own writer never
// produces (see CONTRIBUTING.md's "Additional scalar types" note and
// doc/pages/supported-data-types.md) -- INT8/16, UINT8/16/32/64,
// HALF_FLOAT, and DECIMAL32/64/128/256. 3 rows throughout (uniform column
// length is required within one Arrow Table); per CLAUDE.md's "sized/typed
// from the first element" convention, every column's most extreme/telling
// value is deliberately row 3, never row 1.
//
// Columns, grouped by purpose:
//   Success-path widening (int32/int64/real32/real64 all read cleanly):
//     id (INT32), v_int8, v_int16, v_uint8, v_uint16, v_uint32, v_uint64,
//     v_half_float (integral values, so both the int and real read paths
//     succeed), v_decimal32/64/128/256 (scale 0 -- i.e. a decimal that
//     happens to store an integer, the motivating "accidentally written as
//     decimal" use case), v_decimal_scaled (DECIMAL128(10,2), genuinely
//     fractional -- exercises real read-side scale handling; row 3's
//     123.45 also serves as the "non-integral" trigger value for the
//     decimal->int abort scenarios below).
//   Abort-path triggers (read into an int32/int64 array to hit exactly one
//   report_fatal_error site in convert_values_to_int32/int64 --
//   src/parquet_wrapper.cpp):
//     v_uint32_ovf (row 3 exceeds int32), v_uint64_ovf32 (fits int64, not
//     int32), v_uint64_ovf64 (exceeds int64), v_double_fractional (row 3 has
//     a nonzero fractional part), v_double_ovf32/v_double_ovf64 (row 3
//     exceeds int32/int64 respectively), v_decimal_ovf32/v_decimal_ovf64
//     (row 3 exceeds int32/int64 respectively, both scale 0 so only
//     overflow -- never the fractional-part check -- can fire).
//
// Used by test/test_reading.f90's extended-types success tests and
// test/error_scenarios.f90's corresponding abort scenarios.
static bool generate_extended_types_fixture()
{
    arrow::Int32Builder id_builder;
    auto st = id_builder.AppendValues({1, 2, 3});
    std::shared_ptr<arrow::Array> id_arr;
    st = id_builder.Finish(&id_arr);

    arrow::Int8Builder int8_builder;
    st = int8_builder.AppendValues({5, -128, 127});
    std::shared_ptr<arrow::Array> int8_arr;
    st = int8_builder.Finish(&int8_arr);

    arrow::Int16Builder int16_builder;
    st = int16_builder.AppendValues({100, -32768, 32767});
    std::shared_ptr<arrow::Array> int16_arr;
    st = int16_builder.Finish(&int16_arr);

    arrow::UInt8Builder uint8_builder;
    st = uint8_builder.AppendValues({10, 0, 255});
    std::shared_ptr<arrow::Array> uint8_arr;
    st = uint8_builder.Finish(&uint8_arr);

    arrow::UInt16Builder uint16_builder;
    st = uint16_builder.AppendValues({1000, 0, 65535});
    std::shared_ptr<arrow::Array> uint16_arr;
    st = uint16_builder.Finish(&uint16_arr);

    arrow::UInt32Builder uint32_builder;
    st = uint32_builder.AppendValues({1000, 0, 2000000000});
    std::shared_ptr<arrow::Array> uint32_arr;
    st = uint32_builder.Finish(&uint32_arr);

    arrow::UInt64Builder uint64_builder;
    st = uint64_builder.AppendValues({1000, 0, 2000000000});
    std::shared_ptr<arrow::Array> uint64_arr;
    st = uint64_builder.Finish(&uint64_arr);

    arrow::HalfFloatBuilder half_float_builder;
    st = half_float_builder.AppendValues(
        {arrow::util::Float16(2.0f).bits(), arrow::util::Float16(-3.0f).bits(), arrow::util::Float16(100.0f).bits()});
    std::shared_ptr<arrow::Array> half_float_arr;
    st = half_float_builder.Finish(&half_float_arr);

    auto decimal32_type = arrow::decimal32(9, 0);
    arrow::Decimal32Builder decimal32_builder(decimal32_type);
    st = decimal32_builder.Append(arrow::Decimal32(12));
    st = decimal32_builder.Append(arrow::Decimal32(-34));
    st = decimal32_builder.Append(arrow::Decimal32(999));
    std::shared_ptr<arrow::Array> decimal32_arr;
    st = decimal32_builder.Finish(&decimal32_arr);

    auto decimal64_type = arrow::decimal64(18, 0);
    arrow::Decimal64Builder decimal64_builder(decimal64_type);
    st = decimal64_builder.Append(arrow::Decimal64(int64_t{123456}));
    st = decimal64_builder.Append(arrow::Decimal64(int64_t{-7890}));
    st = decimal64_builder.Append(arrow::Decimal64(int64_t{999999999}));
    std::shared_ptr<arrow::Array> decimal64_arr;
    st = decimal64_builder.Finish(&decimal64_arr);

    auto decimal128_type = arrow::decimal128(20, 0);
    arrow::Decimal128Builder decimal128_builder(decimal128_type);
    st = decimal128_builder.Append(arrow::Decimal128(int64_t{123456789012}));
    st = decimal128_builder.Append(arrow::Decimal128(int64_t{-1}));
    st = decimal128_builder.Append(arrow::Decimal128(int64_t{999999999999}));
    std::shared_ptr<arrow::Array> decimal128_arr;
    st = decimal128_builder.Finish(&decimal128_arr);

    auto decimal256_type = arrow::decimal256(40, 0);
    arrow::Decimal256Builder decimal256_builder(decimal256_type);
    st = decimal256_builder.Append(arrow::Decimal256(arrow::Decimal128(int64_t{123456789012345})));
    st = decimal256_builder.Append(arrow::Decimal256(arrow::Decimal128(int64_t{-1})));
    st = decimal256_builder.Append(arrow::Decimal256(arrow::Decimal128(int64_t{999999999999999})));
    std::shared_ptr<arrow::Array> decimal256_arr;
    st = decimal256_builder.Finish(&decimal256_arr);

    // DECIMAL128(10, 2): raw unscaled values 100/-200/12345 at scale 2 mean
    // 1.00/-2.00/123.45 -- row 3 is genuinely fractional (used both as a
    // real-target success value and as the decimal "non-integral" abort
    // trigger further below).
    auto decimal_scaled_type = arrow::decimal128(10, 2);
    arrow::Decimal128Builder decimal_scaled_builder(decimal_scaled_type);
    st = decimal_scaled_builder.Append(arrow::Decimal128(int64_t{100}));
    st = decimal_scaled_builder.Append(arrow::Decimal128(int64_t{-200}));
    st = decimal_scaled_builder.Append(arrow::Decimal128(int64_t{12345}));
    std::shared_ptr<arrow::Array> decimal_scaled_arr;
    st = decimal_scaled_builder.Finish(&decimal_scaled_arr);

    arrow::UInt32Builder uint32_ovf_builder;
    st = uint32_ovf_builder.AppendValues({1000u, 0u, 4294967295u});
    std::shared_ptr<arrow::Array> uint32_ovf_arr;
    st = uint32_ovf_builder.Finish(&uint32_ovf_arr);

    arrow::UInt64Builder uint64_ovf32_builder;
    st = uint64_ovf32_builder.AppendValues({1000ull, 0ull, 5000000000ull});
    std::shared_ptr<arrow::Array> uint64_ovf32_arr;
    st = uint64_ovf32_builder.Finish(&uint64_ovf32_arr);

    arrow::UInt64Builder uint64_ovf64_builder;
    st = uint64_ovf64_builder.AppendValues({1000ull, 0ull, 18446744073709551615ull});
    std::shared_ptr<arrow::Array> uint64_ovf64_arr;
    st = uint64_ovf64_builder.Finish(&uint64_ovf64_arr);

    arrow::DoubleBuilder double_fractional_builder;
    st = double_fractional_builder.AppendValues({1.0, 2.0, 3.14});
    std::shared_ptr<arrow::Array> double_fractional_arr;
    st = double_fractional_builder.Finish(&double_fractional_arr);

    arrow::DoubleBuilder double_ovf32_builder;
    st = double_ovf32_builder.AppendValues({1.0, 2.0, 5.0e9});
    std::shared_ptr<arrow::Array> double_ovf32_arr;
    st = double_ovf32_builder.Finish(&double_ovf32_arr);

    arrow::DoubleBuilder double_ovf64_builder;
    st = double_ovf64_builder.AppendValues({1.0, 2.0, 1.0e20});
    std::shared_ptr<arrow::Array> double_ovf64_arr;
    st = double_ovf64_builder.Finish(&double_ovf64_arr);

    auto decimal_ovf32_type = arrow::decimal128(20, 0);
    arrow::Decimal128Builder decimal_ovf32_builder(decimal_ovf32_type);
    st = decimal_ovf32_builder.Append(arrow::Decimal128(int64_t{1}));
    st = decimal_ovf32_builder.Append(arrow::Decimal128(int64_t{2}));
    st = decimal_ovf32_builder.Append(arrow::Decimal128(int64_t{5000000000}));
    std::shared_ptr<arrow::Array> decimal_ovf32_arr;
    st = decimal_ovf32_builder.Finish(&decimal_ovf32_arr);

    // DECIMAL128(30, 0): row 3 is 10^20, well beyond int64_t's ~9.22e18 max
    // but comfortably within Decimal128's own ~1.7e38 range.
    auto decimal_ovf64_type = arrow::decimal128(30, 0);
    arrow::Decimal128Builder decimal_ovf64_builder(decimal_ovf64_type);
    st = decimal_ovf64_builder.Append(arrow::Decimal128(int64_t{1}));
    st = decimal_ovf64_builder.Append(arrow::Decimal128(int64_t{2}));
    arrow::Decimal128 huge_decimal;
    int32_t parsed_precision, parsed_scale;
    st = arrow::Decimal128::FromString("100000000000000000000", &huge_decimal, &parsed_precision, &parsed_scale);
    st = decimal_ovf64_builder.Append(huge_decimal);
    std::shared_ptr<arrow::Array> decimal_ovf64_arr;
    st = decimal_ovf64_builder.Finish(&decimal_ovf64_arr);

    auto id_field = arrow::field("id", arrow::int32(), false);
    auto int8_field = arrow::field("v_int8", arrow::int8(), false);
    auto int16_field = arrow::field("v_int16", arrow::int16(), false);
    auto uint8_field = arrow::field("v_uint8", arrow::uint8(), false);
    auto uint16_field = arrow::field("v_uint16", arrow::uint16(), false);
    auto uint32_field = arrow::field("v_uint32", arrow::uint32(), false);
    auto uint64_field = arrow::field("v_uint64", arrow::uint64(), false);
    auto half_float_field = arrow::field("v_half_float", arrow::float16(), false);
    auto decimal32_field = arrow::field("v_decimal32", decimal32_type, false);
    auto decimal64_field = arrow::field("v_decimal64", decimal64_type, false);
    auto decimal128_field = arrow::field("v_decimal128", decimal128_type, false);
    auto decimal256_field = arrow::field("v_decimal256", decimal256_type, false);
    auto decimal_scaled_field = arrow::field("v_decimal_scaled", decimal_scaled_type, false);
    auto uint32_ovf_field = arrow::field("v_uint32_ovf", arrow::uint32(), false);
    auto uint64_ovf32_field = arrow::field("v_uint64_ovf32", arrow::uint64(), false);
    auto uint64_ovf64_field = arrow::field("v_uint64_ovf64", arrow::uint64(), false);
    auto double_fractional_field = arrow::field("v_double_fractional", arrow::float64(), false);
    auto double_ovf32_field = arrow::field("v_double_ovf32", arrow::float64(), false);
    auto double_ovf64_field = arrow::field("v_double_ovf64", arrow::float64(), false);
    auto decimal_ovf32_field = arrow::field("v_decimal_ovf32", decimal_ovf32_type, false);
    auto decimal_ovf64_field = arrow::field("v_decimal_ovf64", decimal_ovf64_type, false);

    auto schema = arrow::schema({id_field, int8_field, int16_field, uint8_field, uint16_field, uint32_field,
        uint64_field, half_float_field, decimal32_field, decimal64_field, decimal128_field, decimal256_field,
        decimal_scaled_field, uint32_ovf_field, uint64_ovf32_field, uint64_ovf64_field, double_fractional_field,
        double_ovf32_field, double_ovf64_field, decimal_ovf32_field, decimal_ovf64_field});
    auto table = arrow::Table::Make(schema, {id_arr, int8_arr, int16_arr, uint8_arr, uint16_arr, uint32_arr,
        uint64_arr, half_float_arr, decimal32_arr, decimal64_arr, decimal128_arr, decimal256_arr, decimal_scaled_arr,
        uint32_ovf_arr, uint64_ovf32_arr, uint64_ovf64_arr, double_fractional_arr, double_ovf32_arr, double_ovf64_arr,
        decimal_ovf32_arr, decimal_ovf64_arr});

    auto maybe_outfile = arrow::io::FileOutputStream::Open("test/fixtures/extended_types.parquet");
    auto outfile = *maybe_outfile;
    auto status = parquet::arrow::WriteTable(*table, arrow::default_memory_pool(), outfile, 3);
    return status.ok();
}

// test/fixtures/nested_struct.parquet: exercises the arbitrary-depth nested-STRUCT-field read
// support (dotted-path column names, e.g. "main.inner.age") -- this library's own writer cannot
// produce STRUCT columns at all, so this fixture is hand-built directly against the Arrow API,
// same as every other fixture in this file. Schema:
//   main    : struct<
//     id    : int32
//     inner : struct<
//       name : utf8
//       age  : int32
//       deep : struct< value : int32 >    -- a 3rd level of nesting, to exercise arbitrary depth
//     >
//   >
//   vecdata : struct< spectrum : fixed_size_list<double, 3> >  -- a vector-column (FIXED_SIZE_LIST)
//             leaf under a struct, kept in its own always-valid column (see below for why)
// 5 rows, covering every independent null source a struct-leaf read must combine (see
// CLAUDE.md's nested-struct-field design notes):
//   row 1: everything present (id=1, name="Alice", age=30, deep.value=100)
//   row 2: the whole "main" struct is null for this row (root-level null)
//   row 3: "main" present (id=3) but "inner" is null (mid-level null) -- name/age/deep all null
//   row 4: "inner" present but "age" itself is null (leaf-level null) -- name/deep present
//   row 5: "inner"/"age"/"name" present but "deep" is null (a 3rd-level null, one level deeper
//          than row 3's, to prove the combination generalizes past 2 levels)
// "vecdata.spectrum" is deliberately its own top-level struct column, always valid in every row
// (spectrum=[i.0, i.1, i.2] for row i), rather than a fourth field of "main": Arrow/Parquet's
// writer rejects a FixedSizeList whose ancestor struct is null ("Lists with non-zero length null
// components are not supported" -- the same restriction documented on generate_has_null_fixture's
// own "arr_with_null" column, which likewise never makes that column's own row null). Since "main"
// is null at row 2 and "inner" is null at row 3, a FixedSizeList nested anywhere under either would
// hit that restriction; keeping it in a separate, never-null column sidesteps it while still
// exercising "FIXED_SIZE_LIST leaf resolved through a struct path" on the read side.
// Used by test/test_reading.f90's nested-struct-field tests and
// test/error_scenarios.f90's corresponding path-resolution error scenarios.
static bool generate_nested_struct_fixture()
{
    auto value_field = arrow::field("value", arrow::int32(), true);
    auto deep_type = arrow::struct_({value_field});

    auto name_field = arrow::field("name", arrow::utf8(), true);
    auto age_field = arrow::field("age", arrow::int32(), true);
    auto deep_field = arrow::field("deep", deep_type, true);
    auto inner_type = arrow::struct_({name_field, age_field, deep_field});

    auto id_field = arrow::field("id", arrow::int32(), true);
    auto inner_field = arrow::field("inner", inner_type, true);
    auto main_type = arrow::struct_({id_field, inner_field});
    auto main_field = arrow::field("main", main_type, true);

    auto spectrum_field = arrow::field("spectrum", arrow::fixed_size_list(arrow::float64(), 3), false);
    auto vecdata_type = arrow::struct_({spectrum_field});
    auto vecdata_field = arrow::field("vecdata", vecdata_type, false);

    auto value_builder = std::make_shared<arrow::Int32Builder>();
    std::vector<std::shared_ptr<arrow::ArrayBuilder>> deep_children = {value_builder};
    auto deep_builder = std::make_shared<arrow::StructBuilder>(deep_type, arrow::default_memory_pool(), deep_children);

    auto name_builder = std::make_shared<arrow::StringBuilder>();
    auto age_builder = std::make_shared<arrow::Int32Builder>();
    std::vector<std::shared_ptr<arrow::ArrayBuilder>> inner_children = {name_builder, age_builder, deep_builder};
    auto inner_builder = std::make_shared<arrow::StructBuilder>(inner_type, arrow::default_memory_pool(), inner_children);

    auto id_builder = std::make_shared<arrow::Int32Builder>();
    std::vector<std::shared_ptr<arrow::ArrayBuilder>> main_children = {id_builder, inner_builder};
    arrow::StructBuilder main_builder(main_type, arrow::default_memory_pool(), main_children);

    auto spectrum_values_builder = std::make_shared<arrow::DoubleBuilder>();
    auto spectrum_builder =
        std::make_shared<arrow::FixedSizeListBuilder>(arrow::default_memory_pool(), spectrum_values_builder, 3);
    std::vector<std::shared_ptr<arrow::ArrayBuilder>> vecdata_children = {spectrum_builder};
    arrow::StructBuilder vecdata_builder(vecdata_type, arrow::default_memory_pool(), vecdata_children);

    arrow::Status st;

    // row 1: everything present
    st = main_builder.Append();
    st = id_builder->Append(1);
    st = inner_builder->Append();
    st = name_builder->Append("Alice");
    st = age_builder->Append(30);
    st = deep_builder->Append();
    st = value_builder->Append(100);

    // row 2: "main" itself is null -- every descendant gets an automatic empty placeholder
    // (StructBuilder::AppendNull cascades this itself).
    st = main_builder.AppendNull();

    // row 3: "main" present, "inner" is null (auto-cascades to name/age/deep)
    st = main_builder.Append();
    st = id_builder->Append(3);
    st = inner_builder->AppendNull();

    // row 4: "inner" present, "age" itself is null (a leaf-level null)
    st = main_builder.Append();
    st = id_builder->Append(4);
    st = inner_builder->Append();
    st = name_builder->Append("Dave");
    st = age_builder->AppendNull();
    st = deep_builder->Append();
    st = value_builder->Append(400);

    // row 5: "inner"/"age" present, "deep" (a 3rd level) is null
    st = main_builder.Append();
    st = id_builder->Append(5);
    st = inner_builder->Append();
    st = name_builder->Append("Eve");
    st = age_builder->Append(50);
    st = deep_builder->AppendNull();

    for (int row = 1; row <= 5; ++row)
    {
        st = vecdata_builder.Append();
        st = spectrum_builder->Append();
        st = spectrum_values_builder->Append(row + 0.0);
        st = spectrum_values_builder->Append(row + 0.1);
        st = spectrum_values_builder->Append(row + 0.2);
    }

    std::shared_ptr<arrow::Array> main_arr;
    st = main_builder.Finish(&main_arr);
    std::shared_ptr<arrow::Array> vecdata_arr;
    st = vecdata_builder.Finish(&vecdata_arr);

    auto schema = arrow::schema({main_field, vecdata_field});
    auto table = arrow::Table::Make(schema, {main_arr, vecdata_arr});

    auto maybe_outfile = arrow::io::FileOutputStream::Open("test/fixtures/nested_struct.parquet");
    auto outfile = *maybe_outfile;
    // Parquet has no native fixed-size-list physical type -- without store_schema(), "spectrum"
    // would round-trip on read as a plain (variable-length) list<double> instead of
    // fixed_size_list<double, 3>, same as this library's own writer needs it
    // (arrow_writer_builder.store_schema() in parquet_wrapper.cpp) to preserve FIXED_SIZE_LIST.
    parquet::ArrowWriterProperties::Builder arrow_writer_builder;
    arrow_writer_builder.store_schema();
    auto status = parquet::arrow::WriteTable(*table, arrow::default_memory_pool(), outfile, 5,
        parquet::default_writer_properties(), arrow_writer_builder.build());
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
        {"test/fixtures/extended_types.parquet", generate_extended_types_fixture},
        {"test/fixtures/nested_struct.parquet", generate_nested_struct_fixture},
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
