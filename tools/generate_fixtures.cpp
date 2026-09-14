// Generates every hand-built Arrow/Parquet test fixture under test/fixtures/
// that this library's own writer cannot produce itself.
// Kept as a single file (one fixture-generating function per fixture, called
// from main()) rather than one .cpp per fixture, so there's exactly one
// program to build and run regardless of how many fixtures exist -- see
// run_generate_fixtures.sh.
//
// This file lives under tools/, not test/, so fpm's auto-test discovery
// doesn't try to build it (and its own main()) into every test executable.
//
// Rebuild and run via:
//   tools/run_generate_fixtures.sh
// which compiles this with clang++ (using this project's own
// FPM_CXXFLAGS/FPM_LDFLAGS, matching README's "Environment variables"
// section) and runs it from the repository root.
//
// These fixtures deliberately contain shapes this library's own writer CANNOT produce -- genuine
// Arrow validity-bitmap Nulls, per-element (rather than per-row) Nulls in a vector column, an
// unsupported physical column type, a list-encoded (per-row array) vector column, extended
// read-only source types, and a STRUCT column -- so that the reader's handling of them can be
// exercised. They are committed, so a normal `fpm test` never regenerates them.
//
// What each one is for:
//
//   * `has_null.parquet` -- columns with real Nulls (not sentinel values), read by the
//     `errors`/`reading` suites.
//   * `unsupported_type.parquet` -- a column of a physical type this library refuses to read.
//   * `list_vector.parquet` -- a vector column stored as Parquet `LIST` rather than the fixed-size
//     layout this library writes.
//   * `no_stats.parquet` -- a Null-carrying column written with column statistics disabled, the
//     one case `parquet_column_has_nulls` cannot answer from the footer. Proves the conservative
//     fallback (request the validity mask anyway) rather than trusting an absent statistic.
//     Unreachable with any other fixture, since every other one carries statistics.
//   * `list_widths.parquet` -- one column per shape a variable-length `LIST` column can take
//     (uniform, ragged, ragged-but-with-a-whole-number mean, uniform-except-in-the-last-row-group,
//     containing a null row, containing an empty row, a `LIST` leaf under a `STRUCT`, plus a
//     scalar control), written across 4 row groups. Drives the deferred-width tests in the `table`
//     and `reading` suites and the `list_width_never_reads_whole_column` error scenario. Written
//     without `store_schema()` on purpose -- with it, Arrow round-trips a `fixed_size_list` as a
//     `fixed_size_list` and none of these columns would exercise the plain-`LIST` path.
//   * `list_payloads.parquet` -- one genuinely ragged variable-length `LIST` column per payload
//     element type this library can read (plus a `large_list`, an `int8`/`uint32` pair whose
//     conversion is visible, and columns carrying null elements / a null row / an empty row),
//     written across 3 row groups. The two other list fixtures are `int32`-only, so without this
//     one eight of the nine element-type arms of the `LIST` read path would never be entered.
//   * `extended_types.parquet` -- columns of the extended read-only source types
//     (`int8`/`int16`/unsigned integers/`half_float`/`decimal`).
//   * `map_payloads.parquet` -- one flat top-level `MAP` column per value family, plus one with
//     duplicate keys and one keyed by int32 that exists only to be refused. Four rows covering a
//     map's two null levels and the present-but-empty case, in two row groups.
//   * `struct_payloads.parquet` -- one flat top-level `STRUCT` column per payload family, plus
//     one carrying all nine at once, with the four null shapes that separate a struct's own
//     row nullness from its fields'. Two row groups. See its own banner for why no existing
//     fixture could serve.
//   * `nested_struct.parquet` -- a `STRUCT` column, nested 3 levels deep, with a `FIXED_SIZE_LIST`
//     vector-column leaf in a sibling `STRUCT` column, and rows covering every independent null-
//     combination source (see Reading a nested struct field (doc/pages/types/supported-data-
//     types.md#reading-a-nested-struct-field)).
//   * `element_nulls.parquet` -- vector (`FIXED_SIZE_LIST`) columns whose nulls sit on individual
//     elements rather than on whole rows, one column per validity dispatch class (`float64`
//     bitmap, `string`, `timestamp`), each with exactly one null element. Read by the `table`
//     suite. It has to come from here rather than from this library's own writer: every other
//     element-null test writes its fixture with `parquet_write_column` and reads it back, so a
//     defect that widened a null on read and broadcast it on write would agree with itself and
//     look healthy.
//   * `screen_declined_nulls.parquet` -- drives the row-group statistics screen's decline paths:
//     a column whose footer statistics cannot rule a row group in or out, so the screen must
//     answer "may match" rather than prune. A wrongly pruned row group is this library's one
//     silent-wrong-answer failure mode, so the fixture exists to prove the screen declines.
//   * `dictionary_types.parquet` -- dictionary-typed (Arrow `DictionaryArray`) columns, which is
//     what a pandas `category` column arrives as, each paired with a plain twin carrying the same
//     values so a test has an oracle to compare against. Written across two row groups whose
//     dictionaries differ, so the same index means different values in the two of them.
//   * `map_list_types.parquet` -- exercises Arrow's `MAP` and (variable-length) `LIST` types,
//     generated by `generate_map_list_types_fixture` in `tools/generate_fixtures.cpp`. Currently
//     reserved/unused: no Fortran test reads it, since `MAP` columns and struct-nested variable-
//     length `LIST` columns remain unsupported (see Features considered but not implemented
//     (#features-considered-but-not-implemented)) -- kept as groundwork for if that support is
//     ever added.
#include <arrow/api.h>
#include <arrow/compute/api.h>
#include <arrow/extension/fixed_shape_tensor.h>
#include <arrow/array/builder_decimal.h>
#include <arrow/io/api.h>
#include <arrow/util/decimal.h>
#include <arrow/util/float16.h>
#include <parquet/arrow/writer.h>
#include <cstdio>
#include <cstring>
#include <functional>
#include <vector>
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

// test/fixtures/list_widths.parquet: every shape a plain Parquet LIST column can take, in one
// file, so the deferred-width machinery can be exercised end to end against all of them at once.
//
// The point of these columns is that a plain `list` carries NO width in the schema (unlike the
// `fixed_size_list` this library writes), so whether one uniform width exists is a property of the
// data. parquet_table therefore defers such a column's kind and width to first use, and resolves
// them in two tiers: a footer screen (per row group, mean elements per row must be a whole number
// and must agree between row groups) and, only for a survivor, a row-group-by-row-group scan. The
// columns below are chosen to hit every outcome of that pair:
//
//   uniform   list<int32>,  every row length 3    -> screen yields candidate 3, scan confirms.
//                                                    A VECTOR column of width 3.
//   avg_ok    list<int32>,  lengths 3,1,3,1,...   -> mean is exactly 2, so the screen CANNOT
//                                                    reject it. Only the scan catches it. This is
//                                                    the column that proves a candidate is not a
//                                                    proof, and that the read path rejects a wrong
//                                                    candidate rather than mis-typing the column.
//   ragged    list<int32>,  lengths 1,2,3,4,...   -> mean is non-integral; screen rejects for free.
//   late      list<int32>,  uniform 3 except the
//                           LAST row group        -> the screen catches it only because it compares
//                                                    row groups against each other, not just each
//                                                    one's own divisibility.
//   with_null list<int32>,  one NULL list row     -> a null row has length 0, so no width >= 1
//                                                    covers every row; the screen already rejects it.
//   null_avg  list<int32>,  lengths 5,5,5,NULL     -> 16 slots over 4 rows is a mean of exactly 4,
//                                                    because a null row occupies one slot -- so the
//                                                    screen passes it and only the scan rejects it.
//                                                    The null counterpart of avg_ok.
//   with_empty list<int32>, one EMPTY list row    -> same, and distinct from the null case on the
//                                                    Arrow side even though the verdict matches.
//   nested    struct<vals: list<int32>>           -> the same deferral reached through a dotted
//                                                    struct path ("nested.vals"), where the leaf
//                                                    index and the max definition level both differ
//                                                    from the top-level case.
//   scalar    int32                               -> the control: a non-list column, whose width is
//                                                    1 from the schema and which must never be
//                                                    deferred or read to classify.
//
// Written with row_group_size 4 and 16 rows, so there are 4 row groups and the `late` column's
// violation genuinely lives in a different row group from its uniform rows. store_schema() is
// deliberately NOT called: with it, Arrow would round-trip a fixed_size_list as a fixed_size_list
// and none of these columns would test the plain-LIST path at all.
//
// Used by test/test_table.f90's deferred-width tests and by
// test/error_scenarios.f90's list_width_screen_avoids_column_read scenario.
static bool generate_list_widths_fixture()
{
    constexpr int kRows = 16;
    constexpr int kRowGroup = 4;

    // Each lambda returns one row's element count; -1 means a NULL list, 0 an empty one.
    auto build_list = [](const std::function<int(int)> &len_of) {
        auto values = std::make_shared<arrow::Int32Builder>();
        arrow::ListBuilder builder(arrow::default_memory_pool(), values);
        arrow::Status st;
        for (int row = 0; row < kRows; ++row)
        {
            int len = len_of(row);
            if (len < 0)
            {
                st = builder.AppendNull();
                continue;
            }
            st = builder.Append();
            for (int e = 0; e < len; ++e)
            {
                st = values->Append(row * 100 + e);
            }
        }
        std::shared_ptr<arrow::Array> out;
        st = builder.Finish(&out);
        return out;
    };

    auto uniform_arr = build_list([](int) { return 3; });
    auto avg_ok_arr = build_list([](int row) { return row % 2 == 0 ? 3 : 1; });
    auto ragged_arr = build_list([](int row) { return row % 4 + 1; });
    // Uniform 3 everywhere except the final row group (rows 12..15), which holds 2s.
    auto late_arr = build_list([](int row) { return row < kRows - kRowGroup ? 3 : 2; });
    auto with_null_arr = build_list([](int row) { return row == 5 ? -1 : 3; });
    // Lengths 5,5,5,NULL in every row group: 16 leaf slots over 4 rows, so the mean is exactly 4
    // and the screen CANNOT reject it -- a null row occupies one slot. Only the scan sees that the
    // null row's length is 0 and no width covers every row. The null counterpart of avg_ok.
    auto null_avg_arr = build_list([](int row) { return row % 4 == 3 ? -1 : 5; });
    auto with_empty_arr = build_list([](int row) { return row == 5 ? 0 : 3; });

    auto nested_vals = build_list([](int) { return 3; });
    auto nested_arr = arrow::StructArray::Make({nested_vals}, std::vector<std::string>{"vals"}).ValueOrDie();

    arrow::Int32Builder scalar_builder;
    arrow::Status st;
    for (int row = 0; row < kRows; ++row)
    {
        st = scalar_builder.Append(row);
    }
    std::shared_ptr<arrow::Array> scalar_arr;
    st = scalar_builder.Finish(&scalar_arr);

    auto schema = arrow::schema({
        arrow::field("uniform", arrow::list(arrow::int32())),
        arrow::field("avg_ok", arrow::list(arrow::int32())),
        arrow::field("ragged", arrow::list(arrow::int32())),
        arrow::field("late", arrow::list(arrow::int32())),
        arrow::field("with_null", arrow::list(arrow::int32())),
        arrow::field("null_avg", arrow::list(arrow::int32())),
        arrow::field("with_empty", arrow::list(arrow::int32())),
        arrow::field("nested", arrow::struct_({arrow::field("vals", arrow::list(arrow::int32()))})),
        arrow::field("scalar", arrow::int32()),
    });
    auto table = arrow::Table::Make(schema, {uniform_arr, avg_ok_arr, ragged_arr, late_arr,
        with_null_arr, null_avg_arr, with_empty_arr, nested_arr, scalar_arr});

    auto maybe_outfile = arrow::io::FileOutputStream::Open("test/fixtures/list_widths.parquet");
    auto outfile = *maybe_outfile;
    auto status = parquet::arrow::WriteTable(*table, arrow::default_memory_pool(), outfile, kRowGroup);
    return status.ok();
}

// test/fixtures/list_payloads.parquet: one genuinely ragged variable-length column per payload
// element type this library can read, so the LIST read path is exercised across all nine of its
// element-type arms rather than only the int32 one.
//
// It exists because the two other list fixtures are int32-only: list_widths.parquet's seven list
// columns are all `list<int32>` (its subject is the WIDTH machinery, one column per shape), and
// has_null.parquet's is too. Without this file, eight of the nine arms of the read path's
// element-type dispatch would be `case default` waiting to happen -- an arm never entered is a
// passing test.
//
// Deliberately a SEPARATE file rather than extra columns on list_widths.parquet: that one is
// consumed by the `table` and `reading` suites and by the list_width_* error scenarios, and every
// one of its column shapes is chosen to hit a specific outcome of the width screen. Adding columns
// to it risks those; a new file risks nothing.
//
// 12 rows written with row_group_size 4, so there are 3 row groups and every column's raggedness
// spans a row-group boundary. Row lengths are `row % 4 + 1` (1,2,3,4 repeating), so no column has
// a whole-number mean elements-per-row and none of them is mistakable for a vector column.
//
// The value in row `r`, element `e` (both 0-based) is `r * 100 + e` in whatever the column's own
// units are, so a test can predict any cell from its coordinates alone:
//
//   i32        list<int32>        the baseline
//   i64        list<int64>
//   f32        list<float>        value + 0.5, so a truncating read would be visible
//   f64        list<double>       value + 0.25
//   flag       list<bool>         (r + e) even
//   text       list<string>       "v<value>", plus a deliberate EMPTY string and a deliberate
//                                 TRAILING SPACE (row 0), which a padded/trimming read would lose
//   day        list<date32>       value days after the epoch
//   clock      list<time64[us]>   value * 1000 microseconds past midnight
//   stamp      list<timestamp[ms]> value * 1000 milliseconds after the epoch
//   narrow8    list<int8>         value % 100, read as an int32 payload (narrowest LOSSLESS kind)
//   wide32     list<uint32>       value + 3000000000, read as an int64 payload -- uint32's top
//                                 half does not fit a signed int32, which is what makes this
//                                 column prove the conversion rather than merely exercise it
//   big        large_list<int32>  the int64-offsets variant, which no other fixture anywhere
//                                 contains and which was previously permanently-dead code
//   elem_nulls list<int32>        every row present, but element 0 of each row is NULL -- the
//                                 second null level, distinct from a null row
//   day_nulls  list<date32>       the same, on a temporal payload: a date carries its null state
//                                 INSIDE the element, so this is the one combination no other
//                                 fixture reaches
//   mixed      list<int32>        all three at once: row 5 is a NULL row, row 6 is an EMPTY
//                                 (present, length 0) row, and row 7's last element is NULL
//   rowid      int32              the scalar control, holding the 0-based row index. The only
//                                 column here a filter rule or a sort key can name -- every other
//                                 one is a list, and neither can be written against one -- so
//                                 without it this fixture cannot be read through a row transform
//
// store_schema() IS called here, unlike list_widths.parquet -- see the call site for why (short
// version: Parquet cannot store the LIST/LARGE_LIST distinction, so `big` needs it and nothing
// else in this file is affected either way).
static bool generate_list_payloads_fixture()
{
    constexpr int kRows = 12;
    constexpr int kRowGroup = 4;

    // Row `row`'s element count. Kept in one place so every column below is ragged identically
    // and a test can predict one column's offsets from another's.
    auto len_of = [](int row) { return row % 4 + 1; };
    // The canonical value of row `row`, element `e`.
    auto value_of = [](int row, int e) { return row * 100 + e; };

    arrow::Status st;
    // Builds one list column, calling `append(values_builder, row, e)` for each element and
    // `null_elem` deciding which elements are appended as NULL instead. `null_row`/`empty_row`
    // pick out the rows that are absent / present-but-empty.
    auto build = [&](auto values_builder, const std::function<void(int, int)> &append,
                     const std::function<bool(int, int)> &null_elem,
                     const std::function<bool(int)> &null_row,
                     const std::function<bool(int)> &empty_row) {
        arrow::ListBuilder builder(arrow::default_memory_pool(), values_builder);
        arrow::Status inner;
        for (int row = 0; row < kRows; ++row)
        {
            if (null_row && null_row(row))
            {
                inner = builder.AppendNull();
                continue;
            }
            inner = builder.Append();
            if (empty_row && empty_row(row)) continue;
            for (int e = 0; e < len_of(row); ++e)
            {
                if (null_elem && null_elem(row, e))
                {
                    inner = values_builder->AppendNull();
                    continue;
                }
                append(row, e);
            }
        }
        std::shared_ptr<arrow::Array> out;
        inner = builder.Finish(&out);
        return out;
    };
    auto no_null_elem = std::function<bool(int, int)>();
    auto no_null_row = std::function<bool(int)>();
    auto no_empty_row = std::function<bool(int)>();

    auto i32b = std::make_shared<arrow::Int32Builder>();
    auto i32_arr = build(i32b, [&](int r, int e) { st = i32b->Append(value_of(r, e)); },
        no_null_elem, no_null_row, no_empty_row);

    auto i64b = std::make_shared<arrow::Int64Builder>();
    auto i64_arr = build(i64b, [&](int r, int e) { st = i64b->Append(value_of(r, e)); },
        no_null_elem, no_null_row, no_empty_row);

    auto f32b = std::make_shared<arrow::FloatBuilder>();
    auto f32_arr = build(f32b, [&](int r, int e) { st = f32b->Append(value_of(r, e) + 0.5f); },
        no_null_elem, no_null_row, no_empty_row);

    auto f64b = std::make_shared<arrow::DoubleBuilder>();
    auto f64_arr = build(f64b, [&](int r, int e) { st = f64b->Append(value_of(r, e) + 0.25); },
        no_null_elem, no_null_row, no_empty_row);

    auto boolb = std::make_shared<arrow::BooleanBuilder>();
    auto flag_arr = build(boolb, [&](int r, int e) { st = boolb->Append((r + e) % 2 == 0); },
        no_null_elem, no_null_row, no_empty_row);

    // Row 0 element 0 is "" and row 0 element 1 would not exist (row 0 has length 1), so the
    // trailing-space value goes on row 1 element 0. Both are what a fixed-width padded read
    // cannot represent, which is the point of testing a string payload at all.
    auto strb = std::make_shared<arrow::StringBuilder>();
    auto text_arr = build(strb, [&](int r, int e) {
        if (r == 0 && e == 0) { st = strb->Append(""); return; }
        if (r == 1 && e == 0) { st = strb->Append("pad "); return; }
        st = strb->Append("v" + std::to_string(value_of(r, e)));
    }, no_null_elem, no_null_row, no_empty_row);

    auto dayb = std::make_shared<arrow::Date32Builder>();
    auto day_arr = build(dayb, [&](int r, int e) { st = dayb->Append(value_of(r, e)); },
        no_null_elem, no_null_row, no_empty_row);

    auto clockb = std::make_shared<arrow::Time64Builder>(arrow::time64(arrow::TimeUnit::MICRO),
        arrow::default_memory_pool());
    auto clock_arr = build(clockb, [&](int r, int e) {
        st = clockb->Append(static_cast<int64_t>(value_of(r, e)) * 1000);
    }, no_null_elem, no_null_row, no_empty_row);

    auto stampb = std::make_shared<arrow::TimestampBuilder>(arrow::timestamp(arrow::TimeUnit::MILLI),
        arrow::default_memory_pool());
    auto stamp_arr = build(stampb, [&](int r, int e) {
        st = stampb->Append(static_cast<int64_t>(value_of(r, e)) * 1000);
    }, no_null_elem, no_null_row, no_empty_row);

    auto n8b = std::make_shared<arrow::Int8Builder>();
    auto narrow8_arr = build(n8b, [&](int r, int e) {
        st = n8b->Append(static_cast<int8_t>(value_of(r, e) % 100));
    }, no_null_elem, no_null_row, no_empty_row);

    auto u32b = std::make_shared<arrow::UInt32Builder>();
    auto wide32_arr = build(u32b, [&](int r, int e) {
        st = u32b->Append(static_cast<uint32_t>(value_of(r, e)) + 3000000000u);
    }, no_null_elem, no_null_row, no_empty_row);

    auto enb = std::make_shared<arrow::Int32Builder>();
    auto elem_nulls_arr = build(enb, [&](int r, int e) { st = enb->Append(value_of(r, e)); },
        [](int, int e) { return e == 0; }, no_null_row, no_empty_row);

    auto dnb = std::make_shared<arrow::Date32Builder>();
    auto day_nulls_arr = build(dnb, [&](int r, int e) { st = dnb->Append(value_of(r, e)); },
        [](int, int e) { return e == 0; }, no_null_row, no_empty_row);

    auto mixb = std::make_shared<arrow::Int32Builder>();
    auto mixed_arr = build(mixb, [&](int r, int e) { st = mixb->Append(value_of(r, e)); },
        [&](int r, int e) { return r == 7 && e == len_of(r) - 1; },
        [](int r) { return r == 5; }, [](int r) { return r == 6; });

    // large_list has no ListBuilder-shaped helper above (arrow::LargeListBuilder is a different
    // type), so it is built out of line -- the one column whose offsets are int64.
    auto bigvals = std::make_shared<arrow::Int32Builder>();
    arrow::LargeListBuilder bigb(arrow::default_memory_pool(), bigvals);
    for (int row = 0; row < kRows; ++row)
    {
        st = bigb.Append();
        for (int e = 0; e < len_of(row); ++e)
        {
            st = bigvals->Append(value_of(row, e));
        }
    }
    std::shared_ptr<arrow::Array> big_arr;
    st = bigb.Finish(&big_arr);

    // The control, and the only column here a filter or a sort key can name: every other column
    // is a list, and neither a filter rule nor a sort key can be written against one. Without it
    // this fixture cannot be read through a row transform at all.
    arrow::Int32Builder rowid_builder;
    for (int row = 0; row < kRows; ++row) st = rowid_builder.Append(row);
    std::shared_ptr<arrow::Array> rowid_arr;
    st = rowid_builder.Finish(&rowid_arr);

    auto schema = arrow::schema({
        arrow::field("rowid", arrow::int32()),
        arrow::field("i32", arrow::list(arrow::int32())),
        arrow::field("i64", arrow::list(arrow::int64())),
        arrow::field("f32", arrow::list(arrow::float32())),
        arrow::field("f64", arrow::list(arrow::float64())),
        arrow::field("flag", arrow::list(arrow::boolean())),
        arrow::field("text", arrow::list(arrow::utf8())),
        arrow::field("day", arrow::list(arrow::date32())),
        arrow::field("clock", arrow::list(arrow::time64(arrow::TimeUnit::MICRO))),
        arrow::field("stamp", arrow::list(arrow::timestamp(arrow::TimeUnit::MILLI))),
        arrow::field("narrow8", arrow::list(arrow::int8())),
        arrow::field("wide32", arrow::list(arrow::uint32())),
        arrow::field("big", arrow::large_list(arrow::int32())),
        arrow::field("elem_nulls", arrow::list(arrow::int32())),
        arrow::field("day_nulls", arrow::list(arrow::date32())),
        arrow::field("mixed", arrow::list(arrow::int32())),
    });
    auto table = arrow::Table::Make(schema, {rowid_arr, i32_arr, i64_arr, f32_arr, f64_arr, flag_arr, text_arr,
        day_arr, clock_arr, stamp_arr, narrow8_arr, wide32_arr, big_arr, elem_nulls_arr,
        day_nulls_arr, mixed_arr});

    auto maybe_outfile = arrow::io::FileOutputStream::Open("test/fixtures/list_payloads.parquet");
    auto outfile = *maybe_outfile;
    // store_schema() is REQUIRED here, and only for `big`. Parquet's own format has no
    // LIST/LARGE_LIST distinction -- both are the same 3-level list encoding -- so without the
    // stored Arrow schema a large_list column reads back as a plain list and the LARGE_LIST arm
    // stays the permanently-dead code it has always been. Measured directly: the same column
    // round-trips as `list<element: int32>` without it and as `large_list<element: int32>` with
    // it. Every other column in this file is unaffected either way (int8, uint32, time64[us] and
    // timestamp[ms] all already survive without it), so this costs nothing but the one property
    // it buys. Contrast list_widths.parquet, which must NOT store its schema: there the point is
    // to exercise the plain-LIST path, and a stored schema would round-trip a fixed_size_list as
    // a fixed_size_list.
    auto arrow_props = parquet::ArrowWriterProperties::Builder().store_schema()->build();
    auto status = parquet::arrow::WriteTable(*table, arrow::default_memory_pool(), outfile, kRowGroup,
        parquet::default_writer_properties(), arrow_props);
    return status.ok();
}

// test/fixtures/no_stats.parquet: a Null-carrying column written with column statistics
// DISABLED, which is the one case parquet_column_has_nulls cannot answer from the footer.
//
// Statistics are optional in the Parquet format. parquet_table asks for a column's null count from
// them so it can skip building a validity mask for a Null-free column, and when they are absent it
// must fall back to "might have Nulls" and request the mask anyway. Getting that fallback backwards
// is not a silent error -- parquet_read_column aborts on an unexpected Null -- but it IS
// unreachable with any other fixture here, because every other one carries statistics. Hence this
// file: one column with Nulls, one without, no statistics on either.
//
//   v: double, Nulls at rows 2 and 5   -> must still read back with its Nulls intact
//   c: double, no Nulls                -> the clean control in the same statistics-free file
//
// Used by test/test_table.f90's statistics-free fallback test.
static bool generate_no_stats_fixture()
{
    arrow::DoubleBuilder v_builder;
    arrow::Status st;
    for (int row = 1; row <= 6; ++row)
    {
        if (row == 2 || row == 5)
        {
            st = v_builder.AppendNull();
        }
        else
        {
            st = v_builder.Append(static_cast<double>(row) * 1.5);
        }
    }
    std::shared_ptr<arrow::Array> v_arr;
    st = v_builder.Finish(&v_arr);

    arrow::DoubleBuilder c_builder;
    for (int row = 1; row <= 6; ++row)
    {
        st = c_builder.Append(static_cast<double>(row) * 100.0);
    }
    std::shared_ptr<arrow::Array> c_arr;
    st = c_builder.Finish(&c_arr);

    auto schema = arrow::schema({arrow::field("v", arrow::float64()), arrow::field("c", arrow::float64())});
    auto table = arrow::Table::Make(schema, {v_arr, c_arr});

    parquet::WriterProperties::Builder props_builder;
    props_builder.disable_statistics();
    auto props = props_builder.build();

    auto maybe_outfile = arrow::io::FileOutputStream::Open("test/fixtures/no_stats.parquet");
    auto outfile = *maybe_outfile;
    auto status = parquet::arrow::WriteTable(*table, arrow::default_memory_pool(), outfile, 3, props);
    return status.ok();
}

// test/fixtures/extended_types.parquet: exercises the read-time widening
// support for Arrow physical types this library's own writer never
// produces (see CONTRIBUTING.md's "Additional scalar types" note and
// doc/pages/types/supported-data-types.md) -- INT8/16, UINT8/16/32/64,
// HALF_FLOAT, and DECIMAL32/64/128/256. 3 rows throughout (uniform column
// length is required within one Arrow Table); per `.claude/rules/testing.md`'s "sized/typed
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

// ---------------------------------------------------------------------------------------------
// test/fixtures/struct_payloads.parquet: one flat top-level STRUCT column per payload family,
// plus one carrying all nine at once, with the four null shapes that separate a struct's two
// independent null levels.
//
// **A new fixture was needed rather than reusing an existing one**, and the reason is worth
// recording: NO fixture in this repository held a flat top-level struct of scalars.
// nested_struct.parquet's `main` is struct<id, inner: struct<...>> -- one scalar field and one
// NESTED struct, i.e. a Phase 7 shape; map_list_types.parquet's three struct columns each contain
// a list, a map or another struct. The only flat structs anywhere (struct<x: int32, y: string>)
// are the ELEMENTS of list_of_struct and the VALUES of map_of_struct, neither of which is a
// top-level column and neither of which a struct read can address.
//
// Every column carries the same four rows, deliberately, so that a test can predict one column's
// null pattern from another's:
//
//   row 0  every field present, nothing null           -- the ordinary case
//   row 1  the STRUCT INSTANCE is null                 -- row-level nullness
//   row 2  the struct is present, field `v` is null    -- field-level nullness, one field
//   row 3  the struct is present and EVERY field null  -- NOT a null row, and the case a naive
//                                                         implementation gets wrong: it gives the
//                                                         same combined mask as row 1 for every
//                                                         field, and only the struct's own
//                                                         validity separates the two
//
// Two row groups (kRowGroup = 2), so the chunked read path is exercised against a foreign file
// rather than only against one this library wrote -- and the row-1/row-3 pair straddles the
// boundary, so a row-group-scoped read has to get the struct's own validity right in both halves.
//
// store_schema() is deliberately NOT used: measured, a struct round-trips through Parquet's own
// nested-group encoding with its field names, order and types intact without it (unlike
// large_list, which list_payloads.parquet needs it for). Leaving it off keeps this a check of the
// plain Parquet path.
static bool generate_struct_payloads_fixture()
{
    constexpr int kRows = 4;
    constexpr int kRowGroup = 2;

    // Which rows are what. `null_row(1)` is the absent struct instance; `null_v(2)` nulls the
    // value field alone; row 3 nulls every field while the struct itself stays present.
    auto null_row = [](int r) { return r == 1; };
    auto null_v = [](int r) { return r == 2 || r == 3; };
    auto null_tag = [](int r) { return r == 3; };

    arrow::Status st;

    // Builds struct<v: T, tag: string> with the null pattern above, calling `append(vb, row)` for
    // each present value.
    auto build = [&](std::shared_ptr<arrow::ArrayBuilder> vb, const std::function<void(int)> &append) {
        auto tagb = std::make_shared<arrow::StringBuilder>();
        auto type = arrow::struct_({arrow::field("v", vb->type()), arrow::field("tag", arrow::utf8())});
        arrow::StructBuilder builder(type, arrow::default_memory_pool(),
            {vb, std::static_pointer_cast<arrow::ArrayBuilder>(tagb)});
        arrow::Status inner;
        for (int row = 0; row < kRows; ++row)
        {
            if (null_row(row))
            {
                // StructBuilder::AppendNull cascades a null into every child, which is what
                // Parquet would have stored anyway -- its definition levels cannot encode "the
                // struct is absent but its field is present".
                inner = builder.AppendNull();
                continue;
            }
            inner = builder.Append();
            if (null_v(row)) { inner = vb->AppendNull(); } else { append(row); }
            if (null_tag(row)) { inner = tagb->AppendNull(); }
            else { inner = tagb->Append("r" + std::to_string(row)); }
        }
        std::shared_ptr<arrow::Array> out;
        inner = builder.Finish(&out);
        if (!inner.ok()) return std::shared_ptr<arrow::Array>();
        return out;
    };

    auto i32b = std::make_shared<arrow::Int32Builder>();
    auto s_int32 = build(i32b, [&](int r) { st = i32b->Append(r * 10); });
    auto i64b = std::make_shared<arrow::Int64Builder>();
    auto s_int64 = build(i64b, [&](int r) { st = i64b->Append(static_cast<int64_t>(r) * 1000000000LL); });
    auto f32b = std::make_shared<arrow::FloatBuilder>();
    auto s_float32 = build(f32b, [&](int r) { st = f32b->Append(static_cast<float>(r) + 0.5f); });
    auto f64b = std::make_shared<arrow::DoubleBuilder>();
    auto s_float64 = build(f64b, [&](int r) { st = f64b->Append(static_cast<double>(r) + 0.25); });
    auto bb = std::make_shared<arrow::BooleanBuilder>();
    auto s_bool = build(bb, [&](int r) { st = bb->Append(r % 2 == 0); });
    auto sb = std::make_shared<arrow::StringBuilder>();
    auto s_string = build(sb, [&](int r) { st = sb->Append(std::string(static_cast<size_t>(r) + 1, 'x')); });
    auto db = std::make_shared<arrow::Date32Builder>();
    auto s_date = build(db, [&](int r) { st = db->Append(19000 + r); });
    auto tb = std::make_shared<arrow::Time64Builder>(arrow::time64(arrow::TimeUnit::MICRO),
        arrow::default_memory_pool());
    auto s_time = build(tb, [&](int r) { st = tb->Append(3600000000LL * (r + 1)); });
    auto tsb = std::make_shared<arrow::TimestampBuilder>(arrow::timestamp(arrow::TimeUnit::MICRO),
        arrow::default_memory_pool());
    auto s_timestamp = build(tsb, [&](int r) { st = tsb->Append(1700000000000000LL + r * 1000000LL); });

    // s_mixed: all nine kinds as nine fields of ONE struct -- the case a per-family column cannot
    // reach, and the one most likely to expose a field-ordering defect. Same four rows: row 1 is
    // the absent instance, row 3 present with every field null.
    auto m_i32 = std::make_shared<arrow::Int32Builder>();
    auto m_i64 = std::make_shared<arrow::Int64Builder>();
    auto m_f32 = std::make_shared<arrow::FloatBuilder>();
    auto m_f64 = std::make_shared<arrow::DoubleBuilder>();
    auto m_b = std::make_shared<arrow::BooleanBuilder>();
    auto m_s = std::make_shared<arrow::StringBuilder>();
    auto m_d = std::make_shared<arrow::Date32Builder>();
    auto m_t = std::make_shared<arrow::Time64Builder>(arrow::time64(arrow::TimeUnit::MICRO),
        arrow::default_memory_pool());
    auto m_ts = std::make_shared<arrow::TimestampBuilder>(arrow::timestamp(arrow::TimeUnit::MICRO),
        arrow::default_memory_pool());
    auto mixed_type = arrow::struct_({
        arrow::field("a_i32", arrow::int32()),
        arrow::field("b_i64", arrow::int64()),
        arrow::field("c_f32", arrow::float32()),
        arrow::field("d_f64", arrow::float64()),
        arrow::field("e_bool", arrow::boolean()),
        arrow::field("f_str", arrow::utf8()),
        arrow::field("g_date", arrow::date32()),
        arrow::field("h_time", arrow::time64(arrow::TimeUnit::MICRO)),
        arrow::field("i_ts", arrow::timestamp(arrow::TimeUnit::MICRO)),
    });
    arrow::StructBuilder mixed_builder(mixed_type, arrow::default_memory_pool(),
        {m_i32, m_i64, m_f32, m_f64, m_b, m_s, m_d, m_t, m_ts});
    for (int row = 0; row < kRows; ++row)
    {
        if (null_row(row)) { st = mixed_builder.AppendNull(); continue; }
        st = mixed_builder.Append();
        if (null_tag(row))
        {
            st = m_i32->AppendNull(); st = m_i64->AppendNull(); st = m_f32->AppendNull();
            st = m_f64->AppendNull(); st = m_b->AppendNull();   st = m_s->AppendNull();
            st = m_d->AppendNull();   st = m_t->AppendNull();   st = m_ts->AppendNull();
            continue;
        }
        st = m_i32->Append(row * 10);
        st = m_i64->Append(static_cast<int64_t>(row) * 1000000000LL);
        st = m_f32->Append(static_cast<float>(row) + 0.5f);
        st = m_f64->Append(static_cast<double>(row) + 0.25);
        st = m_b->Append(row % 2 == 0);
        if (null_v(row)) { st = m_s->AppendNull(); } else { st = m_s->Append("m" + std::to_string(row)); }
        st = m_d->Append(19000 + row);
        st = m_t->Append(3600000000LL * (row + 1));
        st = m_ts->Append(1700000000000000LL + row * 1000000LL);
    }
    std::shared_ptr<arrow::Array> s_mixed;
    st = mixed_builder.Finish(&s_mixed);

    // rowid: a plain scalar column beside the structs, so a test can tell which rows it is
    // looking at without depending on any struct read having worked.
    arrow::Int32Builder ridb;
    for (int row = 0; row < kRows; ++row) st = ridb.Append(row);
    std::shared_ptr<arrow::Array> rowid_arr;
    st = ridb.Finish(&rowid_arr);

    auto schema = arrow::schema({
        arrow::field("rowid", arrow::int32()),
        arrow::field("s_int32", s_int32->type()),
        arrow::field("s_int64", s_int64->type()),
        arrow::field("s_float32", s_float32->type()),
        arrow::field("s_float64", s_float64->type()),
        arrow::field("s_bool", s_bool->type()),
        arrow::field("s_string", s_string->type()),
        arrow::field("s_date", s_date->type()),
        arrow::field("s_time", s_time->type()),
        arrow::field("s_timestamp", s_timestamp->type()),
        arrow::field("s_mixed", mixed_type),
    });
    auto table = arrow::Table::Make(schema, {rowid_arr, s_int32, s_int64, s_float32, s_float64,
        s_bool, s_string, s_date, s_time, s_timestamp, s_mixed});

    auto maybe_outfile = arrow::io::FileOutputStream::Open("test/fixtures/struct_payloads.parquet");
    if (!maybe_outfile.ok()) return false;
    auto outfile = *maybe_outfile;
    auto status = parquet::arrow::WriteTable(*table, arrow::default_memory_pool(), outfile, kRowGroup);
    return status.ok();
}

// test/fixtures/map_payloads.parquet: one flat top-level MAP column per value family, plus one
// carrying duplicate keys and one keyed by int32 that exists only to be refused.
//
// **A new fixture was needed rather than reusing map_list_types.parquet**, and the reason is the
// same shape as struct_payloads.parquet's: that file's `map_col` is one map<string,int32> with 3
// rows in 1 row group, and its purpose is the NESTED type matrix -- every other map in it has a
// container value and belongs to Phase 7. It covers one of nine value families, one row group,
// and none of the four null shapes.
//
// Every column carries the same four rows, deliberately, so that a test can predict one column's
// null pattern from another's:
//
//   row 0  two entries, both values present    -- the ordinary case
//   row 1  the MAP ROW is null                 -- row-level nullness
//   row 2  the map is PRESENT but EMPTY        -- NOT a null row, and the pair that a naive
//                                                 implementation conflates: both report size 0,
//                                                 and only %is_null separates them
//   row 3  one entry whose VALUE is null       -- value-level nullness, with the key present
//
// Two row groups (kRowGroup = 2), so the chunked read path is exercised against a foreign file
// rather than only against one this library wrote -- and the row-1/row-2 pair straddles the
// boundary, so a row-group-scoped read has to get the null/empty distinction right in both halves.
//
// `m_dup` carries DUPLICATE KEYS in a deliberate order ("a", "b", "a"), which is what pins three
// separate promises at once: that the format preserves duplicates, that %get returns the FIRST
// match, and that occurrence= reaches the later one. Nothing else in the repository asserts the
// ORDER a map's entries come back in.
//
// `m_intkey` is a map<int32,int32> and exists only to be REFUSED. V1 keys are strings; this is
// the fixture the refusal is asserted against, and when non-string keys are ever supported the
// test that names it becomes a positive read test rather than being deleted.
static bool generate_map_payloads_fixture()
{
    constexpr int kRows = 4;
    constexpr int kRowGroup = 2;

    // Which rows are what. Row 1 is the absent map; row 2 is present but empty; row 3 holds one
    // entry whose value is null.
    auto null_row = [](int r) { return r == 1; };
    auto empty_row = [](int r) { return r == 2; };
    auto null_value_row = [](int r) { return r == 3; };

    arrow::Status st;

    // Builds map<string, T> with the null pattern above. `append(row, n)` appends the n-th value
    // of row `row` to the item builder; rows 0 gets two entries, row 3 gets one (null) value.
    auto build = [&](std::shared_ptr<arrow::ArrayBuilder> ib, const std::function<void(int, int)> &append) {
        auto kb = std::make_shared<arrow::StringBuilder>();
        arrow::MapBuilder builder(arrow::default_memory_pool(),
            std::static_pointer_cast<arrow::ArrayBuilder>(kb), ib);
        arrow::Status inner;
        for (int row = 0; row < kRows; ++row)
        {
            if (null_row(row)) { inner = builder.AppendNull(); continue; }
            inner = builder.Append();
            if (empty_row(row)) continue;
            if (null_value_row(row))
            {
                inner = kb->Append("solo");
                inner = ib->AppendNull();
                continue;
            }
            inner = kb->Append("alpha");
            append(row, 0);
            inner = kb->Append("beta");
            append(row, 1);
        }
        std::shared_ptr<arrow::Array> out;
        inner = builder.Finish(&out);
        if (!inner.ok()) return std::shared_ptr<arrow::Array>();
        return out;
    };

    auto i32b = std::make_shared<arrow::Int32Builder>();
    auto m_int32 = build(i32b, [&](int r, int n) { st = i32b->Append(r * 10 + n); });
    auto i64b = std::make_shared<arrow::Int64Builder>();
    auto m_int64 = build(i64b, [&](int r, int n) {
        st = i64b->Append(static_cast<int64_t>(r) * 1000000000LL + n); });
    auto f32b = std::make_shared<arrow::FloatBuilder>();
    auto m_float32 = build(f32b, [&](int r, int n) {
        st = f32b->Append(static_cast<float>(r) + 0.5f * static_cast<float>(n + 1)); });
    auto f64b = std::make_shared<arrow::DoubleBuilder>();
    auto m_float64 = build(f64b, [&](int r, int n) {
        st = f64b->Append(static_cast<double>(r) + 0.25 * static_cast<double>(n + 1)); });
    auto bb = std::make_shared<arrow::BooleanBuilder>();
    auto m_bool = build(bb, [&](int r, int n) { st = bb->Append((r + n) % 2 == 0); });
    auto sb = std::make_shared<arrow::StringBuilder>();
    auto m_string = build(sb, [&](int r, int n) {
        st = sb->Append(std::string(static_cast<size_t>(r + n) + 1, 'x')); });
    auto db = std::make_shared<arrow::Date32Builder>();
    auto m_date = build(db, [&](int r, int n) { st = db->Append(19000 + r * 10 + n); });
    auto tb = std::make_shared<arrow::Time64Builder>(arrow::time64(arrow::TimeUnit::MICRO),
        arrow::default_memory_pool());
    auto m_time = build(tb, [&](int r, int n) { st = tb->Append(3600000000LL * (r + 1) + n); });
    auto tsb = std::make_shared<arrow::TimestampBuilder>(arrow::timestamp(arrow::TimeUnit::MICRO),
        arrow::default_memory_pool());
    auto m_timestamp = build(tsb, [&](int r, int n) {
        st = tsb->Append(1700000000000000LL + r * 1000000LL + n); });

    // m_dup: row 0 carries "a" -> 1, "b" -> 2, "a" -> 3, in that order. The other three rows keep
    // this fixture's shared null pattern so that a test can compare against any other column.
    auto dkb = std::make_shared<arrow::StringBuilder>();
    auto dib = std::make_shared<arrow::Int32Builder>();
    arrow::MapBuilder dup_builder(arrow::default_memory_pool(),
        std::static_pointer_cast<arrow::ArrayBuilder>(dkb),
        std::static_pointer_cast<arrow::ArrayBuilder>(dib));
    for (int row = 0; row < kRows; ++row)
    {
        if (null_row(row)) { st = dup_builder.AppendNull(); continue; }
        st = dup_builder.Append();
        if (empty_row(row)) continue;
        if (null_value_row(row)) { st = dkb->Append("solo"); st = dib->AppendNull(); continue; }
        st = dkb->Append("a"); st = dib->Append(1);
        st = dkb->Append("b"); st = dib->Append(2);
        st = dkb->Append("a"); st = dib->Append(3);
    }
    std::shared_ptr<arrow::Array> m_dup;
    st = dup_builder.Finish(&m_dup);

    // m_intkey: a map keyed by int32, present only so that the string-keys-only refusal has
    // something to refuse. Two entries in row 0, nothing exotic.
    auto ikb = std::make_shared<arrow::Int32Builder>();
    auto iib = std::make_shared<arrow::Int32Builder>();
    arrow::MapBuilder intkey_builder(arrow::default_memory_pool(),
        std::static_pointer_cast<arrow::ArrayBuilder>(ikb),
        std::static_pointer_cast<arrow::ArrayBuilder>(iib));
    for (int row = 0; row < kRows; ++row)
    {
        if (null_row(row)) { st = intkey_builder.AppendNull(); continue; }
        st = intkey_builder.Append();
        if (empty_row(row)) continue;
        st = ikb->Append(row * 100); st = iib->Append(row);
    }
    std::shared_ptr<arrow::Array> m_intkey;
    st = intkey_builder.Finish(&m_intkey);

    // rowid: a plain scalar column beside the maps, so a test can tell which rows it is looking at
    // without depending on any map read having worked.
    arrow::Int32Builder ridb;
    for (int row = 0; row < kRows; ++row) st = ridb.Append(row);
    std::shared_ptr<arrow::Array> rowid_arr;
    st = ridb.Finish(&rowid_arr);

    auto schema = arrow::schema({
        arrow::field("rowid", arrow::int32()),
        arrow::field("m_int32", m_int32->type()),
        arrow::field("m_int64", m_int64->type()),
        arrow::field("m_float32", m_float32->type()),
        arrow::field("m_float64", m_float64->type()),
        arrow::field("m_bool", m_bool->type()),
        arrow::field("m_string", m_string->type()),
        arrow::field("m_date", m_date->type()),
        arrow::field("m_time", m_time->type()),
        arrow::field("m_timestamp", m_timestamp->type()),
        arrow::field("m_dup", m_dup->type()),
        arrow::field("m_intkey", m_intkey->type()),
    });
    auto table = arrow::Table::Make(schema, {rowid_arr, m_int32, m_int64, m_float32, m_float64,
        m_bool, m_string, m_date, m_time, m_timestamp, m_dup, m_intkey});

    auto maybe_outfile = arrow::io::FileOutputStream::Open("test/fixtures/map_payloads.parquet");
    if (!maybe_outfile.ok()) return false;
    auto outfile = *maybe_outfile;
    auto status = parquet::arrow::WriteTable(*table, arrow::default_memory_pool(), outfile, kRowGroup);
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

// test/fixtures/map_list_types.parquet: exercises Arrow's MAP and (variable-length)
// LIST types -- neither is supported by this library yet (see CLAUDE.md's
// "Reserved for future element-domain work": parquet_map/parquet_list), so
// this fixture exists purely to feed tools/parquet_metadata_to_md.py while
// that tool's nested-schema rendering is developed, not to be read by this
// library or exercised by any Fortran test.
//
// Covers LIST and MAP both as independent (non-nested) columns and nested
// inside every combination of list/struct/map one level deep, plus one
// deliberately deeper example combining all three. 3 rows throughout; row 2
// is deliberately the "smallest" case (empty containers, never a whole-row
// null -- Parquet's LIST/MAP nesting has its own null-vs-empty subtleties
// this fixture isn't trying to exercise) and row 3 the largest, so nothing
// here is only ever exercised at length 1.
//
//   list_col:         list<int32>
//   map_col:          map<string,int32>
//   list_of_list:     list<list<int32>>
//   list_of_struct:   list<struct<x:int32,y:string>>
//   list_of_map:      list<map<string,int32>>
//   struct_of_list:   struct<label:string, values:list<int32>>
//   struct_of_struct: struct<label:string, inner:struct<a:int32,b:string>>
//   struct_of_map:    struct<label:string, attrs:map<string,int32>>
//   map_of_list:      map<string,list<int32>>
//   map_of_struct:    map<string,struct<x:int32,y:string>>
//   map_of_map:       map<string,map<string,int32>>
//   deep_nested:      list<struct<name:string, tags:list<string>, meta:map<string,int32>>>
static bool generate_map_list_types_fixture()
{
    arrow::Status st;

    // list_col: list<int32> -- [1,2], [], [3,4,5]
    auto list_col_values = std::make_shared<arrow::Int32Builder>();
    arrow::ListBuilder list_col_builder(arrow::default_memory_pool(), list_col_values);
    st = list_col_builder.Append();
    st = list_col_values->Append(1);
    st = list_col_values->Append(2);
    st = list_col_builder.Append();
    st = list_col_builder.Append();
    st = list_col_values->Append(3);
    st = list_col_values->Append(4);
    st = list_col_values->Append(5);
    std::shared_ptr<arrow::Array> list_col_arr;
    st = list_col_builder.Finish(&list_col_arr);

    // map_col: map<string,int32> -- {"a":1}, {}, {"b":2,"c":3}
    auto map_col_keys = std::make_shared<arrow::StringBuilder>();
    auto map_col_items = std::make_shared<arrow::Int32Builder>();
    arrow::MapBuilder map_col_builder(arrow::default_memory_pool(), map_col_keys, map_col_items);
    st = map_col_builder.Append();
    st = map_col_keys->Append("a");
    st = map_col_items->Append(1);
    st = map_col_builder.Append();
    st = map_col_builder.Append();
    st = map_col_keys->Append("b");
    st = map_col_items->Append(2);
    st = map_col_keys->Append("c");
    st = map_col_items->Append(3);
    std::shared_ptr<arrow::Array> map_col_arr;
    st = map_col_builder.Finish(&map_col_arr);

    // list_of_list: list<list<int32>> -- [[1,2],[3]], [], [[4],[5,6],[]]
    auto lol_inner_values = std::make_shared<arrow::Int32Builder>();
    auto lol_inner_builder = std::make_shared<arrow::ListBuilder>(arrow::default_memory_pool(), lol_inner_values);
    arrow::ListBuilder lol_builder(arrow::default_memory_pool(), lol_inner_builder);
    st = lol_builder.Append();
    st = lol_inner_builder->Append();
    st = lol_inner_values->Append(1);
    st = lol_inner_values->Append(2);
    st = lol_inner_builder->Append();
    st = lol_inner_values->Append(3);
    st = lol_builder.Append();
    st = lol_builder.Append();
    st = lol_inner_builder->Append();
    st = lol_inner_values->Append(4);
    st = lol_inner_builder->Append();
    st = lol_inner_values->Append(5);
    st = lol_inner_values->Append(6);
    st = lol_inner_builder->Append();
    std::shared_ptr<arrow::Array> lol_arr;
    st = lol_builder.Finish(&lol_arr);

    // list_of_struct: list<struct<x:int32,y:string>> -- [{1,"a"}], [], [{2,"b"},{3,"c"}]
    auto los_x = std::make_shared<arrow::Int32Builder>();
    auto los_y = std::make_shared<arrow::StringBuilder>();
    auto los_struct_type = arrow::struct_({arrow::field("x", arrow::int32()), arrow::field("y", arrow::utf8())});
    auto los_struct_builder = std::make_shared<arrow::StructBuilder>(los_struct_type, arrow::default_memory_pool(),
        std::vector<std::shared_ptr<arrow::ArrayBuilder>>{los_x, los_y});
    arrow::ListBuilder los_builder(arrow::default_memory_pool(), los_struct_builder);
    st = los_builder.Append();
    st = los_struct_builder->Append();
    st = los_x->Append(1);
    st = los_y->Append("a");
    st = los_builder.Append();
    st = los_builder.Append();
    st = los_struct_builder->Append();
    st = los_x->Append(2);
    st = los_y->Append("b");
    st = los_struct_builder->Append();
    st = los_x->Append(3);
    st = los_y->Append("c");
    std::shared_ptr<arrow::Array> los_arr;
    st = los_builder.Finish(&los_arr);

    // list_of_map: list<map<string,int32>> -- [{"k":1}], [], [{"k":2},{"m":3}]
    auto lom_keys = std::make_shared<arrow::StringBuilder>();
    auto lom_items = std::make_shared<arrow::Int32Builder>();
    auto lom_map_builder = std::make_shared<arrow::MapBuilder>(arrow::default_memory_pool(), lom_keys, lom_items);
    arrow::ListBuilder lom_builder(arrow::default_memory_pool(), lom_map_builder);
    st = lom_builder.Append();
    st = lom_map_builder->Append();
    st = lom_keys->Append("k");
    st = lom_items->Append(1);
    st = lom_builder.Append();
    st = lom_builder.Append();
    st = lom_map_builder->Append();
    st = lom_keys->Append("k");
    st = lom_items->Append(2);
    st = lom_map_builder->Append();
    st = lom_keys->Append("m");
    st = lom_items->Append(3);
    std::shared_ptr<arrow::Array> lom_arr;
    st = lom_builder.Finish(&lom_arr);

    // struct_of_list: struct<label:string, values:list<int32>>
    //   {"first",[1,2,3]}, {"second",[]}, {"third",[4,5]}
    auto sol_label = std::make_shared<arrow::StringBuilder>();
    auto sol_values_values = std::make_shared<arrow::Int32Builder>();
    auto sol_values_builder = std::make_shared<arrow::ListBuilder>(arrow::default_memory_pool(), sol_values_values);
    auto sol_type = arrow::struct_(
        {arrow::field("label", arrow::utf8()), arrow::field("values", arrow::list(arrow::int32()))});
    arrow::StructBuilder sol_builder(sol_type, arrow::default_memory_pool(),
        std::vector<std::shared_ptr<arrow::ArrayBuilder>>{sol_label, sol_values_builder});
    st = sol_builder.Append();
    st = sol_label->Append("first");
    st = sol_values_builder->Append();
    st = sol_values_values->Append(1);
    st = sol_values_values->Append(2);
    st = sol_values_values->Append(3);
    st = sol_builder.Append();
    st = sol_label->Append("second");
    st = sol_values_builder->Append();
    st = sol_builder.Append();
    st = sol_label->Append("third");
    st = sol_values_builder->Append();
    st = sol_values_values->Append(4);
    st = sol_values_values->Append(5);
    std::shared_ptr<arrow::Array> sol_arr;
    st = sol_builder.Finish(&sol_arr);

    // struct_of_struct: struct<label:string, inner:struct<a:int32,b:string>>
    //   {"p",{1,"x"}}, {"q",{2,"y"}}, {"r",{3,"z"}}
    auto sos_a = std::make_shared<arrow::Int32Builder>();
    auto sos_b = std::make_shared<arrow::StringBuilder>();
    auto sos_inner_type = arrow::struct_({arrow::field("a", arrow::int32()), arrow::field("b", arrow::utf8())});
    auto sos_inner_builder = std::make_shared<arrow::StructBuilder>(sos_inner_type, arrow::default_memory_pool(),
        std::vector<std::shared_ptr<arrow::ArrayBuilder>>{sos_a, sos_b});
    auto sos_label = std::make_shared<arrow::StringBuilder>();
    auto sos_type = arrow::struct_({arrow::field("label", arrow::utf8()), arrow::field("inner", sos_inner_type)});
    arrow::StructBuilder sos_builder(sos_type, arrow::default_memory_pool(),
        std::vector<std::shared_ptr<arrow::ArrayBuilder>>{sos_label, sos_inner_builder});
    st = sos_builder.Append();
    st = sos_label->Append("p");
    st = sos_inner_builder->Append();
    st = sos_a->Append(1);
    st = sos_b->Append("x");
    st = sos_builder.Append();
    st = sos_label->Append("q");
    st = sos_inner_builder->Append();
    st = sos_a->Append(2);
    st = sos_b->Append("y");
    st = sos_builder.Append();
    st = sos_label->Append("r");
    st = sos_inner_builder->Append();
    st = sos_a->Append(3);
    st = sos_b->Append("z");
    std::shared_ptr<arrow::Array> sos_arr;
    st = sos_builder.Finish(&sos_arr);

    // struct_of_map: struct<label:string, attrs:map<string,int32>>
    //   {"m1",{"a":1}}, {"m2",{}}, {"m3",{"b":2,"c":3}}
    auto som_label = std::make_shared<arrow::StringBuilder>();
    auto som_attrs_keys = std::make_shared<arrow::StringBuilder>();
    auto som_attrs_items = std::make_shared<arrow::Int32Builder>();
    auto som_attrs_builder =
        std::make_shared<arrow::MapBuilder>(arrow::default_memory_pool(), som_attrs_keys, som_attrs_items);
    auto som_type = arrow::struct_(
        {arrow::field("label", arrow::utf8()), arrow::field("attrs", arrow::map(arrow::utf8(), arrow::int32()))});
    arrow::StructBuilder som_builder(som_type, arrow::default_memory_pool(),
        std::vector<std::shared_ptr<arrow::ArrayBuilder>>{som_label, som_attrs_builder});
    st = som_builder.Append();
    st = som_label->Append("m1");
    st = som_attrs_builder->Append();
    st = som_attrs_keys->Append("a");
    st = som_attrs_items->Append(1);
    st = som_builder.Append();
    st = som_label->Append("m2");
    st = som_attrs_builder->Append();
    st = som_builder.Append();
    st = som_label->Append("m3");
    st = som_attrs_builder->Append();
    st = som_attrs_keys->Append("b");
    st = som_attrs_items->Append(2);
    st = som_attrs_keys->Append("c");
    st = som_attrs_items->Append(3);
    std::shared_ptr<arrow::Array> som_arr;
    st = som_builder.Finish(&som_arr);

    // map_of_list: map<string,list<int32>> -- {"a":[1,2]}, {}, {"b":[3],"c":[4,5]}
    auto mol_keys = std::make_shared<arrow::StringBuilder>();
    auto mol_inner_values = std::make_shared<arrow::Int32Builder>();
    auto mol_inner_builder = std::make_shared<arrow::ListBuilder>(arrow::default_memory_pool(), mol_inner_values);
    arrow::MapBuilder mol_builder(arrow::default_memory_pool(), mol_keys, mol_inner_builder);
    st = mol_builder.Append();
    st = mol_keys->Append("a");
    st = mol_inner_builder->Append();
    st = mol_inner_values->Append(1);
    st = mol_inner_values->Append(2);
    st = mol_builder.Append();
    st = mol_builder.Append();
    st = mol_keys->Append("b");
    st = mol_inner_builder->Append();
    st = mol_inner_values->Append(3);
    st = mol_keys->Append("c");
    st = mol_inner_builder->Append();
    st = mol_inner_values->Append(4);
    st = mol_inner_values->Append(5);
    std::shared_ptr<arrow::Array> mol_arr;
    st = mol_builder.Finish(&mol_arr);

    // map_of_struct: map<string,struct<x:int32,y:string>>
    //   {"a":{1,"foo"}}, {}, {"b":{2,"bar"},"c":{3,"baz"}}
    auto mos_keys = std::make_shared<arrow::StringBuilder>();
    auto mos_x = std::make_shared<arrow::Int32Builder>();
    auto mos_y = std::make_shared<arrow::StringBuilder>();
    auto mos_struct_type = arrow::struct_({arrow::field("x", arrow::int32()), arrow::field("y", arrow::utf8())});
    auto mos_item_builder = std::make_shared<arrow::StructBuilder>(mos_struct_type, arrow::default_memory_pool(),
        std::vector<std::shared_ptr<arrow::ArrayBuilder>>{mos_x, mos_y});
    arrow::MapBuilder mos_builder(arrow::default_memory_pool(), mos_keys, mos_item_builder);
    st = mos_builder.Append();
    st = mos_keys->Append("a");
    st = mos_item_builder->Append();
    st = mos_x->Append(1);
    st = mos_y->Append("foo");
    st = mos_builder.Append();
    st = mos_builder.Append();
    st = mos_keys->Append("b");
    st = mos_item_builder->Append();
    st = mos_x->Append(2);
    st = mos_y->Append("bar");
    st = mos_keys->Append("c");
    st = mos_item_builder->Append();
    st = mos_x->Append(3);
    st = mos_y->Append("baz");
    std::shared_ptr<arrow::Array> mos_arr;
    st = mos_builder.Finish(&mos_arr);

    // map_of_map: map<string,map<string,int32>>
    //   {"outer1":{"inner1":1}}, {}, {"outer2":{"inner2":2,"inner3":3}}
    auto mom_outer_keys = std::make_shared<arrow::StringBuilder>();
    auto mom_inner_keys = std::make_shared<arrow::StringBuilder>();
    auto mom_inner_items = std::make_shared<arrow::Int32Builder>();
    auto mom_inner_builder =
        std::make_shared<arrow::MapBuilder>(arrow::default_memory_pool(), mom_inner_keys, mom_inner_items);
    arrow::MapBuilder mom_builder(arrow::default_memory_pool(), mom_outer_keys, mom_inner_builder);
    st = mom_builder.Append();
    st = mom_outer_keys->Append("outer1");
    st = mom_inner_builder->Append();
    st = mom_inner_keys->Append("inner1");
    st = mom_inner_items->Append(1);
    st = mom_builder.Append();
    st = mom_builder.Append();
    st = mom_outer_keys->Append("outer2");
    st = mom_inner_builder->Append();
    st = mom_inner_keys->Append("inner2");
    st = mom_inner_items->Append(2);
    st = mom_inner_keys->Append("inner3");
    st = mom_inner_items->Append(3);
    std::shared_ptr<arrow::Array> mom_arr;
    st = mom_builder.Finish(&mom_arr);

    // deep_nested: list<struct<name:string, tags:list<string>, meta:map<string,int32>>>
    //   [{"n1",["t1","t2"],{"k1":1}}], [], [{"n2",["t3"],{"k2":2,"k3":3}}, {"n3",[],{}}]
    auto dn_name = std::make_shared<arrow::StringBuilder>();
    auto dn_tags_values = std::make_shared<arrow::StringBuilder>();
    auto dn_tags_builder = std::make_shared<arrow::ListBuilder>(arrow::default_memory_pool(), dn_tags_values);
    auto dn_meta_keys = std::make_shared<arrow::StringBuilder>();
    auto dn_meta_items = std::make_shared<arrow::Int32Builder>();
    auto dn_meta_builder =
        std::make_shared<arrow::MapBuilder>(arrow::default_memory_pool(), dn_meta_keys, dn_meta_items);
    auto dn_struct_type = arrow::struct_({
        arrow::field("name", arrow::utf8()),
        arrow::field("tags", arrow::list(arrow::utf8())),
        arrow::field("meta", arrow::map(arrow::utf8(), arrow::int32())),
    });
    auto dn_struct_builder = std::make_shared<arrow::StructBuilder>(dn_struct_type, arrow::default_memory_pool(),
        std::vector<std::shared_ptr<arrow::ArrayBuilder>>{dn_name, dn_tags_builder, dn_meta_builder});
    arrow::ListBuilder dn_builder(arrow::default_memory_pool(), dn_struct_builder);

    st = dn_builder.Append();
    st = dn_struct_builder->Append();
    st = dn_name->Append("n1");
    st = dn_tags_builder->Append();
    st = dn_tags_values->Append("t1");
    st = dn_tags_values->Append("t2");
    st = dn_meta_builder->Append();
    st = dn_meta_keys->Append("k1");
    st = dn_meta_items->Append(1);

    st = dn_builder.Append();  // row 2: []

    st = dn_builder.Append();
    st = dn_struct_builder->Append();
    st = dn_name->Append("n2");
    st = dn_tags_builder->Append();
    st = dn_tags_values->Append("t3");
    st = dn_meta_builder->Append();
    st = dn_meta_keys->Append("k2");
    st = dn_meta_items->Append(2);
    st = dn_meta_keys->Append("k3");
    st = dn_meta_items->Append(3);
    st = dn_struct_builder->Append();
    st = dn_name->Append("n3");
    st = dn_tags_builder->Append();
    st = dn_meta_builder->Append();

    std::shared_ptr<arrow::Array> dn_arr;
    st = dn_builder.Finish(&dn_arr);

    auto schema = arrow::schema({
        arrow::field("list_col", arrow::list(arrow::int32()), false),
        arrow::field("map_col", arrow::map(arrow::utf8(), arrow::int32()), false),
        arrow::field("list_of_list", arrow::list(arrow::list(arrow::int32())), false),
        arrow::field("list_of_struct", arrow::list(los_struct_type), false),
        arrow::field("list_of_map", arrow::list(arrow::map(arrow::utf8(), arrow::int32())), false),
        arrow::field("struct_of_list", sol_type, false),
        arrow::field("struct_of_struct", sos_type, false),
        arrow::field("struct_of_map", som_type, false),
        arrow::field("map_of_list", arrow::map(arrow::utf8(), arrow::list(arrow::int32())), false),
        arrow::field("map_of_struct", arrow::map(arrow::utf8(), mos_struct_type), false),
        arrow::field("map_of_map", arrow::map(arrow::utf8(), arrow::map(arrow::utf8(), arrow::int32())), false),
        arrow::field("deep_nested", arrow::list(dn_struct_type), false),
    });
    auto table = arrow::Table::Make(schema, {
        list_col_arr, map_col_arr, lol_arr, los_arr, lom_arr,
        sol_arr, sos_arr, som_arr, mol_arr, mos_arr, mom_arr, dn_arr,
    });

    auto maybe_outfile = arrow::io::FileOutputStream::Open("test/fixtures/map_list_types.parquet");
    auto outfile = *maybe_outfile;
    auto status = parquet::arrow::WriteTable(*table, arrow::default_memory_pool(), outfile, 3);
    return status.ok();
}

// test/fixtures/element_nulls.parquet: vector (fixed_size_list) columns whose nulls sit on
// INDIVIDUAL ELEMENTS rather than on whole rows, written by Arrow directly.
//
// Why it has to come from here rather than from this library's own writer: every other
// element-null test writes its fixture with parquet-fortran and reads it back, so a bug that
// widened a null on read AND broadcast it on write would be perfectly self-consistent and
// invisible. This file is the independent statement of what the read path must produce.
//
// One column per validity dispatch class, since they are three different mechanisms behind one
// API (a packed bitmap, the embedded string column, the null inside each element):
//   id    int32 scalar, no nulls                  -- orientation
//   vec   fixed_size_list<double, 3>              -- row 2, element 2 is null (bitmap class)
//   svec  fixed_size_list<string, 2>              -- row 3, element 1 is null (string class)
//   tvec  fixed_size_list<timestamp[us], 2>       -- row 1, element 2 is null (temporal class)
// Every other element of every row is a real value, so a test can assert that the null landed on
// exactly one element and that its siblings survived.
static bool generate_element_nulls_fixture()
{
    arrow::Int32Builder id_builder;
    auto st = id_builder.AppendValues({1, 2, 3, 4});
    std::shared_ptr<arrow::Array> id_arr;
    st = id_builder.Finish(&id_arr);

    // double vector, width 3: null at (row 2, element 2) only.
    auto vec_values = std::make_shared<arrow::DoubleBuilder>();
    arrow::FixedSizeListBuilder vec_builder(arrow::default_memory_pool(), vec_values, 3);
    for (int row = 0; row < 4; ++row)
    {
        st = vec_builder.Append();
        for (int e = 0; e < 3; ++e)
        {
            if (row == 1 && e == 1)
            {
                st = vec_values->AppendNull();
            }
            else
            {
                st = vec_values->Append(10.0 * (row + 1) + (e + 1));
            }
        }
    }
    std::shared_ptr<arrow::Array> vec_arr;
    st = vec_builder.Finish(&vec_arr);

    // string vector, width 2: null at (row 3, element 1) only. First element deliberately the
    // shortest, per this project's "sized from the first element" convention.
    auto svec_values = std::make_shared<arrow::StringBuilder>();
    arrow::FixedSizeListBuilder svec_builder(arrow::default_memory_pool(), svec_values, 2);
    const char *words[4][2] = {{"a", "bbbb"}, {"cc", "ddddd"}, {"e", "ff"}, {"gggg", "h"}};
    for (int row = 0; row < 4; ++row)
    {
        st = svec_builder.Append();
        for (int e = 0; e < 2; ++e)
        {
            if (row == 2 && e == 0)
            {
                st = svec_values->AppendNull();
            }
            else
            {
                st = svec_values->Append(words[row][e]);
            }
        }
    }
    std::shared_ptr<arrow::Array> svec_arr;
    st = svec_builder.Finish(&svec_arr);

    // timestamp vector, width 2: null at (row 1, element 2) only.
    auto ts_type = arrow::timestamp(arrow::TimeUnit::MICRO);
    auto tvec_values = std::make_shared<arrow::TimestampBuilder>(ts_type, arrow::default_memory_pool());
    arrow::FixedSizeListBuilder tvec_builder(arrow::default_memory_pool(), tvec_values, 2);
    for (int row = 0; row < 4; ++row)
    {
        st = tvec_builder.Append();
        for (int e = 0; e < 2; ++e)
        {
            if (row == 0 && e == 1)
            {
                st = tvec_values->AppendNull();
            }
            else
            {
                // 2024-01-01T00:00:00Z is 1704067200 s; add a distinct second per element.
                st = tvec_values->Append((1704067200LL + row * 2 + e) * 1000000LL);
            }
        }
    }
    std::shared_ptr<arrow::Array> tvec_arr;
    st = tvec_builder.Finish(&tvec_arr);

    // The LIST fields themselves are non-nullable while their elements are nullable: that is what
    // makes every null in this file an element null rather than a row null.
    auto schema = arrow::schema({
        arrow::field("id", arrow::int32(), false),
        arrow::field("vec", arrow::fixed_size_list(arrow::field("item", arrow::float64(), true), 3), false),
        arrow::field("svec", arrow::fixed_size_list(arrow::field("item", arrow::utf8(), true), 2), false),
        arrow::field("tvec", arrow::fixed_size_list(arrow::field("item", ts_type, true), 2), false),
    });
    auto table = arrow::Table::Make(schema, {id_arr, vec_arr, svec_arr, tvec_arr});

    auto maybe_outfile = arrow::io::FileOutputStream::Open("test/fixtures/element_nulls.parquet");
    auto outfile = *maybe_outfile;
    auto props = parquet::ArrowWriterProperties::Builder().store_schema()->build();
    auto status = parquet::arrow::WriteTable(*table, arrow::default_memory_pool(), outfile, 4,
                                             parquet::default_writer_properties(), props);
    return status.ok();
}

// test/fixtures/screen_declined_nulls.parquet: the row-group statistics screen's null tests, on
// column types the screen otherwise DECLINES to reason about.
//
// The point is a short-circuit that is easy to miss: `is_null`/`is_not_null` are answered from the
// chunk's recorded null count alone, so `resolve_screen_leaf` returns "usable" for them BEFORE it
// looks at the column's Arrow type and before the sort-order gate. That means they genuinely prune
// on a UINT32/DECIMAL/HALF_FLOAT column -- the very types every comparison operator declines. A
// change that moved the null tests below the type switch would silently stop pruning (slow, but
// correct), while one that moved the type switch's decline above them would silently prune wrongly
// (fast, and a wrong answer). Only a fixture with such a column AND real nulls can tell the two
// apart, and this library's own writer cannot produce any of these three types.
//
// 40 rows in 4 row groups of 10, statistics on. Each declined-type column is null in exactly ONE
// row group, and a different one per column, so the expected pruned count is unambiguous:
//   id        INT32,          1..40, never null   -- the screenable control
//   v_uint32  UINT32,         null in row group 2 (rows 11-20)
//   v_decimal DECIMAL128(10,2), null in row group 3 (rows 21-30)
//   v_half    HALF_FLOAT,     null in row group 4 (rows 31-40)
// So `v_uint32 is_null` can prune 3 row groups and `v_uint32 is_not_null` exactly 1, and likewise
// for the other two on their own row groups. Per `.claude/rules/testing.md`'s "Assertions"
// convention the nulls are deliberately never in row group 1.
static bool generate_screen_declined_nulls_fixture()
{
    const int nrows = 40;
    const int chunk = 10;
    arrow::Status st;

    arrow::Int32Builder id_builder;
    arrow::UInt32Builder uint32_builder;
    auto decimal_type = arrow::decimal128(10, 2);
    arrow::Decimal128Builder decimal_builder(decimal_type);
    arrow::HalfFloatBuilder half_builder;

    for (int row = 1; row <= nrows; ++row)
    {
        const int row_group = (row - 1) / chunk + 1;   // 1-based, matching the reader's numbering
        st = id_builder.Append(row);
        if (row_group == 2) st = uint32_builder.AppendNull();
        else st = uint32_builder.Append(static_cast<uint32_t>(row) * 100u);
        if (row_group == 3) st = decimal_builder.AppendNull();
        else st = decimal_builder.Append(arrow::Decimal128(int64_t{row} * 25));
        if (row_group == 4) st = half_builder.AppendNull();
        else st = half_builder.Append(arrow::util::Float16(static_cast<float>(row) * 0.5f).bits());
    }

    std::shared_ptr<arrow::Array> id_arr, uint32_arr, decimal_arr, half_arr;
    st = id_builder.Finish(&id_arr);
    st = uint32_builder.Finish(&uint32_arr);
    st = decimal_builder.Finish(&decimal_arr);
    st = half_builder.Finish(&half_arr);

    auto schema = arrow::schema({
        arrow::field("id", arrow::int32(), false),
        arrow::field("v_uint32", arrow::uint32()),
        arrow::field("v_decimal", decimal_type),
        arrow::field("v_half", arrow::float16()),
    });
    auto table = arrow::Table::Make(schema, {id_arr, uint32_arr, decimal_arr, half_arr});

    auto maybe_outfile = arrow::io::FileOutputStream::Open("test/fixtures/screen_declined_nulls.parquet");
    auto outfile = *maybe_outfile;
    auto status = parquet::arrow::WriteTable(*table, arrow::default_memory_pool(), outfile, chunk);
    return status.ok();
}

// test/fixtures/encoded_types.parquet: columns wearing Arrow ENCODING WRAPPERS -- an extension
// type, a dictionary type -- over storage of differing shapes.
//
// The point is the SHAPE queries, not the values. Arrow gives an extension/dictionary/run-end
// type its own Type::type, so a query that switches on the leaf type id without peeling the
// wrapper classifies the WRAPPER. That is not academic: an `arrow.fixed_shape_tensor` column has
// fixed_size_list storage, so its rows hold four values each, and it was reported as a "scalar"
// with col_size 1 -- telling a caller to allocate a 1-D array for a column needing a 2-D one.
// See unwrap_encoding_layers in src/parquet_wrapper.cpp.
//
// store_schema() is REQUIRED: the wrappers live in the Arrow schema, and without it every column
// here round-trips as its bare storage type and the fixture tests nothing.
//
//   plain       int32                                        -> ("int32",   "scalar") control
//   tensor_col  extension<arrow.fixed_shape_tensor>          -> ("unknown", "vector"), col_size 4
//               over fixed_size_list<int32>[4]
//   dict_col    dictionary<values=string, indices=int32>     -> ("string",  "scalar")
//               decoded to its values on read (decode_dictionary_chunks), so it reports the
//               VALUE type here where the extension column above still reports "unknown" --
//               that pair is what separates "peeled a dictionary" from "peeled everything"
//               (see test/fixtures/dictionary_types.parquet for the pandas-shaped cases)
//   vec_col     fixed_size_list<int32>[4], unwrapped         -> ("int32",   "vector"), col_size 4
//               the negative control: the same shape WITHOUT a wrapper, so a test can tell
//               "peeled correctly" from "happened to say vector anyway"
//
// Used by test/test_reading.f90's encoding-wrapper shape test.
static bool generate_encoded_types_fixture()
{
    arrow::Status st;
    const int kRows = 3;

    // The tensor column's storage, and an identical unwrapped twin as the control.
    auto make_fsl = [&](std::shared_ptr<arrow::Array> &out) {
        auto values = std::make_shared<arrow::Int32Builder>();
        arrow::FixedSizeListBuilder builder(arrow::default_memory_pool(), values, 4);
        for (int row = 0; row < kRows; ++row)
        {
            st = builder.Append();
            for (int k = 0; k < 4; ++k)
            {
                st = values->Append(row * 4 + k);
            }
        }
        return builder.Finish(&out).ok();
    };
    std::shared_ptr<arrow::Array> tensor_storage, vec_arr;
    if (!make_fsl(tensor_storage) || !make_fsl(vec_arr)) return false;

    auto tensor_type = arrow::extension::fixed_shape_tensor(arrow::int32(), {2, 2});
    if (!tensor_type) return false;
    auto tensor_arr = arrow::ExtensionType::WrapArray(tensor_type, tensor_storage);

    arrow::Int32Builder id_builder;
    for (int row = 0; row < kRows; ++row) st = id_builder.Append(row);
    std::shared_ptr<arrow::Array> id_arr;
    st = id_builder.Finish(&id_arr);

    arrow::StringBuilder dict_values;
    for (int row = 0; row < kRows; ++row) st = dict_values.Append(row == 1 ? "b" : "a");
    std::shared_ptr<arrow::Array> dict_plain;
    st = dict_values.Finish(&dict_plain);
    auto maybe_dict = arrow::compute::DictionaryEncode(dict_plain);
    if (!maybe_dict.ok()) return false;
    auto dict_arr = maybe_dict->make_array();

    auto schema = arrow::schema({
        arrow::field("plain", arrow::int32()),
        arrow::field("tensor_col", tensor_type),
        arrow::field("dict_col", dict_arr->type()),
        arrow::field("vec_col", vec_arr->type()),
    });
    auto table = arrow::Table::Make(schema, {id_arr, tensor_arr, dict_arr, vec_arr});

    auto maybe_outfile = arrow::io::FileOutputStream::Open("test/fixtures/encoded_types.parquet");
    if (!maybe_outfile.ok()) return false;
    auto outfile = *maybe_outfile;
    auto arrow_props = parquet::ArrowWriterProperties::Builder().store_schema()->build();
    auto status = parquet::arrow::WriteTable(*table, arrow::default_memory_pool(), outfile, kRows,
        parquet::default_writer_properties(), arrow_props);
    return status.ok();
}

// test/fixtures/dictionary_types.parquet: DICTIONARY-typed columns -- what pandas writes for a
// `Categorical` -- each with a plain twin holding exactly the same values.
//
// A dictionary column is not a page-level encoding (every string column this library writes is
// dictionary-ENCODED at the page level); it is an Arrow TYPE, restored on read from the stored
// Arrow schema. So store_schema() is REQUIRED here: without it every column below round-trips as
// its bare value type and the fixture tests nothing at all.
//
// Arrow restores a stored dictionary type only over STRING/BINARY values (IsDictionaryReadSupported
// in parquet/arrow/schema.cc), and rebuilds it with the ORIGINAL index type -- pandas picks the
// narrowest that fits, so int8 rather than the int32 arrow::compute::DictionaryEncode produces.
// Both facts are pinned by the columns below rather than trusted, since a future Arrow that
// widened either rule would change what this library reads without any change here.
//
// Two row groups (chunk_size 4 over 8 rows). `cat` is built as a two-CHUNK column whose chunks
// carry DIFFERENT dictionaries -- "alpha"/"beta" in the first, "gamma"/"beta"/"delta" in the
// second, so the same index means different values in the two row groups. That is the shape the
// per-chunk decode exists for, and a decode done after the chunks were combined rather than
// before would have to unify those two dictionaries first. Building `cat` from one array with a
// single dictionary would NOT produce it: Arrow writes the Arrow dictionary it is given into
// every row group's dictionary page, so both pages would be identical (measured).
//
//   id           int32                                    1..8, row identity for filter tests
//   cat          dictionary<values=string, indices=int8>  -> "string": the pandas `category` case,
//                                                           nulls at rows 3 and 8
//   plain        string                                   THE ORACLE: identical values and nulls
//   cat_ordered  the same, ordered=1                      -> "string": pandas' `ordered` flag is
//                                                           dropped on read, not refused
//   cat_large    dictionary<large_string, int8>           -> "string" (Arrow hands the value type
//                                                           back as `string`, not `large_string`)
//   cat_int      dictionary<int64, int8>                  -> "int64": Arrow does NOT restore this
//                                                           one, so the column arrives dense and
//                                                           nothing here decodes it
//   cat_bytes    dictionary<binary, int8>                 -> "unknown": restored, but a dictionary
//                                                           over a value type this library cannot
//                                                           read stays unreadable, exactly as a
//                                                           plain binary column does
//   cat_allnull  dictionary<string, int8>, empty dict     -> "string", every row null
//
// Used by test/test_reading.f90's dictionary tests, test/test_filter.f90, test/test_filter_screen.f90
// and test/test_table.f90.
static bool generate_dictionary_types_fixture()
{
    arrow::Status st;
    const int kRows = 8;
    // Rows 3 and 8 are null; rows 1-4 use "alpha"/"beta" only and rows 5-8 introduce "gamma" and
    // "delta", so the two row groups cannot share a dictionary page.
    const int8_t kIndices[kRows] = {0, 1, 0, 0, 2, 1, 3, 0};
    const bool kValid[kRows] = {true, true, false, true, true, true, true, false};
    const char *kValues[4] = {"alpha", "beta", "gamma", "delta"};

    arrow::Int32Builder id_builder;
    for (int row = 0; row < kRows; ++row) st = id_builder.Append(row + 1);
    std::shared_ptr<arrow::Array> id_arr;
    st = id_builder.Finish(&id_arr);

    arrow::Int8Builder index_builder;
    for (int row = 0; row < kRows; ++row)
    {
        if (kValid[row]) { st = index_builder.Append(kIndices[row]); } else { st = index_builder.AppendNull(); }
    }
    std::shared_ptr<arrow::Array> indices;
    st = index_builder.Finish(&indices);

    // `cat`'s own per-row-group indices, into the two SEPARATE dictionaries built below.
    auto build_indices = [&](const std::vector<int> &values, std::shared_ptr<arrow::Array> &out) {
        arrow::Int8Builder builder;
        for (int value : values)
        {
            if (value < 0) { st = builder.AppendNull(); } else { st = builder.Append(static_cast<int8_t>(value)); }
        }
        return builder.Finish(&out).ok();
    };
    std::shared_ptr<arrow::Array> first_indices, second_indices;
    if (!build_indices({0, 1, -1, 0}, first_indices)) return false;   // alpha, beta, null, alpha
    if (!build_indices({0, 1, 2, -1}, second_indices)) return false;  // gamma, beta, delta, null

    auto build_values = [&](const std::vector<const char *> &values, std::shared_ptr<arrow::Array> &out) {
        arrow::StringBuilder builder;
        for (const char *value : values) st = builder.Append(value);
        return builder.Finish(&out).ok();
    };
    std::shared_ptr<arrow::Array> first_values, second_values;
    if (!build_values({"alpha", "beta"}, first_values)) return false;
    if (!build_values({"gamma", "beta", "delta"}, second_values)) return false;

    arrow::Int8Builder null_index_builder;
    for (int row = 0; row < kRows; ++row) st = null_index_builder.AppendNull();
    std::shared_ptr<arrow::Array> null_indices;
    st = null_index_builder.Finish(&null_indices);

    // The plain twin: the same eight values, as an ordinary string column.
    arrow::StringBuilder plain_builder;
    for (int row = 0; row < kRows; ++row)
    {
        if (kValid[row]) { st = plain_builder.Append(kValues[kIndices[row]]); } else { st = plain_builder.AppendNull(); }
    }
    std::shared_ptr<arrow::Array> plain_arr;
    st = plain_builder.Finish(&plain_arr);

    arrow::StringBuilder value_builder;
    for (const char *value : kValues) st = value_builder.Append(value);
    std::shared_ptr<arrow::Array> string_values;
    st = value_builder.Finish(&string_values);

    arrow::LargeStringBuilder large_builder;
    for (const char *value : kValues) st = large_builder.Append(value);
    std::shared_ptr<arrow::Array> large_values;
    st = large_builder.Finish(&large_values);

    arrow::Int64Builder int_builder;
    for (int k = 0; k < 4; ++k) st = int_builder.Append(10 * (k + 1));
    std::shared_ptr<arrow::Array> int_values;
    st = int_builder.Finish(&int_values);

    arrow::BinaryBuilder binary_builder;
    for (const char *value : kValues) st = binary_builder.Append(value, static_cast<int32_t>(std::strlen(value)));
    std::shared_ptr<arrow::Array> binary_values;
    st = binary_builder.Finish(&binary_values);

    // An all-null column's dictionary is empty -- the shape a decode that assumes at least one
    // dictionary entry trips over.
    arrow::StringBuilder empty_builder;
    std::shared_ptr<arrow::Array> empty_values;
    st = empty_builder.Finish(&empty_values);

    auto make_dict = [&](const std::shared_ptr<arrow::DataType> &value_type, bool ordered,
        const std::shared_ptr<arrow::Array> &idx, const std::shared_ptr<arrow::Array> &values,
        std::shared_ptr<arrow::Array> &out) {
        auto type = arrow::dictionary(arrow::int8(), value_type, ordered);
        auto made = arrow::DictionaryArray::FromArrays(type, idx, values);
        if (!made.ok()) return false;
        out = *made;
        return true;
    };

    std::shared_ptr<arrow::Array> first_cat, second_cat, ordered_arr, large_arr, int_arr, bytes_arr, allnull_arr;
    if (!make_dict(arrow::utf8(), false, first_indices, first_values, first_cat)) return false;
    if (!make_dict(arrow::utf8(), false, second_indices, second_values, second_cat)) return false;
    if (!make_dict(arrow::utf8(), true, indices, string_values, ordered_arr)) return false;
    if (!make_dict(arrow::large_utf8(), false, indices, large_values, large_arr)) return false;
    if (!make_dict(arrow::int64(), false, indices, int_values, int_arr)) return false;
    if (!make_dict(arrow::binary(), false, indices, binary_values, bytes_arr)) return false;
    if (!make_dict(arrow::utf8(), false, null_indices, empty_values, allnull_arr)) return false;

    // `cat` is the only column that has to be chunked; every other one is a single array, which
    // WriteTable slices at the row-group boundary by itself.
    auto cat_chunked = arrow::ChunkedArray::Make({first_cat, second_cat}, first_cat->type());
    if (!cat_chunked.ok()) return false;
    auto one_chunk = [](const std::shared_ptr<arrow::Array> &array) {
        return std::make_shared<arrow::ChunkedArray>(array);
    };

    auto schema = arrow::schema({
        arrow::field("id", arrow::int32()),
        arrow::field("cat", first_cat->type()),
        arrow::field("plain", arrow::utf8()),
        arrow::field("cat_ordered", ordered_arr->type()),
        arrow::field("cat_large", large_arr->type()),
        arrow::field("cat_int", int_arr->type()),
        arrow::field("cat_bytes", bytes_arr->type()),
        arrow::field("cat_allnull", allnull_arr->type()),
    });
    auto table = arrow::Table::Make(schema, {one_chunk(id_arr), *cat_chunked, one_chunk(plain_arr),
        one_chunk(ordered_arr), one_chunk(large_arr), one_chunk(int_arr), one_chunk(bytes_arr),
        one_chunk(allnull_arr)}, kRows);

    auto maybe_outfile = arrow::io::FileOutputStream::Open("test/fixtures/dictionary_types.parquet");
    if (!maybe_outfile.ok()) return false;
    auto outfile = *maybe_outfile;
    auto arrow_props = parquet::ArrowWriterProperties::Builder().store_schema()->build();
    auto status = parquet::arrow::WriteTable(*table, arrow::default_memory_pool(), outfile, kRows / 2,
        parquet::default_writer_properties(), arrow_props);
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
        {"test/fixtures/list_widths.parquet", generate_list_widths_fixture},
        {"test/fixtures/list_payloads.parquet", generate_list_payloads_fixture},
        {"test/fixtures/no_stats.parquet", generate_no_stats_fixture},
        {"test/fixtures/extended_types.parquet", generate_extended_types_fixture},
        {"test/fixtures/nested_struct.parquet", generate_nested_struct_fixture},
        {"test/fixtures/struct_payloads.parquet", generate_struct_payloads_fixture},
        {"test/fixtures/map_payloads.parquet", generate_map_payloads_fixture},
        {"test/fixtures/map_list_types.parquet", generate_map_list_types_fixture},
        {"test/fixtures/element_nulls.parquet", generate_element_nulls_fixture},
        {"test/fixtures/screen_declined_nulls.parquet", generate_screen_declined_nulls_fixture},
        {"test/fixtures/encoded_types.parquet", generate_encoded_types_fixture},
        {"test/fixtures/dictionary_types.parquet", generate_dictionary_types_fixture},
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
