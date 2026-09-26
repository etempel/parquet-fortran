# parquet-fortran — open risks

Silent failures a user of the library can suffer that **no test or check catches today**: a wrong
answer, lost or corrupted data, or a hang, with nothing failing. A risk a test covers is not here;
neither is a cost, a diagnostics or build property, a compiler trap, a test-design lesson, a
documented caller contract or a contributor-only trap. Read the entries for an area before editing
it. `.claude/rules/workflow.md`, "The `feature_risks.md` open-risks register", holds the admission
test and the editing rules; `check_risk_register_shape` enforces the shape.

Every `Risk-N` cited anywhere in the repository names an entry below. Closing an entry removes its
citations in the same change, after what it required moves into a comment at the site or a rule
file, so a number never points at something that is gone; `check_risk_register_shape` enforces that
over the instruction files and the four source trees. The numbering was compacted once, so a
`Risk-N` in the git history or in an archived document does not name the entry of that number here.

Next number: Risk-5

## Open risks

### Risk-1 — A parallel table read loses nulls if the validity bitmap is not allocated before the region

`materialize_column_parallel` (`src/parquet_tables_read.f90`) pastes one row group per thread into
one column. The pastes are disjoint, but `%paste` allocates the validity bitmap on first null
(`ensure_bitmap`), so two threads pasting null-carrying row groups can both allocate it: one
allocation leaks and its nulls are silently lost. The guard is the footer query before the region
(`parquet_column_has_nulls` then `%ensure_validity()`); a null-free column still allocates nothing.
**Why no test:** removing the guard passed 5 runs of 5 on a 200 000-row, 8-thread fixture; the
window is a few instructions. **Forbids:** any parallel path writing into one `parquet_column` from
several threads ensures validity before the region, never inside it. A ThreadSanitizer report of
`paste` storing `has_nulls = .true.` from several threads is a benign same-value store, not this.

### Risk-2 — A weighted sampler at the caller's own coordinates couples with the caller's draws

`pf_weighted_permutation` and `pf_weighted_draw` derive their seeds through `pf_random_key` with
fixed family labels (`wperm_family_label`, `wd_family_label`), so neither reads the raw
`(seed, stream)` axis. That keeps them independent of each other and of a caller's own
`pf_random_at(seed, stream, ...)` at the same coordinates. Coupling concentrates in the rarest cell
(the lowest-weight item) and leaves every marginal clean. **Why uncovered:**
`test_families_independent` (`test/test_random_weighted.f90`) fails only when BOTH labels are
reverted; one family reverting to the raw axis couples with the caller and no test sees it.
**Closes with:** a joint test of each family against `pf_random_at` at matched coordinates in the
lowest-weight cell, with a deliberately coupled control arm. A new construction over the same
generator takes its own label.

### Risk-3 — A size-then-fill pair whose sizing pass discards work writes past its buffer

`pf_join_path_many` (`src/parquet_utils.f90`) sizes a joined path in one walk and fills it in a
second; an absolute component discards everything before it. If the filling walk writes a prefix
the sizing walk discarded, it stores past a buffer sized for the survivor while returning the
right answer. Both walks start at the last absolute component so neither resets. **Why uncovered:**
`test_join_many_degenerate` (`test/test_utils.f90`) supplies the input, but only a bounds-checked
build reports the overrun (`fpm test run_tester --profile debug -- utils`, or `--profile nagdeb`);
plain `fpm test` and CI compile no `-fcheck`. **Closes with:** a CI job running the suite under
`--profile debug`. **Forbids:** in any size-then-fill pair, pass 1 may shrink its accumulator and
pass 2 may not.

### Risk-4 — A far-tail truncated-normal draw hangs when its case threshold cancels under FMA

`tn_threshold` (`src/parquet_random.f90`) must not form `a*a - a*s` with `s = sqrt(a*a + 4)`: for a
standardised bound near `1e10` an FMA contraction turns the exact 0 into hundreds, the threshold
becomes huge, every far-tail draw routes to a proposal whose acceptance underflows, and
`%normal_truncated` never returns. The rationalised forms (`exp(0.5 - a/(a+s))`,
`(m - z)*(m + z)`, `hypot(a, 2)`) prevent it; keep them. **Why uncovered:**
`test_normal_trunc_far_tail_terminates` (`test/test_random_dist.f90`) fails only in an optimised
build; plain `fpm test` and CI run at `-O0`, where no FMA exists. **Closes with:** a CI job running
`random_dist` under `--profile release`. **Forbids:** a fixture whose standardised bound reaches
about `9e307`, where `a + s` overflows into the same hang.
