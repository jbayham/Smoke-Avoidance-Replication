# Validation status — November 28, 2026

**The clean full-data reproduction is running but incomplete. It is building monthly consumer totals, the first of 50 stages. This is not a claim that the standalone package has completed a full rebuild.**

A supervised restart was launched November 22. Storage access subsequently returned and all 2,678 inventoried raw files (1,041,896,545,056 bytes) were readable with matching sizes. Input preflight and environment checks passed. On November 28, the first stage was found heavily throttled by its 16 GiB memory-pressure threshold; raising that threshold to 48 GiB allowed processing to advance from January to February 2019. The four-CPU affinity, `nice +10`, sequential stages, 64 GiB hard memory ceiling, and 25,000-row daily-array batches remain in force. These are resource budgets, not demonstrated minimum requirements.

The November 23 collaborator corrections to lower AQI breakpoints and Canadian-smoke costs are incorporated. All nine R test suites pass, including new boundary and cost-aggregation fixtures. Applying the new cost expression to **previously accepted model outputs** gives signed spending and welfare changes of -85,392,835.04398 and -1,369,369.05315; this checks the expression, not completion of the fresh rebuild. See [analysis updates](analysis_updates.md).

| Check | Evidence/status |
|---|---|
| Python runner | Ten tests pass, including dependency order, missing-input reporting, path handling, exclusive execution, failure status, resume and downstream invalidation |
| R logic | Nine synthetic test suites pass: monthly commercial aggregation/matching, joins, temperature conversion, calendar alignment, fixed-effect keys, LP-DID boundaries/sample rules, date coverage, heterogeneity plotting, and AQI/Canadian-cost expressions |
| Environment | R 4.5.0 and all 145 pinned package versions pass `doctor`; Arrow Dataset/Parquet round trip passes |
| Project library restore | `scripts/setup.R` successfully recognizes the exact project library as synchronized and passes `doctor`; this used copies of the reference server's installed packages, not a fresh compilation/download of every dependency |
| Raw input preflight | All 237 required file/pattern entries are present; representative monthly Parquet schemas, place dimension, CSV headers and NetCDF fields pass; every delivery shard is checked by the readers during construction |
| Numerical validator | Accepted archived results pass all 260 coefficient/SE comparisons, 21 sample sizes, spending coefficients and aggregate, 42 industry classifications, and 42 neighbor shares |
| Negative validation test | Deliberately adding 0.1 to an archived coefficient causes validation to fail, as required |
| Fresh ZIP extraction | All eight R suites, ten Python tests, the schematic stage, and the real NAICS classification build pass in a directory containing spaces; all 42 regenerated classifications match accepted values, and the second classification run is correctly reused |
| Clean full-data Linux run | The initial November 21 run was interrupted before any stage completed. The supervised November 22 restart is active in `consumer_totals`, processing February 2019 after the November 28 memory-throttling correction. No complete stage has yet been recorded. No processed panels or fitted models were seeded into this run |
| Additional input-construction check | Separate fresh county-characteristic build reached county 701 of 3,142 in the park geometry log. No active process or completed park output was found on November 22; its saved `running` status is also stale |
| Fresh CI installation | The November 22 GitHub Actions runs failed during environment restoration on Linux, macOS and Windows, before synthetic testing. All three reported that `knitr` could not load because `attr` is not exported by the pinned `xfun`; macOS and Windows also reported native package compilation/configuration failures. The working server library does not establish clean-install portability. These failures remain unresolved; see [CI run](https://github.com/jbayham/Smoke-Avoidance-Replication/actions/runs/44976854323) |
| macOS/Windows | Native entry point and three-platform CI workflow supplied. Full data execution is unverified, and fresh CI installation failed as described above |
| Release integrity | The ZIP is checked by listing entries, rejecting symlinks and unsafe paths, verifying checksums, extracting to a new directory containing spaces, and running package integrity and Python tests there. Archive CRC, extracted-file SHA-256, Python tests and missing-input checks passed. Run `python scripts/verify_package.py` to verify the distributed source files |

The numerical validator was tested against archived accepted results to verify its behavior. That exercise does **not** establish that the newly refactored pipeline has independently regenerated those results. The fresh full run must reach its final `validate` step before that claim is made.

The reference server has approximately 377 GiB RAM; this is the host configuration, not a demonstrated minimum requirement. Commercial source inputs alone occupy roughly 1.2 TB. Complete runtime and peak memory for the standalone pipeline are not yet measured. Four R threads are configured by default, and stages run sequentially.

After a full run, review `output/run_status.json`, `output/validation/`, and per-stage session information under `output/checks/`. Verify that the requested stage list covers the complete pipeline and that numerical validation passed. A targeted run's `complete` state applies only to its requested scope.

The earlier research analysis runs completed separately. Their accepted results underpin the reference benchmarks, but do not establish that this standalone package has completed a fresh rebuild. No new substantive discrepancy with those estimates has been established during packaging. End-to-end execution and platform verification remain unfinished.
