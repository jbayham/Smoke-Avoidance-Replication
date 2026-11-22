# Validation status — November 22, 2026

**Implementation is prepared, but the clean full-data reproduction is incomplete. A November 22 process and output check found no active rebuild process and no completed stages in the full-run record. Its saved `running` status is stale. This is not a claim that all 50 stages have completed from raw inputs.**

A supervised restart was launched November 22. All ten Python tests and eight R synthetic suites passed again, including a check that smaller daily-array batches preserve the aggregation. The restart is waiting for the institution's vendor-data storage to become readable: the mount reports a host outage. All 132 non-vendor inputs are readable and match their inventory sizes. The service checks input availability every five minutes and will then run all stages. It uses four-CPU affinity, `nice +10`, sequential stages, a 16 GiB memory-pressure threshold, a 64 GiB memory ceiling, and 25,000-row daily-array batches. This is a resource budget, not a demonstrated minimum memory requirement.

| Check | Evidence/status |
|---|---|
| Python runner | Ten tests pass, including dependency order, missing-input reporting, path handling, exclusive execution, failure status, resume and downstream invalidation |
| R logic | Eight synthetic test suites pass: monthly commercial aggregation/matching, joins, temperature conversion, calendar alignment, fixed-effect keys, LP-DID boundaries/sample rules, date coverage and heterogeneity plotting |
| Environment | R 4.5.0 and all 145 pinned package versions pass `doctor`; Arrow Dataset/Parquet round trip passes |
| Project library restore | `scripts/setup.R` successfully recognizes the exact project library as synchronized and passes `doctor`; this used copies of the reference server's installed packages, not a fresh compilation/download of every dependency |
| Raw input preflight | All 237 required file/pattern entries are present; representative monthly Parquet schemas, place dimension, CSV headers and NetCDF fields pass; every delivery shard is checked by the readers during construction |
| Numerical validator | Accepted archived results pass all 260 coefficient/SE comparisons, 21 sample sizes, spending coefficients and aggregate, 42 industry classifications, and 42 neighbor shares |
| Negative validation test | Deliberately adding 0.1 to an archived coefficient causes validation to fail, as required |
| Fresh ZIP extraction | All eight R suites, ten Python tests, the schematic stage, and the real NAICS classification build pass in a directory containing spaces; all 42 regenerated classifications match accepted values, and the second classification run is correctly reused |
| Clean full-data Linux run | Started November 21 in a fresh verification data/output directory, with external raw snapshots only. Last log entry is prerequisite checking for `consumer_totals`; no stage completions or generated intermediate datasets were recorded. No active rebuild process was found on November 22. The interruption cause is not established. No processed panels or fitted models were seeded into this run |
| Additional input-construction check | Separate fresh county-characteristic build reached county 701 of 3,142 in the park geometry log. No active process or completed park output was found on November 22; its saved `running` status is also stale |
| macOS/Windows | Native entry point and three-platform CI workflow supplied. Full data execution and CI installation have **not** been run on these platforms |
| Release integrity | The ZIP is checked by listing entries, rejecting symlinks and unsafe paths, verifying checksums, extracting to a new directory containing spaces, and running package integrity and Python tests there. Archive CRC, extracted-file SHA-256, Python tests and missing-input checks passed. Run `python scripts/verify_package.py` to verify the distributed source files |

The numerical validator was tested against archived accepted results to verify its behavior. That exercise does **not** establish that the newly refactored pipeline has independently regenerated those results. The fresh full run must reach its final `validate` step before that claim is made.

The reference server has approximately 377 GiB RAM; this is the host configuration, not a demonstrated minimum requirement. Commercial source inputs alone occupy roughly 1.2 TB. Complete runtime and peak memory for the standalone pipeline are not yet measured. Four R threads are configured by default, and stages run sequentially.

After a full run, review `output/run_status.json`, `output/validation/`, and per-stage session information under `output/checks/`. Verify that the requested stage list covers the complete pipeline and that numerical validation passed. A targeted run's `complete` state applies only to its requested scope.

The earlier research analysis runs completed separately. Their accepted results underpin the reference benchmarks, but do not establish that this standalone package has completed a fresh rebuild. No new substantive discrepancy with those estimates has been established during packaging. End-to-end execution and platform verification remain unfinished.
