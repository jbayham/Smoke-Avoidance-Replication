# Data and analysis provenance

This repository implements the Smoke Avoidance manuscript analysis, including the corrected spending-date alignment. `MANIFEST.sha256` identifies the distributed source files and their exact contents.

The CSVs in `reference/` contain accepted numerical benchmarks for the manuscript. They are used only to compare newly generated results, never to construct analysis panels or fit models. The `evidence` column in `reference/regression_comparisons.csv` identifies coefficient exports relative to the configured output directory, such as `checks/count_models/est_original_coefficients.csv`.

## Analysis conventions

- Runtime paths resolve within the repository or explicit local configuration.
- External inputs are supplied under `data/raw` and remain unmodified. Routing uses the archived county-pair CSV without API calls.
- Smoke and weather aggregation use 2020 county geography. County characteristics and spatial weights use 2019 geography. Counties without park intersections receive zero park share.
- Appendix A.2 is reconstructed from the smoke and daily analysis panels.
- The runner orders dependencies, bounds threads, verifies file hashes, exports coefficients and records logs and session information. Stages are re-estimated when their inputs or implementation change.
- Both the treated-only and strict-both-groups LP-DID robustness specifications are explicitly labeled in the generated outputs.

## External inputs

The input guide in `data/README.md` identifies upstream-derived snapshots: the place dimension, tourism and Census measures, CPI deflator, VIIRS/perimeter panel and archived routing product. These are supplied inputs. The pipeline constructs amenity measures from tourism and park inputs and generates the downstream study panels.

`docs/input_snapshot_hashes.csv` records the supplied CSV and Excel input snapshots used for verification. Input fingerprints for each execution are stored in `output/run_status.json`.

The author spatial illustration in `assets/` is an explanatory schematic, not an estimated result. Input datasets, credentials, local configuration, generated model objects and installed R libraries are excluded from distribution.
