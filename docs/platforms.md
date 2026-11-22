# Platform setup

The entry point uses Python's standard library and subprocess argument arrays, not Bash, `fcntl`, Unix `flock`, symlinks or a server mount. Use native Python and R on Linux, macOS or Windows. Restore the project separately on each operating system; never copy compiled R libraries between platforms.

The reference environment is Ubuntu 22.04, R 4.5.0 and Python 3.10.12. Exact package versions are in `renv.lock` and `package_versions.csv`. Notable pins are `fixest` 0.11.2, `arrow` 14.0.0.2, `sf` 1.0-15 and `ggplot2` 4.0.3. The setup script installs renv 1.3.1 into the package's own bootstrap library and restores into `renv/library/`; global R libraries are not modified. `doctor` refuses mismatched versions.

## Linux

Source builds may require a C/C++/Fortran toolchain and development headers for GDAL, GEOS, PROJ, UDUNITS2, NetCDF, curl, SSL, XML, fontconfig, FreeType, HarfBuzz, FriBidi, JPEG, PNG and TIFF. Install these through your distribution's package manager. Package names and versions depend on the distribution; consult the restore error for the missing library. The server's compiled-library versions are recorded by `doctor` and stage session logs.

Arrow must include its Dataset and Parquet capabilities. Use the pinned R and C++ Arrow release; installing a current Arrow library under the old R package is not an equivalent environment. Source installation can be lengthy and memory-intensive. Do not silently relax `renv.lock` to get past an installation failure.

## macOS

Install R 4.5.0 for your architecture, Python, and Apple's command-line development tools if a source build is needed. Match arm64 versus x86_64 across R and compiled libraries. Historical CRAN binaries may no longer be available; pinned source builds may need the geospatial and NetCDF libraries above and an R-compatible Fortran toolchain. Use a writable location outside synchronizing cloud folders for large active datasets.

## Windows

Use R 4.5.0 and Python 3.10+, with Rscript on PATH or specified in `config/local.json`. Use the matching Rtools45 toolchain if restoring archived packages from source. Prefer compatible historical binaries when available; otherwise `sf`, `units`, `ncdf4` and Arrow may require their documented source-build dependencies. Paths containing spaces are supported. In JSON, use forward slashes or escape backslashes. Long source/dataset paths may require Windows long-path support.

## What portability testing means

The GitHub Actions workflow restores the lockfile and runs synthetic checks on all three operating systems, without datasets. A three-platform pass requires successful workflow runs on all three operating systems. A synthetic CI pass does not substitute for the full numerical replication. See `validation.md` for the current evidence and pending work.

Primary installation references: [renv](https://rstudio.github.io/renv/articles/renv.html), [sf](https://r-spatial.github.io/sf/), [Arrow R installation](https://arrow.apache.org/docs/r/articles/install.html). Current documentation may describe newer versions; apply it to the pinned releases rather than upgrading the analysis implicitly.
