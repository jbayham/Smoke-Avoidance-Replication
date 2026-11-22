# Required inputs

No datasets are included in the code repository. Download the non-vendor inputs using the [download and extraction instructions](../README.md#download-and-place-the-data); obtain licensed vendor inputs separately. Place the author-used snapshots under `data/raw/` using the paths below. Do not place raw files in `output/`. All supplied inputs remain read-only during the pipeline. `data/intermediate/` and `data/processed/` are generated and can be rebuilt.

The complete machine-readable list is [input_manifest.csv](../docs/input_manifest.csv): 237 required files or glob patterns, with coverage, minimum file counts, source descriptions and checked columns. A pattern means **all** delivery shards, not just one example file. `python run.py check-inputs` reports missing files together and then checks CSV/NetCDF schemas and one representative Parquet shard per month. The commercial builders check every shard as it is read. The minimum-count check cannot establish whether a licensed delivery contains every original shard; compare delivery manifests and the recorded input hashes.

## Licensed commercial inputs

| Directory | Files | Coverage |
|---|---|---|
| `raw/vendor/places/` | Parquet dataset, possibly Hive-partitioned by `naics_code` | Author-used place dimension |
| `raw/vendor/spend/` | `YYYY-MM--*.parquet` | Every month January 2019–December 2023 |
| `raw/vendor/monthly_patterns/` | `YYYY-MM--*.parquet` | Every month January 2019–December 2023 |

Obtain the licensed Dewey/SafeGraph snapshots through the appropriate data agreement or from the authors where permitted. Retain the complete redelivery vintage and every shard. The package starts from the supplied place dimension; it does not reconstruct that upstream dimension from a proprietary POI master database. Required fields are listed in the manifest. Column names may be uppercase or lowercase. Daily arrays may be JSON strings or nested arrays, and timestamps must preserve the actual start date. Preserve partition directories for the places dataset so `naics_code` remains available.

## Smoke, weather and geography

- `raw/smoke/grids/`: 36 annual `krigedPM25_<YEAR>_v2_statfix_<meanbk|medianbk>.nc` files, 2006–2023. Obtain the author-used Pierce/Childs gridded smoke release. See exact names in the manifest. Required NetCDF fields include `doy`, `PM25`, `Background_PM25`, and `HMS_Smoke`.
- `raw/smoke/grid/grid-fullUS.shp`, `.shx`, `.dbf`: matching smoke grid polygons. The source has no `.prj`; the original builder explicitly assigns EPSG:4326. Do not substitute a different grid.
- `raw/aqi/daily_aqi_by_county_<YEAR>.csv`: 2006–2023 EPA AirData county AQI exports, unmodified.
- `raw/weather/<VAR>/<VAR>_<YEAR>.nc`: gridMET variables `pr`, `tmmn`, `tmmx`, `rmax`, 2019–2023. Preserve native units (temperature in kelvin); the daily-panel builder converts temperatures.
- `raw/geography/`: Census/TIGER files `tl_2019_us_county`, `tl_2020_us_county`, `tl_2020_us_state`, and `cb_2019_us_county_500k`, each with `.shp`, `.shx`, `.dbf`, `.prj`. Smoke/weather aggregation uses **2020** boundaries; county characteristics and spatial weights use **2019**. These are deliberately fixed vintages.
- `raw/parks/USA_Parks.shp`, `.shx`, `.dbf`, `.prj`: author-used park polygons. The package computes county park areas, then combines them with supplied tourism measures. No-park counties receive zero park share.

These upstream products are external inputs. The package reproduces their county aggregation and analysis, not the original atmospheric model, gridMET model, Census geography, or park database construction. Use the author snapshots for numerical replication rather than silently substituting the current public release.

## County characteristics

Supply these exact files under `raw/county/`:

- `tourism_earnings_and_employment_proportions_by_county.csv`
- `county_census_population_and_income.csv`
- `CBP2018.CB1800CBP-Data.csv`
- `county_carr_public_rescale_091324.csv`
- `geocorr2022_CT_counties_to_county_equivalents.csv`

Tourism and population/income CSVs are **upstream author-constructed inputs**. Their original upstream recipes have not been independently validated here; request the snapshots from the authors. Tourism needs five-digit `county_geoid`, `both_emp_prop`, `both_earn_prop`; population/income needs `county_geoid`, `median_household_income`, `population_estimate` (and the supplied imputation field). Preserve FIPS as text with leading zeros. The package generates natural-amenity dependence, park shares, rurality, CBP sector composition and the combined county panel from these inputs.

CBP is the 2018 Census download; retain its metadata/header rows. CARR is the September 13, 2024 county rescale release. Geocorr maps Connecticut counties to county equivalents and includes population allocation weights; preserve `CTcounty`, `county`, `afact`.

## Spending, mortality, industry and fire inputs

Supply these files under `raw/bea/`, preserving original BEA metadata and header rows:

- `bea_overall_gdp_and_compensation_table_2018.csv`
- `bea_outdoor_rec_value_added_by_activity_2018.csv`
- `bea_outdoor_rec_value_added_by_naics_2018.csv`
- `bea_outdoor_rec_compensation_by_naics_2018.csv`
- `annual_personal_expenditures_chained_dollars_2018_2024.csv`

Also supply:

- `raw/cpi/annual_avg_cpi_deflation_relative_to_2017.csv`: author-used annual CPI deflator snapshot (`year`, `avg_cpi`, `deflator`), relative to 2017.
- `raw/naics/6-digit_2017_Codes.xlsx`: Census 2017 six-digit industry descriptions; classification rules are in the code.
- `raw/mortality/Underlying Cause of Death <YEAR>.csv`: five CDC WONDER exports, 2019–2023. Keep original export layout, footers and suppressed values.
- `raw/fire/fire_panel_us_2019_2026.csv`: supplied county-day VIIRS/perimeter panel, with `county_fips`, `date`, `viirs_total`, `viirs_in_perimeter`. This is an upstream derived input; constructing fire detections and perimeters is outside this package. The analysis uses 2019–2023.
- `raw/routing/county_neighbors_df_500_km_radius_with_drive_time.csv`: archived county-pair driving distances/times. Required fields include `source_geoid`, `neighbor_geoid`, `travel_times_filled` and the original distance/time fields. Obtain this existing snapshot from the authors. **The package makes no Google Maps or OSRM requests.**

No Google Drive credentials, API keys, remote mounts or absolute server paths are required. The [directory map](../README.md#files-and-directories) explains where every downstream output is written.

For CSV and Excel snapshots, [input_snapshot_hashes.csv](../docs/input_snapshot_hashes.csv) records the exact input bytes used for this verification. NetCDF, commercial Parquet and shapefiles are fingerprinted by the runner; their full hashes are recorded in local run state.
