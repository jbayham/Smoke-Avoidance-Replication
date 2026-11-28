# Analysis updates

## November 28, 2026: incorporate November 23 collaborator corrections

Two changes are included without changing package-relative paths or the accepted model specifications:

- **AQI conversion:** the first interpolation interval ends below 9.1, with endpoint 9.0; the second starts at 9.1 and ends at 35.4. Previously, the interval condition and the second interval's starting point still used 12.1, inconsistent with the first interval's 9.0 endpoint. The remaining intervals and behavior at concentrations of 500.5 or higher are unchanged. This propagates the supplied correction; it does not replace the entire conversion with a different external standard.
- **Canadian-smoke cost example:** the spending/welfare script now multiplies the three high-smoke-bin estimates by the supplied county-day counts **1,218 / 645 / 6**. It prints `Canada Smoke Spend` and `Canada Smoke Welfare` in `output/logs/welfare_spending.log`. The signs follow the collaborator's calculation: spending changes are signed changes, and positive per-day welfare losses are printed as negative aggregate changes. These counts are supplied constants, not newly inferred source-fire attribution.

The main exposure indicator uses the median-smoke/background-standard-deviation filter, independently of AQI conversion. The AQI correction changes AQI-derived quantities and can affect the AQI-filter robustness results. Regenerate those results before judging agreement; existing numerical references have not been silently replaced.

`tests/test_aqi_and_canadian_costs.R` executes the actual expressions on synthetic inputs. It checks both lower boundaries, decimal truncation, upper intervals, missing values, bin-name matching independent of row order, the supplied counts, and printed signs/totals. All nine R test suites pass. The code archive's integrity and Python checks are also rerun when packaging.

With previously accepted model outputs, the added expression yields a spending change of **-85,392,835.04398** and welfare change of **-1,369,369.05315**. These are a calculation check using existing estimates, not a claim that the active raw-input rebuild is complete. See [validation status](validation.md).
