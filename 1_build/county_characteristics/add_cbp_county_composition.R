library(tidyverse)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))


# --- 1. Load indoor/outdoor classification ---
indoor_outdoor_df <- read_csv(NAICS_CODES_INDOOR_OUTDOOR_CLASSIFICATIONS_PATH)

# --- 2. Load and clean CBP 2018 data ---
cbp_raw <- read_csv(
  CBP_EST_AND_EMP_DATA_2018_PATH,
  col_types = cols(.default = "c")  # read all as character to handle suppressed values
) %>%
  slice(-1)  # drop the description row

cbp <- cbp_raw %>%
  filter(EMPSZES == "001", LFO == "001") %>%  # totals only
  mutate(
    county_geoid = str_sub(GEO_ID, start = -5),
    naics_code = as.integer(NAICS2017),
    estab = suppressWarnings(as.numeric(ESTAB)),
    payann = suppressWarnings(as.numeric(PAYANN))
  ) %>%
  filter(!is.na(naics_code)) %>%
  # Keep only deferrable NAICS codes
  inner_join(indoor_outdoor_df, by = "naics_code") %>%
  select(county_geoid, naics_code, naics_description, likely_outdoor_label, estab, payann)
# --- 3. Compute county-level measures ---

# 3a. Outdoor share of establishments and payroll
outdoor_indoor_totals <- cbp %>%
  group_by(county_geoid, likely_outdoor_label) %>%
  summarize(
    estab_sum = sum(estab, na.rm = TRUE),
    payann_sum = sum(payann, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = likely_outdoor_label,
    values_from = c(estab_sum, payann_sum),
    values_fill = 0
  )

# Standardize column names from pivot
outdoor_indoor_totals <- outdoor_indoor_totals %>%
  rename_with(~ str_replace_all(str_to_lower(.), " ", "_"))

outdoor_indoor_totals <- outdoor_indoor_totals %>%
  mutate(
    total_estab = estab_sum_likely_outdoors + estab_sum_likely_indoors,
    total_payann = payann_sum_likely_outdoors + payann_sum_likely_indoors,
    cbp_outdoor_share_estab = estab_sum_likely_outdoors / total_estab,
    cbp_outdoor_share_payroll = payann_sum_likely_outdoors / total_payann
  ) %>%
  select(county_geoid, cbp_outdoor_share_estab, cbp_outdoor_share_payroll)

# 3b. HHI of establishments and payroll across NAICS codes
county_totals <- cbp %>%
  group_by(county_geoid) %>%
  summarize(
    total_estab = sum(estab, na.rm = TRUE),
    total_payann = sum(payann, na.rm = TRUE),
    .groups = "drop"
  )

hhi <- cbp %>%
  left_join(county_totals, by = "county_geoid") %>%
  mutate(
    share_estab = estab / total_estab,
    share_payann = payann / total_payann
  ) %>%
  group_by(county_geoid) %>%
  summarize(
    cbp_hhi_estab = sum(share_estab^2, na.rm = TRUE),
    cbp_hhi_payroll = sum(share_payann^2, na.rm = TRUE),
    .groups = "drop"
  )

# --- 4. Combine and compute percentiles ---
county_composition <- outdoor_indoor_totals %>%
  left_join(hhi, by = "county_geoid")


# --- 5. Save ---
write_csv(
  county_composition,
  CBP_EST_AND_EMP_NAICS_CODE_DISTRIBUTION_2018_PATH
)

