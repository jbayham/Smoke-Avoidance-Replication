# ============================================================================
# build_spatial_weight_matrix.R
#
# Builds the spatial weight matrix W and the balanced county/date panel the
# spatial-substitution SDEM reads, and writes them to ONE self-contained cache
# (sdem_prepared_panel.rds) that spatial_substitution_estimate_run.R consumes
# through sdem_v4_load_panel().
#
# The cache carries, in a single panel_df (sorted county-then-date):
#   county_geoid_str, spend_date_range_start   keys
#   bin_d_*                                     the six RAW own-county smoke bins
#   y                                           the RAW outcome
#                                               (asinh_daily_visits_deferrable)
#   binned_pr / tmmn / tmmx / rmax              weather bin FE factors
#   day_of_week, season                         calendar FE factors
# plus  W, county_ids, dates_bal, bad_dates, n_dates.
#
# No FWL / demean residualization happens here. The estimation puts every
# control in X and filters it by B = I - lambda W itself, so the only thing the
# build has to hand it is the RAW data and the weights.
#
# WHAT COUNTS AS A NEIGHBOR, AND HOW IT IS WEIGHTED
# W is inverse DRIVE TIME (not inverse road distance), row-standardized,
# restricted to counties reachable within NEIGHBOR_MAX_HOURS. Drive time is the
# substitution-relevant cost: a deferrable trip is given up or moved because of
# how long the drive takes, not how many kilometres it covers.
# ============================================================================

library(tidyverse)
library(arrow)
library(Matrix)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

say <- function(...) cat(format(Sys.time(), "[%H:%M:%S] "), ..., "\n", sep = "")

# W is inverse drive time over neighbors reachable within this many hours.
NEIGHBOR_MAX_HOURS <- 4

# Sub-minute drives are not a meaningful separation between two counties, and
# 1/t would hand them an unbounded share of their row. Two pairs sit below this
# floor (Charleston SC -> Berkeley SC at 0.41 min, Middlesex VA -> Lancaster VA
# at 0.59 min); it is the drive-time analogue of the old 0.1 km distance floor.
MIN_DRIVE_HOURS <- 1 / 60

CACHE_PATH <- file.path(REGRESSION_RESULTS_DIR, "sdem_prepared_panel.rds")

SMOKE_VARS   <- c("bin_d_001_015", "bin_d_015_030", "bin_d_030_060",
                  "bin_d_060_100", "bin_d_100_200", "bin_d_200_500")
WEATHER_VARS <- c("binned_pr", "binned_tmmn", "binned_tmmx", "binned_rmax")
Y_RAW_VAR    <- "asinh_daily_visits_deferrable"

say("Output cache: ", CACHE_PATH)
say("Drive-time cutoff: ", NEIGHBOR_MAX_HOURS, " h")

# ============================================================================
# 1. Read the daily data and build the balanced, fully-complete county panel
# ============================================================================

say("Reading daily data")
# Loaded for the sample definition but not all are stored: state_year_month and
# week enter only the completeness rule (kept so the county/date sample is the
# same one the paper uses); the design rebuilds the state x year-month effect
# from county_ids + dates, so it is not cached.
fe_vars    <- c("state_year_month", "week", "day_of_week", "season")
model_vars <- c(Y_RAW_VAR, SMOKE_VARS, WEATHER_VARS)

full_df <- read_csv_arrow(
  DAILY_DATA_FILEPATH,
  col_select = c("county_geoid", "spend_date_range_start",
                 all_of(model_vars), all_of(fe_vars))
) %>%
  mutate(
    county_geoid_str = str_pad(as.character(county_geoid), width = 5, side = "left", pad = "0"),
    across(c(all_of(WEATHER_VARS), day_of_week, season, state_year_month, week), as.factor)
  ) %>%
  filter(!is.na(spend_date_range_start))

say("Rows read: ", nrow(full_df))

# --- Drop systematically-missing dates --------------------------------------
# (Nov 30 of every year is missing for ~all counties -- a data-pipeline
# artifact, not a real gap in any one county's coverage.)
full_df <- full_df %>%
  mutate(complete = if_all(all_of(c(model_vars, fe_vars)), ~ !is.na(.)))

bad_dates <- full_df %>%
  group_by(spend_date_range_start) %>%
  summarize(frac_incomplete = mean(!complete), .groups = "drop") %>%
  filter(frac_incomplete > 0.5) %>%
  pull(spend_date_range_start)

say("Dropping ", length(bad_dates), " systematically-incomplete dates: ",
    paste(as.character(sort(bad_dates)), collapse = ", "))

full_df       <- full_df %>% filter(!(spend_date_range_start %in% bad_dates))
n_dates_total <- n_distinct(full_df$spend_date_range_start)
say("Dates remaining: ", n_dates_total)

# --- Balanced, fully-complete county panel ----------------------------------
# The SDEM needs the same county set on every day: a balanced county/date grid.
balanced_counties <- full_df %>%
  group_by(county_geoid_str) %>%
  summarize(n = n(), all_complete = all(complete), .groups = "drop") %>%
  filter(n == n_dates_total, all_complete) %>%
  pull(county_geoid_str)

say("Balanced, fully-complete counties: ", length(balanced_counties),
    " out of ", n_distinct(full_df$county_geoid_str))

county_ids <- sort(balanced_counties)

panel_df <- full_df %>%
  filter(county_geoid_str %in% county_ids) %>%
  arrange(county_geoid_str, spend_date_range_start)
rm(full_df); gc()

say("Balanced panel rows: ", nrow(panel_df),
    " ( ", length(county_ids), " counties x ", n_dates_total, " days )")
stopifnot(nrow(panel_df) == length(county_ids) * n_dates_total)

# ============================================================================
# 2. Spatial weights: inverse drive time, row-standardized
# ============================================================================
# Built on EXACTLY the surviving county set. The drive time is
# travel_times_filled, except where the route it came from was rejected. A
# routed distance at or below the straight-line distance is not a measurement
# of THIS pair -- OSRM snapped an endpoint somewhere else (county polygons
# include coastal water, so a "nearest point" can sit in open water and snap
# back to the origin's own shore). Those fall back to the build's own
# convention for unroutable pairs, euclidean / 40 kph, which still understates
# the drive but by a bounded amount.

say("Reading county neighbors with drive times")
neighbor_df <- read_csv_arrow(COUNTY_NEIGHBORS_500KM_DRIVE_TIME_CSV_PATH) %>%
  mutate(
    source_geoid   = str_pad(as.character(source_geoid),   width = 5, side = "left", pad = "0"),
    neighbor_geoid = str_pad(as.character(neighbor_geoid), width = 5, side = "left", pad = "0")
  )

pair_df <- neighbor_df %>%
  filter(source_geoid %in% county_ids, neighbor_geoid %in% county_ids,
         source_geoid != neighbor_geoid) %>%
  mutate(
    route_ok    = !is.na(road_distances_extracted) &
                    road_distances_extracted > euclidean_distance,
    drive_hours = pmax(if_else(route_ok, travel_times_filled,
                               euclidean_distance / 40), MIN_DRIVE_HOURS)
  ) %>%
  filter(drive_hours <= NEIGHBOR_MAX_HOURS) %>%
  group_by(source_geoid, neighbor_geoid) %>%
  slice_min(drive_hours, n = 1, with_ties = FALSE) %>%
  ungroup()

say("Pairs within ", NEIGHBOR_MAX_HOURS, " h: ", nrow(pair_df))

# A county with no neighbor inside the cutoff gives an all-zero row of W, which
# is not row-standardizable. Iterate to a fixed point: dropping an isolate can
# orphan a county that had it as its only reachable neighbor. Ten counties have
# no neighbor within 4 h to begin with -- mostly enormous interior-West
# counties whose centroid genuinely is that far from any neighbor's edge (San
# Bernardino, Pershing, Gunnison), plus Inyo and Owyhee, whose centroids snap
# far from the road network and so are measured from the wrong place.
repeat {
  keep <- pair_df %>%
    filter(source_geoid %in% county_ids, neighbor_geoid %in% county_ids) %>%
    pull(source_geoid) %>% unique() %>% sort()
  dropped <- setdiff(county_ids, keep)
  if (length(dropped) == 0) break
  say("Dropping ", length(dropped), " counties with no neighbor within ",
      NEIGHBOR_MAX_HOURS, " h: ", paste(head(dropped, 12), collapse = ", "),
      if (length(dropped) > 12) ", ..." else "")
  county_ids <- keep
}

pair_df  <- pair_df %>% filter(source_geoid %in% county_ids,
                               neighbor_geoid %in% county_ids)
panel_df <- panel_df %>% filter(county_geoid_str %in% county_ids)
stopifnot(setequal(county_ids, unique(pair_df$source_geoid)))
say("Counties retained: ", length(county_ids))

weights_df <- pair_df %>%
  mutate(inv_time = 1 / drive_hours) %>%
  group_by(source_geoid) %>%
  mutate(weight = inv_time / sum(inv_time)) %>%
  ungroup()

W <- sparseMatrix(
  i = match(weights_df$source_geoid, county_ids),
  j = match(weights_df$neighbor_geoid, county_ids),
  x = weights_df$weight,
  dims = c(length(county_ids), length(county_ids)),
  dimnames = list(county_ids, county_ids)
)
say("W: ", nrow(W), " x ", ncol(W), ", ", length(W@x), " nonzeros, ",
    "mean neighbors = ", round(length(W@x) / nrow(W), 1))
stopifnot(all(abs(Matrix::rowSums(W) - 1) < 1e-8))

# ============================================================================
# 3. Assemble and cache the self-contained prepared panel
# ============================================================================
# panel_df is already sorted county-then-date and restricted to the retained
# county set; rename the raw outcome to `y` and keep only the columns the
# design reads. Everything is aligned by construction -- no join, no separate
# base cache to realign.

dates_bal <- sort(unique(panel_df$spend_date_range_start))

panel_df <- panel_df %>%
  rename(y = all_of(Y_RAW_VAR)) %>%
  select(county_geoid_str, spend_date_range_start, all_of(SMOKE_VARS), y,
         all_of(WEATHER_VARS), day_of_week, season) %>%
  mutate(across(c(all_of(WEATHER_VARS), day_of_week, season), droplevels))

stopifnot(nrow(panel_df) == length(county_ids) * length(dates_bal),
          !anyNA(panel_df$y))

prep <- list(
  panel_df   = panel_df,
  W          = W,
  county_ids = county_ids,
  dates_bal  = dates_bal,
  bad_dates  = bad_dates,
  n_dates    = length(dates_bal)
)
saveRDS(prep, CACHE_PATH, compress = FALSE)
say("Cached prepared panel to ", CACHE_PATH,
    " (N = ", length(county_ids), ", T = ", length(dates_bal), ")")
say("DONE")
