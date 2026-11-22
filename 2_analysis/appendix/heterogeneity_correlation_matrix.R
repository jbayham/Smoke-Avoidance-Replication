# Correlation matrix of the county characteristics in the main panels of figure 2 (the
# day-of heterogeneity figure drawn by figure_2_day_of_analysis_heterogeneities_plot.R).
# Weekend is the fifth panel of that figure, but it is a calendar indicator: in the balanced
# panel every county carries the same days, so it is orthogonal to any county characteristic
# by construction and is left out of the matrix (the script checks that the panel really is
# balanced).
#
# The characteristics are county-level and time invariant, and the quartiles in the
# regressions are cut across counties, so the correlations are taken across counties rather
# than county-days. With every county carrying the same number of days, weighting by
# county-days would give the same numbers. The sample is the counties in the balanced panel
# that the regressions are fit on, with all four characteristics observed.
#
# Pearson correlations sit below the diagonal and Spearman rank correlations above it. The
# regressions use quartile dummies rather than levels, so the rank correlation is the closer
# guide to how much the quartile groups overlap.

library(tidyverse)
library(data.table)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

# In the order of VARS_IN_MAIN_SAMPLE in the figure script
MAIN_HETEROGENEITY_VARS <- c(
  median_household_income         = "Median Household Income",
  rurality_index                  = "Rurality Index",
  pm_background_mean              = "Background PM",
  natural_amenity_tourism_measure = "Natural Amenity Tourism"
)

# --- Counties in the regression sample ---------------------------------------------------
# Only the two columns needed are read from the 8.5 GB panel
cat("Reading the balanced-panel counties...\n")
panel_counties <- fread(DAILY_DATA_FILEPATH, select = c("county_geoid", "balanced_panel"),
                        colClasses = list(character = "county_geoid"))
panel_counties <- panel_counties[balanced_panel %in% TRUE, .(n_days = .N), by = county_geoid]
cat("Balanced-panel counties:", nrow(panel_counties), "\n")
cat("Days per county:", paste(unique(panel_counties$n_days), collapse = ", "), "\n")
stopifnot(n_distinct(panel_counties$n_days) == 1)

# --- County characteristics ----------------------------------------------------------------
county_data <- read_csv(COUNTY_INFO_COMBINED_PATH,
                        col_select = all_of(c("county_geoid", names(MAIN_HETEROGENEITY_VARS))),
                        col_types = cols(county_geoid = col_character(), .default = col_double()),
                        show_col_types = FALSE) %>%
  filter(!is.na(county_geoid)) %>%
  distinct(county_geoid, .keep_all = TRUE) %>%
  semi_join(panel_counties, by = "county_geoid")

cat("\nMissing values among the balanced-panel counties:\n")
print(colSums(is.na(county_data[names(MAIN_HETEROGENEITY_VARS)])))

county_data <- county_data %>% drop_na(all_of(names(MAIN_HETEROGENEITY_VARS)))
n_counties <- nrow(county_data)
cat("Counties with all four characteristics:", n_counties, "\n")

# --- Correlations ------------------------------------------------------------------------
X <- as.matrix(county_data[names(MAIN_HETEROGENEITY_VARS)])
colnames(X) <- unname(MAIN_HETEROGENEITY_VARS)
pearson  <- cor(X, method = "pearson")
spearman <- cor(X, method = "spearman")

cat("\nPearson:\n");  print(round(pearson, 3))
cat("\nSpearman:\n"); print(round(spearman, 3))

# Pearson below the diagonal, Spearman above
combined <- pearson
combined[upper.tri(combined)] <- spearman[upper.tri(spearman)]

write_csv(
  bind_rows(
    as_tibble(pearson,  rownames = "variable") %>% mutate(method = "pearson"),
    as_tibble(spearman, rownames = "variable") %>% mutate(method = "spearman")
  ) %>% relocate(method),
  file.path(APPENDIX_TABLES_DIR, "heterogeneity_correlation_matrix.csv")
)

# --- LaTeX table ---------------------------------------------------------------------------
k <- ncol(combined)
cells <- matrix(sprintf("%.2f", combined), k, k)
diag(cells) <- "1"
# columns are numbered so the header stays narrow
rows <- map_chr(seq_len(k), \(i) paste0("(", i, ") ", colnames(combined)[i], " & ",
                                        paste(cells[i, ], collapse = " & "), " \\\\"))
tex <- c(
  "\\begingroup",
  "\\centering",
  paste0("\\begin{tabular}{l", strrep("c", k), "}"),
  "   \\midrule \\midrule",
  paste0("    & ", paste0("(", seq_len(k), ")", collapse = " & "), " \\\\"),
  "   \\midrule",
  paste0("   ", rows),
  "   \\midrule \\midrule",
  paste0("   \\multicolumn{", k + 1, "}{l}{\\emph{Counties: ", format(n_counties, big.mark = ","),
         ". Pearson below the diagonal, Spearman above.}}\\\\"),
  "\\end{tabular}",
  "\\par\\endgroup"
)
tex_path <- file.path(APPENDIX_TABLES_DIR, "appendix_table_heterogeneity_correlations.tex")
writeLines(tex, tex_path)
cat("\nsaved", tex_path, "\n")

# --- Heatmap -------------------------------------------------------------------------------
heatmap_data <- as_tibble(combined, rownames = "row") %>%
  pivot_longer(-row, names_to = "col", values_to = "r") %>%
  mutate(row = factor(row, levels = rev(colnames(combined))),
         col = factor(col, levels = colnames(combined)),
         # the diagonal is greyed out so it does not outshout the off-diagonal cells
         r = if_else(row == col, NA_real_, r),
         label = if_else(row == col, "", sprintf("%.2f", r)))

heatmap <- ggplot(heatmap_data, aes(x = col, y = row, fill = r)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = label), size = 7) +
  scale_fill_gradient2("Correlation", low = "#2166AC", mid = "white", high = "#B2182B",
                       midpoint = 0, limits = c(-1, 1), na.value = "grey90") +
  scale_x_discrete(labels = \(x) str_wrap(x, 12), position = "top") +
  labs(x = NULL, y = NULL,
       caption = paste0("Pearson below the diagonal, Spearman above. ",
                        format(n_counties, big.mark = ","), " balanced-panel counties.")) +
  theme_minimal(base_size = 20) +
  theme(panel.grid = element_blank())

fig_path <- file.path(APPENDIX_FIGURES_DIR, "heterogeneity_correlation_matrix.pdf")
ggsave(fig_path, heatmap, width = 11, height = 8, units = "in")
cat("saved", fig_path, "\n")
