# Appendix table: the neighbor exposure profile behind the SDEM "typical episode"
# effect, from spatial_substitution_estimate_run.R.
#
#   appendix_table_sdem_neighbor_profile.tex
#       Row k, column b: wbar_{k,b} = E[ sum_c' w_{c,c'} 1{PM_{c',d} in bin_b} |
#       PM_{c,d} in bin_k ], the average W-weighted share of a county's neighbors in
#       bin b on days the county itself is in bin k. The last column is the remaining
#       share, neighbors with no smoke. The typical episode effect for bin k is
#       gamma_k + sum_b wbar_{k,b} delta_b.

library(tidyverse)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

results <- readRDS(file.path(REGRESSION_RESULTS_DIR,
                             "spatial_sdem_v4_dcs_units_results.rds"))
profile <- results$neighbor_profile

BINS       <- c("bin_d_001_015", "bin_d_015_030", "bin_d_030_060",
                "bin_d_060_100", "bin_d_100_200", "bin_d_200_500")
BIN_LABELS <- c("(0, 15]", "(15, 30]", "(30, 60]", "(60, 100]", "(100, 200]", "(200, 500]")
stopifnot(identical(rownames(profile), BINS), identical(colnames(profile), BINS))

no_smoke <- 1 - rowSums(profile)
stopifnot(all(no_smoke > -1e-8))
print(round(cbind(profile, no_smoke = no_smoke), 3))

num  <- function(x) sprintf("%.3f", x)
rows <- sapply(seq_along(BINS), function(k)
  paste0("   ", BIN_LABELS[k], " & ",
         paste(num(c(profile[k, ], no_smoke[k])), collapse = " & "), "\\\\"))

tex <- c(
  "",
  "\\begingroup",
  "\\centering",
  "\\begin{tabular}{lccccccc}",
  "   \\midrule \\midrule",
  "   Own-county & \\multicolumn{7}{c}{Weighted share of neighbors in smoke bin} \\\\",
  "   \\cmidrule(lr){2-8}",
  paste0("   smoke bin & ", paste(BIN_LABELS, collapse = " & "), " & No smoke\\\\"),
  "   \\midrule",
  rows,
  "   \\midrule \\midrule",
  "\\end{tabular}",
  "\\par\\endgroup",
  ""
)

out_path <- file.path(APPENDIX_TABLES_DIR, "appendix_table_sdem_neighbor_profile.tex")
writeLines(tex, out_path)
cat("\nsaved", out_path, "\n")
