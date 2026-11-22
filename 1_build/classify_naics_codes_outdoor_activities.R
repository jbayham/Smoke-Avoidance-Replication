
library(tidyverse)

source(file.path(Sys.getenv('SMOKE_AVOIDANCE_PROJECT_DIR', unset = getwd()), 'project_helpers_and_constants.R'))

# https://www.census.gov/naics/?48967
naics_code_descriptions <- readxl::read_xlsx(NAICS_CODES_DESCRIPTIONS_PATH)


likely_outdoor_descriptions <- c(
  'Zoos and Botanical Gardens',
  'Nature Parks and Other Similar Institutions',
  'Amusement and Theme Parks',
  'Golf Courses and Country Clubs',
  'RV (Recreational Vehicle) Parks and Campgrounds',
  'Skiing Facilities',
  'Racetracks',
  'Marinas'
) 


naics_code_descriptions_filtered <- naics_code_descriptions %>% 
  select(-`...3`) %>% 
  janitor::clean_names() %>% 
  filter(
    !is.na(x2017_naics_code),
    str_sub(x2017_naics_code, end=2) %in% c('71', '72')
  ) %>% 
  mutate(
    likely_outdoor_label = case_when(
      x2017_naics_title %in% likely_outdoor_descriptions ~ 'Likely Outdoors',
      TRUE ~ 'Likely Indoors'
    )
  ) %>% 
  rename(naics_description = x2017_naics_title, naics_code=x2017_naics_code) %>%
  # The foot-traffic delivery codes some places only to the 4-digit industry group, which
  # the 6-digit title file does not list. These two carry real visit volume (7212 has more
  # visits than its 6-digit children combined), so they are classified by hand.
  mutate(naics_code = as.character(naics_code)) %>%
  bind_rows(tribble(
    ~naics_code, ~naics_description,                                      ~likely_outdoor_label,
    '7212',      'RV (Recreational Vehicle) Parks and Recreational Camps', 'Likely Outdoors',
    '7111',      'Performing Arts Companies',                              'Likely Indoors'
  ))

write_csv(naics_code_descriptions_filtered, NAICS_CODES_INDOOR_OUTDOOR_CLASSIFICATIONS_PATH)


# --- LaTeX table ---------------------------------------------------------------------------
# Every code the classification covers appears, split into the two labels, so the appendix
# shows the full set it is applied to, not just the outdoor side. That is the 40 six-digit
# codes in sectors 71--72 plus the two four-digit industry groups added by hand above (7111,
# 7212). A four-digit group is listed just before its six-digit industries and marked with a
# dagger; the table note explains that some places in the foot-traffic data are coded only to
# four digits. Each panel is laid out as two side-by-side code/title pairs, read down the left
# block and then down the right, so all 42 codes fit on one appendix page.
escape_tex <- function(x) {
  x %>%
    str_replace_all("\\\\", "\\\\textbackslash{}") %>%
    str_replace_all("([&%$#_{}])", "\\\\\\1") %>%
    str_replace_all("~", "\\\\textasciitilde{}") %>%
    str_replace_all("\\^", "\\\\textasciicircum{}")
}

# Sort on the code right-padded with zeros to six digits, so a four-digit group sits just
# before its own six-digit industries (7111 -> 711100, ahead of 711110; 7212 -> 721200, ahead
# of 721211). A plain text sort would put 7212 after every 7139xx code.
table_data <- naics_code_descriptions_filtered %>%
  mutate(naics_code = as.character(naics_code)) %>%
  arrange(likely_outdoor_label, str_pad(naics_code, 6, side = 'right', pad = '0')) %>%
  select(likely_outdoor_label, naics_code, naics_description)

rows_for <- function(label) {
  d <- table_data %>%
    filter(likely_outdoor_label == label) %>%
    # the codes come off the spreadsheet as doubles; pad cells must match them as text
    mutate(naics_description = escape_tex(naics_description),
           # dagger marks the four-digit industry groups (see the table note)
           naics_code = if_else(nchar(naics_code) == 4, paste0(naics_code, "$^\\dagger$"), naics_code))
  half  <- ceiling(nrow(d) / 2)
  left  <- d[seq_len(half), ]
  right <- d[setdiff(seq_len(nrow(d)), seq_len(half)), ] %>%
    bind_rows(tibble(naics_code = rep("", half - (nrow(d) - half)), naics_description = ""))
  paste0("   ", left$naics_code, " & ", left$naics_description, " & ",
         right$naics_code, " & ", right$naics_description, " \\\\")
}

panel <- function(title, rows) c(
  "   \\midrule",
  paste0("   \\multicolumn{4}{l}{\\emph{", title, "}} \\\\"),
  "   \\midrule",
  rows
)

n_outdoor <- sum(table_data$likely_outdoor_label == "Likely Outdoors")
n_indoor  <- sum(table_data$likely_outdoor_label == "Likely Indoors")

tex <- c(
  "\\begingroup",
  "\\centering",
  "\\footnotesize",
  "\\setlength{\\tabcolsep}{4pt}",
  "\\begin{tabular}{@{}lp{0.38\\linewidth}@{\\hspace{1em}}lp{0.38\\linewidth}@{}}",
  "   \\midrule \\midrule",
  "   Code & Title & Code & Title \\\\",
  panel(paste0("Panel A: Likely outdoors (", n_outdoor, " codes)"), rows_for("Likely Outdoors")),
  panel(paste0("Panel B: Likely indoors (", n_indoor, " codes)"), rows_for("Likely Indoors")),
  "   \\midrule \\midrule",
  "\\end{tabular}",
  "\\par\\endgroup"
)

tex_path <- file.path(APPENDIX_TABLES_DIR, "appendix_table_indoor_outdoor_classifications.tex")
writeLines(tex, tex_path)
cat("\nsaved", tex_path, "\n")
