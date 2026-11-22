suppressPackageStartupMessages({library(arrow);library(dplyr)})
path <- tempfile(fileext='.parquet')
values <- tibble(tmmn=c(250,273.15,300,NA_real_),tmmx=c(260,280,310,NA_real_))
write_parquet(values,path)
actual <- open_dataset(path) %>%
  mutate(across(c(tmmn,tmmx), ~(.x-273.15)*(9/5)+32)) %>% collect()
expected <- values %>%
  mutate(across(c(tmmn,tmmx), \(temp){(temp-273.15)*(9/5)+32}))
stopifnot(isTRUE(all.equal(actual,expected,tolerance=1e-12)))
unlink(path)
cat('PASS: Arrow dataset conversion matches the original R calculation, including missing values.\n')
