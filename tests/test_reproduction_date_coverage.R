source('scripts/check_date_coverage.R')
jan <- seq(as.Date('2019-01-01'),as.Date('2019-01-31'),by='day')
jan_double <- structure(as.double(jan),class='Date')
jan_integer <- structure(as.integer(jan),class='Date')
stopifnot(check_date_coverage(jan_double,'2019-01','spend')$ok,
          check_date_coverage(jan_integer,'2019-01','spend')$ok,
          !check_date_coverage(jan[-15],'2019-01','spend')$ok,
          !check_date_coverage(c(jan,as.Date('2019-02-01')),'2019-01','spend')$ok,
          !check_date_coverage(c(jan,as.Date(NA)),'2019-01','spend')$ok)
nov <- seq(as.Date('2023-11-01'),as.Date('2023-11-30'),by='day')
stopifnot(check_date_coverage(nov[-30],'2023-11','visits')$ok,
          identical(check_date_coverage(nov[-30],'2023-11','visits')$allowed_missing,'2023-11-30'),
          !check_date_coverage(nov[-30],'2023-11','spend')$ok,
          !check_date_coverage(nov[-c(15,30)],'2023-11','visits')$ok,
          check_date_coverage(nov,'2023-11','visits')$ok)
leap <- seq(as.Date('2020-02-01'),as.Date('2020-02-29'),by='day')
stopifnot(check_date_coverage(leap,'2020-02','visits')$ok,
          !check_date_coverage(leap[-29],'2020-02','visits')$ok)
cat('PASS: date storage types, documented gap, unexpected gaps, extra dates, NA and leap day.\n')
