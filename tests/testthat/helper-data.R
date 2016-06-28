# Helper file. Set global test parameters available in all testthat sections
# Use helper-synthetic_data for synthetic data like data.frames, ...

# All names (month, weekday, ...) in english!
# Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252") .... Needed?
# library(lubridate) ........................................................... Needed?

origin_date = lubridate::ymd_hms("2016-01-01 00:00:00")
test_date <- lubridate::ymd_hms("2016-06-16 14:16:53 CEST") # => "Thurs"
test_date2 <- lubridate::ymd_hms("2016-06-16 23:59:59 CEST") # => "Thurs"

