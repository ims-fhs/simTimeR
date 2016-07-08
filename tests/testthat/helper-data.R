# Helper file. Set global test parameters available in all testthat sections
# Use helper-synthetic_data for synthetic data like data.frames, ...

# All names (month, weekday, ...) in english!
Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252") # .... Needed!!! But not for simTimeR...

path <- ""
data <- imsbasics::load_rdata("Z-testdata_bdmchv_LUKS2015", path)
r_vehicles <- data$hist_vehicles # ............................................. synthetic data with s_variable?

origin_date = lubridate::ymd_hms("2016-01-01 00:00:00")
test_date <- lubridate::ymd_hms("2016-06-16 14:16:53 CEST") # => "Thurs"
test_date2 <- lubridate::ymd_hms("2016-06-16 23:59:59 CEST") # => "Thurs"
test_sec <- date2simtime(test_date, origin_date)

test_interval2015 <- lubridate::interval(lubridate::ymd_hms("2015-01-01 00:00:00"),
  lubridate::ymd_hms("2016-01-01 00:00:00"))
test_interval2016 <- lubridate::interval(lubridate::ymd_hms("2016-01-01 00:00:00"),
  lubridate::ymd_hms("2017-01-01 00:00:00"))
test_interval1 <- lubridate::interval(lubridate::ymd_hms("2016-02-01 12:00:00"),
  lubridate::ymd_hms("2019-10-27 12:00:00"))
test_interval2 <- lubridate::interval(lubridate::ymd_hms("2016-02-01 12:00:00"),
  lubridate::ymd_hms("2019-01-01 00:00:00"))
test_interval3 <- lubridate::int_shift(test_interval1, lubridate::dyears(0.5)) # start/ends at 12:00!
