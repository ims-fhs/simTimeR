# Helper file. Set global test parameters available in all testthat sections
# Use helper-synthetic_data for synthetic data like data.frames, ...


origin_date <- lubridate::ymd_hms("2016-01-01 00:00:00")
t_20160101 <- lubridate::ymd_hms("2016-01-01 00:00:00") # = origin_date!
t_20160102 <- lubridate::ymd_hms("2016-01-02 00:00:00")
t_20170101 <- lubridate::ymd_hms("2017-01-01 00:00:00")

# Set upper bounds for microbenchmark tests. Set units via
# microbenchmark::microbenchmark(function(), times = nL, unit = "us"):
t_500us <- 500 # 500 us

