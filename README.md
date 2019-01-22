
<!-- README.md is generated from README.Rmd. Please edit that file -->
simtimer
========

simtimer in a little R package designed to simplify (and speeding up) calculating time intervals in discrete event simulations.

Discrete event simulations is a simulation paradigm that is based on the evaluation of events taking place in a time-specific order. Therefore a discrete event simulation calculates many time intervals. simtimer handles dates and times as integers. This makes working with time intervals as easy (and as fast) as subtracting integers.

Installation
------------

You can install simtimer from `CRAN` or `github` with:

``` r
install.packages("simtimer")

# install.packages("devtools")
devtools::install_github("ims-fhs/simtimer")
```

Example
-------

Simtimer allows transformation between datetimes (POSIXt) and sim\_datetimes (integer) with `sim_datetime()` and `datetime()`.

``` r
origin_date <- as.POSIXct("2016-01-01 00:00:00", tz = "UTC")
my_datetime <-  as.POSIXct("2016-01-02 01:01:01", tz = "UTC")
my_simdatetime <- as.sim_datetime(my_datetime, origin_date)
my_simdatetime
#> [1] 90061
```

``` r
as.datetime(my_simdatetime, origin_date)
#> [1] "2016-01-02 01:01:01 UTC"
```

Simtimer allows to manipulate sim\_datetimes and extract parts of sim\_datetimes with `sim_time()`, `sim_wday()` and `sim_date()`.

``` r
sim_time(my_simdatetime)
#> [1] 3661
sim_wday(my_simdatetime, origin_date)
#> [1] "6"
sim_date(my_simdatetime)
#> [1] 1
```
