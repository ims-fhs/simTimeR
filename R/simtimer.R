# simtimer converts date-times to a relative timescale based on seconds.
# The functions are faster than any other implementation using dates.
# * datetime functions and variables always refers to date-times
# * sim_ functions and variables always refers to sim_"times"
#
# origin_date needs day-time 00:00:00. Otherwise simtimer leads to wrong results.
# tz will be ignored at the moment.
# datetime is always POSIXct date-time.



#' Days that have passed in sim_datetime
#'
#' sim_date() calculates the number of 24h-intervals that have passed since origin_date. If the
#' origin_date of sim_datetime has a time component different than 00:00:00, the 24h-intervals
#' are correlated to this particular time component.
#'
#' @param sim_datetime A sim_datetime (integer representing the passed seconds since origin_date)
#'
#' @return the number of days (24h-intervals) that have passed since origin_date
#' @export
#' @examples
#' sim_date(24*60*60-1)
#' # [1] 0
#' sim_date(24*60*60)
#' # [1] 1
#' sim_date(452*24*60*60)
#' # [1] 452
#' origin_date <- lubridate::ymd_hms("2016-01-01 00:00:00")
#' sim_date(sim_datetime(lubridate::ymd_hms("2016-01-02 00:01:00"), origin_date))
#' # [1] 1
sim_date <- function(sim_datetime) {
  return(trunc(sim_datetime/(24*60*60)))
}

#' Day-time part of a sim_datetime
#'
#' sim_time() calculates the number of seconds that have passed since the beginning of a day.
#' The beginning of a day is defined by the time component of origin_date which defines the
#' parameter sim_datetime.
#'
#' @param sim_datetime A sim_datetime (integer representing the passed seconds since origin_date)
#'
#' @return the actual day-time in seconds (0-(24*60*60-1))
#' @export
#' @examples
#' sim_time(200)
#' # [1] 200
#' sim_time(24*60*60-1)
#' # [1] 86399
#' sim_time(24*60*60)
#' # [1] 0
#' origin_date <- lubridate::ymd_hms("2016-01-01 00:00:00")
#' sim_time(sim_datetime(lubridate::ymd_hms("2016-01-01 00:01:00"), origin_date))
#' # [1] 60
#' sim_time(sim_datetime(lubridate::ymd_hms("2016-01-02 00:01:00"), origin_date))
#' # [1] 60
sim_time <- function(sim_datetime) {
  return(sim_datetime %% (24*60*60))
}


#' Weekday part of a sim_datetime
#'
#' sim_wday() gives the weekday of a sim_datetime. It's crucial to use the same origin_date for
#' sim_wday() than the origin_date that was used to generate the sim_datetime
#'
#' @param sim_datetime A sim_datetime (integer representing the passed seconds since origin_date)
#' @param origin_date A datetime (POSIXt)
#'
#' @return the abbreviated weekday
#' @export
#' @examples
#' origin_date <- lubridate::ymd_hms("2016-01-01 00:00:00")
#' sim_wday(sim_datetime(lubridate::ymd_hms("2016-01-01 00:01:00"), origin_date), origin_date)
#' # [1] "Fri"
#' sim_wday(sim_datetime(lubridate::ymd_hms("2016-01-02 00:01:00"), origin_date), origin_date)
#' # [1] "Sat"
sim_wday <- function(sim_datetime, origin_date) {
  # tz = "GMT" not considered at the moment. In case of performance problems: Memoize.
  wday <- lubridate::wday(as.POSIXct(sim_datetime, origin = origin_date),
                          label = T, abbr = T)
  return(as.character(wday))
}

#' Transformation from a datetime to a sim_datetime
#'
#' sim_datetime() transforms a regular datetime element (POSIXt) to a sim_datetime element (integer)
#'
#' @param datetime A datetime (POSIXt)
#' @param origin_date A datetime (POSIXt)
#'
#' @return A sim_datetime (integer representing the passed seconds since origin_date)
#' @export
#'
#' @examples
#' origin_date <- lubridate::ymd_hms("2016-01-01 00:00:00")
#' sim_datetime(lubridate::ymd_hms("2016-01-01 00:01:00"), origin_date)
#' # [1] 60
#' sim_datetime(lubridate::ymd_hms("2016-01-02 00:01:00"), origin_date)
#' # [1] 86460
sim_datetime <- function(datetime, origin_date) {
  seconds <- as.integer(as.numeric(datetime - origin_date, units = "secs"))
  # seconds <- sim_date(datetime)*24*60*60 + sim_time(datetime) # does not work any more. Change? SCN
  return(seconds)
}

#' Back-transformation from a sim_datetime to a datetime
#'
#' datetime() transforms a sim_datetime element (integer) to a regular datetime element (POSIXt)
#'
#' @param sim_datetime A sim_datetime (integer representing the passed seconds since origin_date)
#' @param origin_date A datetime (POSIXt)
#'
#' @return datetime A POSIXct
#' @export
#' @examples
#' origin_date <- lubridate::ymd_hms("2016-01-01 00:00:00")
#' datetime(60, origin_date)
#' # [1] "2016-01-01 00:01:00 UTC"
#' datetime(600, origin_date)
#' # [1] "2016-01-01 00:10:00 UTC"
#' datetime(sim_datetime(lubridate::ymd_hms("2016-01-02 00:00:00"), origin_date), origin_date)
#' # [1] "2016-01-02 UTC"
datetime <- function(sim_datetime, origin_date) {
  datetime <- origin_date + sim_datetime
  return(datetime)
}

