# simtimer converts datetimes to a relative timescale based on seconds for the
# usage inside Discrete-Event Simulation (DES) engines. DES engines have to calculate
# many timestamps and intervalls during a simulation run. The representation of
# datetimes as integers helps to speed up these calculations.
#
# * Functions called sim_something() refer to the relative integer timescale called "sim_datetime".
# * The functions as.sim_datetime() and as.datetime() transform datetimes to sim_datetimes and back.


#' Date part of a sim_datetime
#'
#' sim_date() returns the date part of a sim_datetime.
#' Therefore sim_date() calculates the number of days (24h-intervals) that have
#' passed since origin_date. If the origin_date of sim_datetime has a time component
#' different than 00:00:00, the 24h-intervals are correlated to this particular time component.
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
#' origin_date <- as.POSIXct("2016-01-01 00:00:00", tz = "UTC")
#' sim_date(as.sim_datetime(as.POSIXct("2016-01-02 00:01:00", tz = "UTC"), origin_date))
#' # [1] 1
sim_date <- function(sim_datetime) {
  return(trunc(sim_datetime/(24*60*60)))
}

#' Time part of a sim_datetime
#'
#' sim_time() returns the time of a sim_datetime in seconds.
#' The beginning of a day is defined by the time component of origin_date which defines the
#' parameter sim_datetime.
#'
#' @param sim_datetime A sim_datetime (integer representing the passed seconds since origin_date)
#'
#' @return time in seconds (Range: 0-(24*60*60-1))
#' @export
#' @examples
#' sim_time(200)
#' # [1] 200
#' sim_time(24*60*60-1)
#' # [1] 86399
#' sim_time(24*60*60)
#' # [1] 0
#' origin_date <- as.POSIXct("2016-01-01 00:00:00", tz = "UTC")
#' sim_time(as.sim_datetime(as.POSIXct("2016-01-01 00:01:00", tz = "UTC"), origin_date))
#' # [1] 60
#' sim_time(as.sim_datetime(as.POSIXct("2016-01-02 00:01:00", tz = "UTC"), origin_date))
#' # [1] 60
sim_time <- function(sim_datetime) {
  return(sim_datetime %% (24*60*60))
}


#' Weekday part of a sim_datetime
#'
#' sim_wday() returns the weekday of a sim_datetime. It's crucial to use the same origin_date for
#' sim_wday() than the origin_date that was used to generate the sim_datetime.
#' sim_wday() uses the base R format(x, "\%u") function.
#'
#' @param sim_datetime A sim_datetime (integer representing the passed seconds since origin_date)
#' @param origin_date A datetime (POSIXt)
#'
#' @return A character, giving the weekday number ("1" = Monday, "2" = Tuesday, ..., "7" = Sunday)
#' @export
#' @examples
#' origin_date <- as.POSIXct("2016-01-01 00:00:00", tz = "UTC")
#' sim_wday(60, origin_date)
#' sim_wday(3600,origin_date)
#' sim_wday(36*3600,origin_date)
sim_wday <- function(sim_datetime, origin_date) {
  return(as.character(format(origin_date + sim_datetime, "%u")))
}

#' Transformation from a datetime to a sim_datetime
#'
#' as.sim_datetime() transforms a regular datetime element (POSIXt) to a sim_datetime
#' (integer representing the passed seconds since origin_date).
#' The timezone (tz) will be ignored at the moment.
#' Therefore tz of datetime and origin_date should be identical.
#'
#' @param datetime A datetime (POSIXt)
#' @param origin_date A datetime (POSIXt)
#'
#' @return A sim_datetime
#' @export
#'
#' @examples
#' origin_date <- as.POSIXct("2016-01-01 00:00:00", tz = "UTC")
#' as.sim_datetime(as.POSIXct("2016-01-01 00:01:00", tz = "UTC"), origin_date)
#' # [1] 60
#' as.sim_datetime(as.POSIXct("2016-01-02 00:01:00", tz = "UTC"), origin_date)
#' # [1] 86460
as.sim_datetime <- function(datetime, origin_date) {
  return(as.integer(as.numeric(datetime - origin_date, units = "secs")))
}

#' Back-transformation from a sim_datetime to a datetime
#'
#' as.datetime() transforms a sim_datetime element (integer) to a regular datetime
#' element (POSIXt)
#'
#' @param sim_datetime A sim_datetime (integer representing the passed seconds since origin_date)
#' @param origin_date A datetime (POSIXt)
#'
#' @return datetime A POSIXt
#' @export
#' @examples
#' origin_date <- as.POSIXct("2016-01-01 00:00:00", tz = "UTC")
#' as.datetime(60, origin_date)
#' # [1] "2016-01-01 00:01:00 UTC"
#' as.datetime(600, origin_date)
#' # [1] "2016-01-01 00:10:00 UTC"
#' as.datetime(as.sim_datetime(as.POSIXct("2016-01-02 00:00:00", tz = "UTC"), origin_date),
#' origin_date)
#' # [1] "2016-01-02 UTC"
as.datetime <- function(sim_datetime, origin_date) {
  return(origin_date + sim_datetime)
}

