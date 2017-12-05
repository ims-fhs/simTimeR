# simtimer converts date-times to a relative timescale based on seconds.
# The functions are faster than any other implementation using dates.
# * datetime functions and variables always refers to date-times
# * sim_ functions and variables always refers to sim_"times"
#
# origin_date needs day-time 00:00:00. Otherwise simtimer leads to wrong results.
# tz will be ignored at the moment.
# datetime is always POSIXct date-time.

#' Return day of year from rel. time in seconds.
#'
#' @param sim_datetime A sim_datetime
#'
#' @return the day of the period (> 0, in many cases also < 365)
#' @export
#' @examples origin_date <- lubridate::ymd_hms("2016-01-01 00:00:00")
#' sim_date(sim_datetime(lubridate::ymd_hms("2016-01-01 00:01:00"), origin_date))
#' # [1] 0
#' sim_date(sim_datetime(lubridate::ymd_hms("2016-01-02 00:01:00"), origin_date))
#' # [1] 1
sim_date <- function(sim_datetime) {
  return(trunc(sim_datetime/(24*60*60)))
}

#' Day-time part of a sim_datetime
#'
#' @param sim_datetime A sim_datetime
#'
#' @return the actual day-time in seconds (0-(24*60*60-1))
#' @export
#' @examples origin_date <- lubridate::ymd_hms("2016-01-01 00:00:00")
#' sim_time(sim_datetime(lubridate::ymd_hms("2016-01-01 00:01:00"), origin_date))
#' # [1] 60
#' sim_time(sim_datetime(lubridate::ymd_hms("2016-01-02 00:01:00"), origin_date))
#' # [1] 60
sim_time <- function(sim_datetime) {
  return(sim_datetime %% (24*60*60))
}

#' Weekday of a sim_datetime
#'
#' @param sim_datetime A sim_datetime
#' @param origin_date A datetime
#'
#' @return the weekday in the format: "Mo, Di, Mi, Do, Fr, Sa, So"
#' @export
#' @examples origin_date <- lubridate::ymd_hms("2016-01-01 00:00:00")
#' sim_wday(sim_datetime(lubridate::ymd_hms("2016-01-01 00:01:00"), origin_date), origin_date)
#' # [1] "Fri"
sim_wday <- function(sim_datetime, origin_date) {
  # tz = "GMT" not considered at the moment. In case of performance problems: Memoize.
  wday <- lubridate::wday(as.POSIXct(sim_datetime, origin = origin_date),
                          label = T, abbr = T)
  return(as.character(wday))
}

#' Converts a datetime to a sim_datetime.
#'
#' @param datetime A datetime
#' @param origin_date A datetime
#'
#' @return sim_datetime A sim_datetime
#' @export
#'
#' @examples origin_date <- lubridate::ymd_hms("2016-01-01 00:00:00")
#' sim_datetime(lubridate::ymd_hms("2016-01-01 00:01:00"), origin_date)
#' # [1] 60
#' sim_datetime(lubridate::ymd_hms("2016-01-02 00:01:00"), origin_date)
#' # [1] 86460
sim_datetime <- function(datetime, origin_date) {
  seconds <- as.integer(as.numeric(datetime - origin_date, units = "secs"))
  # seconds <- sim_date(datetime)*24*60*60 + sim_time(datetime) # does not work any more. Change? SCN
  return(seconds)
}

#' Converts a sim_datetime to a datetime.
#'
#' @param sim_datetime A sim_datetime
#' @param origin_date A datetime
#'
#' @return datetime A POSIXct date-time
#' @export
#' @examples origin_date <- lubridate::ymd_hms("2016-01-01 00:00:00")
#' datetime(sim_datetime(lubridate::ymd_hms("2016-01-02 00:00:00"), origin_date), origin_date)
#' # [1] "2016-01-02 UTC"
datetime <- function(sim_datetime, origin_date) {
  datetime <- origin_date + sim_datetime
  return(datetime)
}

