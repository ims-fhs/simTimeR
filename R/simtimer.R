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
sim_date <- function(sim_datetime) {
  return(trunc(sim_datetime/(24*60*60)))
}

#' Day-time part of a sim_datetime
#'
#' @param sim_datetime A sim_datetime
#'
#' @return the actual day-time in seconds (0-(24*60*60-1))
#' @export
sim_time <- function(sim_datetime) {
  return(sim_datetime %% (24*60*60))
}

#' Weekday of a sim_datetime
#'
#' @param sim_datetime A sim_datetime
#'
#' @return the weekday in the format: "Mo, Di, Mi, Do, Fr, Sa, So"
#' @export
sim_wday <- function(sim_datetime) {
  # tz = "GMT" not considered at the moment. In case of performance problems: Memoize.
  wday <- lubridate::wday(as.POSIXct(sim_datetime, origin = origin_date),
                          label = T, abbr = T)
  return(as.character(wday))
}

#' Converts a datetime to a sim_datetime.
#'
#' @param datetime A datetime
#'
#' @return sim_datetime A sim_datetime
#' @export
sim_datetime <- function(datetime) {
  seconds <- as.integer(as.numeric(datetime - origin_date, units="secs"))
  # seconds <- sim_date(datetime)*24*60*60 + sim_time(datetime) # does not work any more. Change? SCN
  return(seconds)
}

#' Converts a sim_datetime to a datetime.
#'
#' @param sim_datetime
#'
#' @return datetime A POSIXct date-time
#' @export
datetime <- function(sim_datetime) {
  datetime <- origin_date + sim_datetime
  return(datetime)
}

