# =============================================================================
# Independent functions:

#' Converts a DateTime to a simTime. Only time part of datetime. range 0-(24*60*60-1).
#' @param datetime
#'
#' @return t A simDateTime
as.simTime <- function(datetime) {
  datetime <- as.character(datetime)
  seconds <- 60*60*as.integer(strftime(datetime, "%H")) +
    60*as.integer(strftime(datetime, "%M")) +
    as.integer(strftime(datetime, "%S"))
  return(seconds)
}


#' Converts a DateTime to a simDate. Only date part of datetime. range 0-364.
#'
#' @param datetime
#'
#' @return t A simDate
as.simDate <- function(datetime) {
  # warning("Implementation via POSIXct possible. See testthat")
  datetime <- as.character(datetime)
  days <- as.integer(strftime(datetime, "%j")) - 1 # Day of year. 1-365.
  return(days)
}


#' as.charTime(): Converts a simTime back to a char containing the time in the
#' format HH:MM
#'
#' @param simTime
#'
#' @return charTime A char containing a time in the format HH:MM
as.charTime <- function(simTime) {
  # warning("Implementation via POSIXct possible. See testthat")
  h <- trunc(simTime/60/60)
  if(h < 10) {h <- as.character(paste0(0,h))}
  m <- simTime %% (60*60)/60
  if(m < 10) {m <- as.character(paste0(0,m))}
  return(paste0(h,":",m))
}


#' Return day of year from rel. time in sconds.
#'
#' @param t A simDateTime
#'
#' @return the day of year (0-364)
simDate <- function(t) {
  # warning("Implementation via POSIXct possible. See testthat. ToDo vals 0, 365?")
  return(trunc(t/(24*60*60)))
}


#' Time part of a simDateTime
#'
#' @param t A simDateTime
#'
#' @return the actual time in seconds (0-(24*60*60-1))
simTime <- function(t) {
  return(t %% (24*60*60)) # ..................... t_h <- strptime(t, "%Y-%m-%d") => t-t_h
}


#' Weekday of a simDateTime
#'
#' @param t A simDateTime
#'
#' @return the weekday in the format: "Mo, Di, Mi, Do, Fr, Sa, So"
simWeekday <- function(t, order = c("Mi", "Do", "Fr", "Sa", "So", "Mo", "Di")) {
  # rep(order, length = 365)[simDate(t) + 1]
  # Test alternative:
  if (exists("n_sim_weekday", env = .GlobalEnv, inherits = F)) {
    print(n_sim_weekday)
    assign("n_sim_weekday", n_sim_weekday + 1, envir = .GlobalEnv)
  }
  weekdays(as.POSIXct(t, tz = "GMT", origin = "2014-01-01 00:00:00"), abbreviate = T) # german "Do" "Fr" "Sa" "So" "Mo" "Di" "Mi"
  # lubridate::wday(as.POSIXct(t_int[1], tz = "GMT", origin = "1970-01-01"), label = T, abbr = T) # english Thurs Fri   Sat   Sun   Mon   Tues  Wed
}


# =============================================================================
# Functions with dependencies:

#' Converts a DateTime to a simDateTime. Date + Time in seconds. Range 0- (365*24*60*60-1)
#'
#' @param datetime
#'
#' @return t A simDateTime
as.simDateTime <- function(datetime) {
  # warning("Implementation via POSIXct possible. See testthat")
  seconds <- as.simDate(datetime)*24*60*60 + as.simTime(datetime)
  return(seconds)
}


# =============================================================================
# Not used any more:

