# Old simtimer functions without changes for backwards compatibility (except
# package name) - DO NOT MAKE CHANGES HERE!!!

#' Converts a DateTime to a simTime. Only time part of datetime. range 0-(24*60*60-1).
#' @param datetime
#'
#' @return t A simDateTime
#' @export
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
#' @export
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
#' @export
as.charTime <- function(simTime) { # ........................................... SQC zero_n
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
#' @export
simDate <- function(t) {
  # warning("Implementation via POSIXct possible. See testthat. ToDo vals 0, 365?")
  return(trunc(t/(24*60*60)))
}


#' Time part of a simDateTime
#'
#' @param t A simDateTime
#'
#' @return the actual time in seconds (0-(24*60*60-1))
#' @export
simTime <- function(t) {
  return(t %% (24*60*60)) # ..................... t_h <- strptime(t, "%Y-%m-%d") => t-t_h
}


#' Weekday of a simDateTime
#'
#' @param t A simDateTime
#'
#' @return the weekday in the format: "Mo, Di, Mi, Do, Fr, Sa, So"
#' @export
simWeekday <- function(t, order = c("Mi", "Do", "Fr", "Sa", "So", "Mo", "Di")) {
  return(rep(order, length = 365)[simDate(t) + 1])
}

#' Converts a DateTime to a simDateTime. Date + Time in seconds. Range 0- (365*24*60*60-1)
#'
#' @param datetime
#'
#' @return t A simDateTime
#' @export
as.simDateTime <- function(datetime) {
  # warning("Implementation via POSIXct possible. See testthat")
  seconds <- as.simDate(datetime)*24*60*60 + as.simTime(datetime)
  return(seconds)
}

#' is_on_duty from sim911 package finds all vehicles that are on duty
#'
#' @param vehicles A set of class sim_vehicle
#' @param t A numeric, relative time in seconds
#' @import data.table
#'
#' @return vehicles class sim_vehicles, a subset of sim_vehicles
#' @export
is_on_duty <- function(t, vehicles) { # ........................................ rule?
  return(vehicles$shift_from_simdate <= simTimeR::simDate(t) &
      vehicles$shift_to_simdate >= simTimeR::simDate(t) &
      vehicles$shift_from_simtime <= simTimeR::simTime(t) &
      vehicles$shift_to_simtime > simTimeR::simTime(t) &
      grepl(simTimeR::simWeekday(t), vehicles$shift_weekday))
}
