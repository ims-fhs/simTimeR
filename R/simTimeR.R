#' Converts a DateTime to a simTime. truncates the Date part. range 0-(24*60*60-1)
#'
#' @param datetime
#'
#' @return t A simDateTime
as.simTime <- function(datetime) {
  datetime <- as.character(datetime)
  60*60*as.integer(strftime(datetime, "%H")) +
    60*as.integer(strftime(datetime, "%M")) +
    as.integer(strftime(datetime, "%S"))
}


#' Converts a DateTime to a simDate. truncates the Time part. range 0-364
#'
#' @param datetime
#'
#' @return t A simDate
as.simDate <- function(datetime) {
  datetime <- as.character(datetime)
  as.integer(strftime(datetime, "%j")) - 1
}


#' Converts a DateTime to a simDateTime
#'
#' @param datetime
#'
#' @return t A simDateTime
as.simDateTime <- function(datetime) {
  as.simDate(datetime)*24*60*60 + as.simTime(datetime)
}

#' as.charTime(): Converts a simTime back to a char containing the time in the
#' format HH:MM
#'
#' @param simTime
#'
#' @return charTime A char containing a time in the format HH:MM
as.charTime <- function(simTime) {
  h <- trunc(simTime/60/60)
  if(h < 10) {h <- as.character(paste0(0,h))}
  m <- simTime %% (60*60)/60
  if(m < 10) {m <- as.character(paste0(0,m))}
  paste0(h,":",m)
}

#' Date part of a simDateTime
#'
#' @param t A simDateTime
#'
#' @return the day of year (0-364)
simDate <- function(t) {
  trunc(t/(24*60*60))
}

#' Time part of a simDateTime
#'
#' @param t A simDateTime
#'
#' @return the actual time in seconds (0-(24*60*60-1))
simTime <- function(t) {
  t %% (24*60*60)
}


#' Weekday of a simDateTime
#'
#' @param t A simDateTime
#'
#' @return the weekday in the format: "Mo, Di, Mi, Do, Fr, Sa, So"
simWeekday <- function(t) {
  rep(c("Mi", "Do", "Fr", "Sa", "So", "Mo", "Di"), length = 365)[simDate(t) + 1]
}
