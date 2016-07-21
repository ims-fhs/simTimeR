# .............................................................................. Check tz = "GMT" everywhere
#                                                                                mark benchmark tests as such...
# Nach jetzigem wissen gibt es folgende Reihenfolgen was Schnelligkeit anbelangt: ... tbd
# * %scheduled% "slow", for everything
# * %sin% handles all "t in schedule"
# * %fast_sin% only for "int_in...". Drawback: Needs int has to be converted to
#      date to  check whether update is necessary.
# * int_in_sint. Uses our weekday function, generic!
# * Further speed-up with extra columns in missions:
#    - t_alarm_year
#    - t_alarm_weekday
#    - t_alarm_simdate (0-365)
#    - t_alarm_simtime

# Discuss odl structre of schedule including weekdays: In case of schedule change ... ?

#' weekday is a customized lubridate/as.POSIXct function
#'
#' @param t
#' @param origin_date
#'
#' @return weekday as character. One of Sun < Mon < Tues < Wed < Thurs < Fri < Sat
#' @export
weekday <- function(t, origin_date = "2014-01-01 00:00:00", tz = "GMT") { # ....... sim_wday
  # dangerous with origin_date => wrong origin is taken?
  # In case of performance problems: Memoize...
  return(lubridate::wday(as.POSIXct(t, tz, origin_date), label = T, abbr = T))
} # ... Check all arguments

#' Title
#'
#' @param y
#'
#' @return
#' @export
first_day_of_year <- function(y) {
  return(lubridate::ymd_hms(paste0(y, "-01-01 00:00:00")))
}

#' Title
#'
#' @param date
#' @param origin_date
#'
#' @return
#' @export
date2simtime <- function(date, origin_date) { # date2sim_datetime
  return(as.numeric(date - origin_date, units = "secs"))
}

#' Title
#'
#' @param t
#' @param origin_date
#'
#' @return
#' @export
simtime2date <- function(t, origin_date) { # as.date or just t + origin?
  return(origin_date + t)
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
as.schedule <- function(object) {
  # Could be used to add class(schedule) to object
} # ........................................ Not used

#' Title
#'
#' @param object
#'
#' @return
#' @export
is.schedule <- function(object) {
  if (length(grepl("|", object)) == nrow(object) &
      length(grepl("--", object)) == nrow(object) &
      all(c("schedule", "update", "shift_from_simdate", "shift_to_simdate",
            "shift_from_simtime", "shift_to_simtime") %in% names(object)) &
      any(grepl(" ", object)) == F) {
    return(T)
  } else {
    return(F)
  }
}

#' gsub2 extends gsub to a pre-defined english-german dictionary
#'
#' @param translation , a dictionary
#' @param x, a character, to be translated
#' @param ...
#'
#' @return
gsub2 <- function(translation, x, ...) {
  for(i in names(translation)) {
    x <- gsub(i, translation[i], x, ...)
  }
  return(x)
} # ................................. imsbasics

#' Title
#'
#' @param schedule
#'
#' @return
#' @export
schedule_g2e <- function(schedule) {
  ints <- schedule::int_dict()
  dict <- schedule::dict()
  res <- gsub2(dict$g2e, paste(schedule, collapse = "%"))
  res <- unlist(strsplit(res, split = "%", fixed = T))

}

#' as.labor.list converts missions data.frame to labor-class
#'
#' @param missions
#' @param vehicle
#'
#' @return labor
#' @export
as.labor.list <- function(missions, vehicle) {
  missions <- missions[missions$vehicle_id == vehicle$id, ]
  labor <- list(
    vehicle_id = vehicle$id,
    schedule = vehicle$schedule,
    labor = lubridate::interval(origin_date + missions$t_alarm_sec,
                                origin_date + missions$t_alarm_sec + missions$dt_to_completion),
    productive = NA, # ......................................................... busy
    idle = NA,
    overtime = NA,
    stringsAsFactors = F)
  class(labor) <- c(class(labor), "labor")
  return(labor)
}


#' Title
#'
#' @param object
#'
#' @return
#' @export
is.labor.list <- function(object) {
  if (all(c("vehicle_id", "schedule", "labor", "productive", "idle", "overtime") %in%
          names(object)) &
      "labor" %in% class(object)) {
    return(T)
  } else {
    return(F)
  }
}

#' Title
#'
#' @param y
#' @param date_df
#' @param type
#'
#' @return
calculate_shift_simdate <- function(y, date_df, type=stop("Argument from or to needed")) {
  # date_df = data.frame(X1 = c("Jan-01", "Dec-31"))
  ints <- int_dict()
  if (type == "from") {
    i <- 1
    correct <- 1
  } else if (type == "to") {
    i <- 2
    correct <- 0
  } else {
    stop("Argument from or to needed")
  }
  dates_abbr_list <- strsplit(as.character(date_df[i, ]), split = "-", fixed = T)
  dates_abbr <- as.data.frame(dates_abbr_list, col.names = c(1:length(dates_abbr_list)),
    stringsAsFactors = F)
  month <- as.character(ints$e2int[as.character(dates_abbr[1, ])])
  day <- as.character(dates_abbr[2, ])
  return(lubridate::yday(paste(y, month, day, sep = "-")) - correct)
}

#' Title
#'
#' @param y
#' @param time_df
#' @param type
#'
#' @return
calculate_shift_simtime <- function(y, time_df, type=stop("Argument from or to needed")) {
  # time_df = data.frame(X1 = c("00:00", "24:00"))
  ints <- int_dict()
  if (type == "from") {
    i <- 1
  } else if (type == "to") {
    i <- 2
  } else {
    stop("Argument from or to needed")
  }
  time_abbr <- as.data.frame(strsplit(as.character(time_df[i, ]), split = "-", fixed = T),
    stringsAsFactors = F)
  time <- as.data.frame(strsplit(as.character(time_abbr), split = ":", fixed = T),
    stringsAsFactors = F)
  return(60 * (60 * as.integer(time[1, ]) + as.integer(time[2, ])))
}

#' yearly_intervals splits lubridate interval which does not end in the same year
#' into an array of intervals, each containing a lubridate interval which ends in
#' the same year.
#'
#' @param interval
#'
#' @return array of intervals
yearly_intervals <- function(interval) {
  start_date <- lubridate::int_start(interval)
  end_date <- lubridate::int_end(interval)
  my_years <- c(lubridate::year(start_date):lubridate::year(end_date))
  my_array <- my_years[1]
  for (i in 2:(length(my_years))) {
    my_array <- c(my_array, rep(my_years[i], 2))
  }
  my_array <- c(my_array, my_years[length(my_years)])
  interval_on_year_base <- first_day_of_year(my_array)
  interval_on_year_base[1] <- start_date
  interval_on_year_base[length(interval_on_year_base)] <- end_date
  res <- lubridate::interval(interval_on_year_base[1],interval_on_year_base[2])
  for (i in 1:(length(my_array)/2-1)) {
    res <- c(res, lubridate::interval(interval_on_year_base[2*i+1],interval_on_year_base[2*i+2]))
  }
  if (lubridate::int_start(res[length(res)]) == lubridate::int_end(res[length(res)])) {
    res <- res[-length(res)]
  }
  return(res)
}

#' update_schedule updates the schedule-part of any object containing
#' update, shift_from_simdate shift_to_simdate, shift_from_simtime and shift_to_simtime
#' IF ist update flag is not up to date
#'
#' @param date
#' @param schedule
#'
#' @return schedule
#' @export
update_schedule <- function(date, schedule) {
  y <- lubridate::year(date)
  if (schedule$update[1] != y) {
    assertthat::assert_that(all(c("update", "shift_from_simdate", "shift_to_simdate",
                                  "shift_from_simtime", "shift_to_simtime",
                                  "shift_weekday", "schedule") %in% names(schedule)))
    assertthat::assert_that(assertthat::noNA(schedule$id))

    schedule$update <- rep(y, nrow(schedule))
    message(paste0("update_schedule: ", date, ", new year ", schedule$update[1]))
    df <- as.data.frame(strsplit(schedule$schedule, split = "|", fixed = T),
                        stringsAsFactors = F)

    # Update dates
    date_list <- strsplit(as.character(df[1, ]), split = "--", fixed = T)
    date_df <- as.data.frame(date_list, col.names = c(1:length(date_list)),
                             stringsAsFactors = F)

    schedule$shift_from_simdate <- calculate_shift_simdate(y, date_df, "from")
    schedule$shift_to_simdate <- calculate_shift_simdate(y, date_df, "to")

    # Update time
    time_df <- as.data.frame(strsplit(as.character(df[2, ]), split = "--", fixed = T),
                             stringsAsFactors = F)
    schedule$shift_from_simtime <- calculate_shift_simtime(y, time_df, "from")
    schedule$shift_to_simtime <- calculate_shift_simtime(y, time_df, "to")

    assign("schedule", schedule, envir = .GlobalEnv)
  } else {
    # Do nothing as schedule is already up to date.
  }
  return(schedule)
}

#' calculate_daily_times calculates overtime, idle time... on a daily (0h-24h) basis.
#' Schedule needs not to be updated. Called in labor_in_year_schar
#'
#' @param labor
#' @param schedule
#' @param idle2overtime = F, is idle time outside labortime considered to idle or
#' overtime? Depends on application
#'
#' @return labor
calculate_daily_times <- function(labor, planned_times, idle2overtime = F) {
  library(lubridate) # needed for %within%
  # Check, that only one day is considered
  cond_same_day <- lubridate::wday(lubridate::int_start(planned_times)) ==
    lubridate::wday(lubridate::int_start(labor$labor))
  remaining <- labor$labor[cond_same_day]
  cond_productive <- remaining %within% planned_times
  cond_overlap <- lubridate::int_overlaps(remaining, planned_times)
  cond_split <- xor(cond_productive, cond_overlap) # ........................... comment needed

  # Separate initial idle-possibility
  start_date_plan <- lubridate::int_start(planned_times)
  start_date_labor <- lubridate::int_start(remaining[1])
  # Initialization needed to keep lubridate-class
  productive <- lubridate::interval(start_date_plan, start_date_plan)
  idle <- lubridate::interval(start_date_plan, start_date_plan)
  overtime <- lubridate::interval(start_date_plan, start_date_plan)
  if (start_date_labor < start_date_plan) {
    stop("start_date_labor < start_date_plan not possible")
  } else if (start_date_labor == start_date_plan) {
    # Do nothing, idle time 0
  } else {
    idle <- c(idle, lubridate::interval(start_date_plan, start_date_labor))
  }

  # Handle split in case labor is crossing end of planned_time (*)
  if (sum(cond_split) == 1) {
    first_part <- remaining[1:(which(cond_split)-1)]
    third_part <- remaining[(which(cond_split)+1):length(cond_split)]
    split_interval <- remaining[cond_split]
    start <- lubridate::int_start(split_interval)
    end <- lubridate::int_end(split_interval)
    intermediate <- lubridate::int_end(planned_times)
    remaining <- c(first_part,
      lubridate::interval(start, intermediate),
      lubridate::interval(intermediate, end), third_part)
  } else if (sum(cond_split) > 1) {
    stop("Can't be...")
  } # else: Do nothing

  start_ref <- lubridate::int_start(planned_times)
  for (k in 1:length(remaining)) {
    # browser()
    my_interval <- remaining[k]
    my_start <- lubridate::int_start(my_interval)
    my_stop <- lubridate::int_end(my_interval)
    if (idle2overtime) {
      overtime <- c(overtime, lubridate::interval(start_ref, my_start))
    } else {
      idle <- c(idle, lubridate::interval(start_ref, my_start))
    }
    if (my_stop <= lubridate::int_end(planned_times)) {
      productive <- c(productive, my_interval)
    } else {
      # According to (*) the whole interval is in or out
      overtime <- c(overtime, my_interval)
    }
    start_ref <- my_stop
  }

  labor$idle <- unique(idle[!as.numeric(as.duration(idle), units = "secs") == 0])
  labor$overtime <- unique(overtime[!as.numeric(as.duration(overtime), units = "secs") == 0])
  labor$productive <- unique(productive[!as.numeric(as.duration(productive), units = "secs") == 0])
  return(labor)
}

#' int_in_sint is the fast version of is_on_duty but
#' * using weekday calculated from calendar
#' * returns T/F array in order to subset whatever necessary,
#'    e.g. vehicles[int_in_sint(t, vehicles), ]
#'
#' @param t
#' @param vehicles
#'
#' @return T/F array
int_in_sint <- function(t, vehicles) {
  return(vehicles$shift_from_simdate <= simTimeR::simDate(t) &
      vehicles$shift_to_simdate >= simTimeR::simDate(t) &
      vehicles$shift_from_simtime <= simTimeR::simTime(t) &
      vehicles$shift_to_simtime > simTimeR::simTime(t) &
      grepl(weekday(t), vehicles$shift_weekday))
} # ................................... rule, vectorized!

#' Title
#'
#' @param date
#' @param vehicles
#'
#' @return
posix_in_sint <- function(date, vehicles) {
  t <- date2simtime(date, origin_date)
  return(int_in_sint(t, vehicles))
}

#' %fast_sin% fast wrapper for ...................................... How to handle tz and origin_date
#' * int_in_sint and
#' * "int_in_schar" which does not exist as extra function (only update_schedule necessary)
#'
#' @param t
#' @param vehicles
#'
#' @return
#' @export
`%fast_sin%` <- function(t, vehicles) {
  mydate <- simtime2date(t, origin_date)
  if (lubridate::year(mydate) != vehicles$update[1]) {
    vehicles <- update_schedule(mydate, vehicles)
  }
  res <- int_in_sint(t, vehicles)
  return(res)
}

#' %sin% checks whether t is in schedule for the following possible formats:
#' * int, sint => fast_sin
#' * int, schar => fast_sin
#' * posix, sint
#' * posix, schar
#'
#' @param t
#' @param vehicles
#'
#' @return
#' @export
#'
#' @examples
#' @export
`%sin%` <- function(t, vehicles) {
  if (class(t)[1] == "integer" | class(t)[1] == "numeric") {
    # Use %fast_sin% to check whether int is "in" schedule
    res <- t %fast_sin% vehicles
  } else {
    if (lubridate::year(t) == vehicles$update[1]) {
      res <- posix_in_sint(t, vehicles)
    } else {
      # Corresponds to posix_in_schar
      vehicles <- update_schedule(t, vehicles)
      res <- posix_in_sint(t, vehicles)
    }
  }
  return(res)
}

#' sint_in_year_interval Calculates planned labor in interval on a yearly basis
#'
#' @param schedule
#' @param interval
#'
#' @return
sint_in_year_interval <- function(schedule, interval) { # ...................... planned_labor_time can be any range - exeeding a year
  library(lubridate) # needed for %within%
  assertthat::assert_that(nrow(schedule) == 1)
  # Array containing all days in interval:
  start_date <- lubridate::floor_date(lubridate::int_start(interval), "day")
  end_date <- lubridate::floor_date(lubridate::int_end(interval), "day")
  assertthat::assert_that(lubridate::year(start_date) == lubridate::year(end_date - 0.001))
  all_days <- seq(from = start_date, to = end_date, by = "days")

  # Filter days according to weekday, shift_from_simdate and shift_to_simdate ..... filter_labordays
  c1 <- lubridate::wday(all_days, label = T, abbr = T) %in% unlist(strsplit(schedule$shift_weekday, split = ","))
  c2 <- lubridate::yday(all_days) >= schedule$shift_from_simdate # ................ simdate
  c3 <- lubridate::yday(all_days) <= schedule$shift_to_simdate # ................... simdate
  real_labor_days <- all_days[c1 & c2 & c3]

  # Set up array of time intervals including shift_from_simtime and shift_to_simtime
  res <- lubridate::interval(real_labor_days + schedule$shift_from_simtime,
    real_labor_days + schedule$shift_to_simtime)

  # If intersection is empty delete unused intervals
  overlaps <- lubridate::int_overlaps(res, interval)
  if (any(!overlaps)) {
    res <- res[-which(!overlaps)]
  }

  # Get rid of wrong or too long intervals
  not_within_interval <- !res %within% interval
  if (any(not_within_interval)) {
    throw_out <- as.numeric(lubridate::duration(lubridate::intersect(
      res, interval))) == 0 & not_within_interval
    restrict_to_overlap <- as.numeric(lubridate::duration(lubridate::intersect(
      res, interval))) > 0 & not_within_interval
    if (any(throw_out)) {
      # Delete outliers
      res <- res[-which(throw_out)]
    } else if (any(restrict_to_overlap)) {
      # only take overlapping part of both intervals
      res[which(restrict_to_overlap)] <- intersect(res[restrict_to_overlap], interval)
    } else {
      stop("Unknown case")
    }
  }
  return(res)
}

#' sint_in_interval obsolete. Throws error
#'
#' @param schedule
#' @param interval
#'
#' @return error
sint_in_interval <- function(schedule, interval) { # .......................................... obsolet
  stop("In case of change in year, schedule needs to update. Use schar_in_interval")
}

#' schar_in_interval calculates the planned labor time of a schedule in the interval.
#' Used to calculate planned labor time and as basis for calculation of overtime, ...
#'
#' @param schedule
#' @param interval
#'
#' @return intervals
schar_in_interval <- function(schedule, interval) { # ...................................... obsolet
  start_date <- lubridate::floor_date(lubridate::int_start(interval), "day")
  end_date <- lubridate::floor_date(lubridate::int_end(interval), "day")
  if (lubridate::year(start_date) == lubridate::year(end_date - 0.001)) {
    # Interval is fully contained in onbe calendar year
    schedule <- update_schedule(lubridate::int_start(interval), schedule)
    res <- sint_in_year_interval(schedule, interval)
  } else {
    my_intervals <- yearly_intervals(interval)
    schedule <- update_schedule(lubridate::int_start(my_intervals[1]), schedule)
    # First interval also used as initialization
    res <- sint_in_year_interval(schedule, my_intervals[1])
    for (i in 2:(length(my_intervals))) {
      start <- lubridate::int_start(my_intervals[i])
      schedule <- update_schedule(start, schedule)
      res <- c(res, sint_in_year_interval(schedule, my_intervals[i]))
    }
  }
  return(res)
}

#' labor_in_year_schar calculates overtime, idle time... on a yearly basis.
#' Schedule needs not to be updated. Called in labor_in_schar
#'
#' @param labor
#' @param schedule
#'
#' @return labor
labor_in_year_schar <- function(labor, schedule) { # .......................... obsolet
  assertthat::assert_that(all(class(labor) == c("list", "labor")))
  assertthat::assert_that(labor$vehicle_id == schedule$id)
  # Ensure that you start with a clean labor (Do not mix years):
  assertthat::assert_that(as.numeric(as.duration(labor$idle), units = "secs") == 0 |
      is.na(labor$idle))
  assertthat::assert_that(as.numeric(as.duration(labor$overtime), units = "secs") == 0 |
      is.na(labor$overtime))

  start_date <- min(lubridate::int_start(labor$labor))
  end_date <- max(lubridate::int_end(labor$labor))
  assertthat::assert_that(lubridate::year(start_date) == lubridate::year(end_date - 0.001))
  schedule <- update_schedule(start_date, schedule)

  year_range <- lubridate::interval(lubridate::floor_date(start_date, "day"),
    lubridate::ceiling_date(end_date, "day"))
  planned_times <- schar_in_interval(schedule, year_range)
  if (length(planned_times) == 0) {
    stop("labor and schedule do not overlap")
  }
  start_date_plan <- lubridate::int_start(planned_times)
  # Initialize lubridate intervals:
  productive <- lubridate::interval(start_date_plan, start_date_plan)
  idle <- lubridate::interval(start_date_plan, start_date_plan)
  overtime <- lubridate::interval(start_date_plan, start_date_plan)
  for (i in 1:length(planned_times)) { # ....................................... loop over wday?
    # Compare workload on a daily basis: labor = productive + idle + overtime
    r <- calculate_daily_times(labor, planned_times[i], idle2overtime = F) # idle2overtime wichtig!
    idle <- c(idle, r$idle); productive <- c(productive, r$productive); overtime <- c(overtime, r$overtime); rm(r)
  }
  # Get rid of initialization. Ensure intervals to be unique
  labor$idle <- unique(idle[!as.numeric(as.duration(idle), units = "secs") == 0])
  labor$overtime <- unique(overtime[!as.numeric(as.duration(overtime), units = "secs") == 0])
  labor$productive <- unique(productive[!as.numeric(as.duration(productive), units = "secs") == 0])
  return(labor)
}

#' labor_in_schar calculates overtime, idletime, ...
#'
#' @param labor
#' @param schedule
#'
#' @return labor
labor_in_schar <- function(labor, schedule) { # Verwende Code aus _year_
  library(lubridate) # needed for %within%
  start_date <- min(lubridate::int_start(labor$labor))
  end_date <- max(lubridate::int_end(labor$labor))
  if (lubridate::year(start_date) == lubridate::year(end_date - 0.001)) {
    # labor contained in one calendar year, that is the easy case:
    labor <- labor_in_year_schar(labor, schedule)
  } else {
    # labor not contained in one calendar year, calculate for each year separately:
    total_span <- lubridate::interval(lubridate::floor_date(start_date, "day"),
      lubridate::ceiling_date(end_date, "day"))
    my_intervals <- yearly_intervals(total_span)
    for (i in 1:(length(my_intervals))) {
      schedule <- update_schedule(lubridate::int_start(my_intervals[i]), schedule)
      my_labor <- labor
      my_labor$labor <- labor$labor[labor$labor %within% my_intervals[i]]
      # Initialize lubridate intervals:
      my_labor$idle <- lubridate::interval("1900-01-01 00:00:00", "1900-01-01 00:00:00")
      my_labor$overtime <- lubridate::interval("1900-01-01 00:00:00", "1900-01-01 00:00:00")
      my_labor$productive <- lubridate::interval("1900-01-01 00:00:00", "1900-01-01 00:00:00")
      my_labor <- labor_in_year_schar(my_labor, schedule)
      if (!all(is.na(labor$idle))) {
        labor$idle <- c(labor$idle, my_labor$idle)
      } else {
        labor$idle <- my_labor$idle
      }
      if (!all(is.na(labor$overtime))) {
        labor$overtime <- c(labor$overtime, my_labor$overtime)
      } else {
        labor$overtime <- my_labor$overtime
      }
      if (!all(is.na(labor$productive))) {
        labor$productive <- c(labor$productive, my_labor$productive)
      } else {
        labor$productive <- my_labor$productive
      }
      rm(my_labor)
    }
    # Get rid of initialization. Ensure intervals to be unique
    labor$idle <- unique(labor$idle[!as.numeric(
      as.duration(labor$idle), units = "secs") == 0])
    labor$overtime <- unique(labor$overtime[!as.numeric(
      as.duration(labor$overtime), units = "secs") == 0])
    labor$productive <- unique(labor$productive[!as.numeric(
      as.duration(labor$productive), units = "secs") == 0])
  }
  return(labor)
}

#' %scheduled% is a wrapper for whole schedule functionality, covering the
#' 3 cases
#' * Is a date "in" the schedule = Is somebody avaailable at t
#' * Is a schedule "in" an timedate interval = What are the "available times" of a schedule
#' * Are labor times "in" schedule = Calculate overtime, idletime, ...
#'
#' @param lhs
#' @param rhs
#'
#' @return According to the above cases: T/F, lubridate::intervals or labor
#' @export
`%scheduled%` <- function(lhs, rhs) {
  c_lhs <- as.character(class(lhs))
  c_rhs <- as.character(class(rhs))
  if ("POSIXct" %in% c_lhs & "schedule" %in% c_rhs) {
    # Datetime in schedule. Return T/F
    return(lhs %sin% rhs)
  } else if (all(c_lhs == "numeric") & "schedule" %in% c_rhs) {
    # integer time (seconds) in schedule. Return T/F
    return(lhs %sin% rhs)
  } else if ("schedule" %in% c_lhs & all(c_rhs == "Interval")) {
    # schedule in interval. Return interval
    return(schar_in_interval(lhs, rhs))
  } else if ("labor" %in% c_lhs & "schedule" %in% c_rhs) {
    # labor in schedule. Return labor
    return(labor_in_schar(lhs, rhs))
  } else {
    stop("Wrong classes of lhs or rhs.")
  }
}

