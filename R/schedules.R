# .............................................................................. Check tz = "GMT" everywhere
# From /Erste Testprogramme/51e schedule
# data structure:
# as.labor <- function(missions) {
#   n <- nrow(missions)
#   df <- data.frame(
#     t = missions$t_alarm_sec,
#     dt = missions$dt_to_completion,
#     idle = rep(0, n),
#     overtime = rep(0,n),
#     stringsAsFactors = F)
#   return(df)
# }

# is.labor <- function(labor) {
#   ifelse(all(c("t", "dt", "idle", "overtime") %in% names(labor)), return(T), return(F))
# } # ............................................ Needed?

as.labor.list <- function(missions, vehicle) {
  missions <- missions[missions$vehicle_id == vehicle$id, ]
  labor <- list(
    vehicle_id = vehicle$id,
    schedule = vehicle$schedule,
    labor = lubridate::interval(origin_date + missions$t_alarm_sec,
      origin_date + missions$t_alarm_sec + missions$dt_to_completion),
    productive = NA,
    idle = NA,
    overtime = NA,
    stringsAsFactors = F)
  class(labor) <- c(class(labor), "labor")
  return(labor)
}

# Basics functions
weekday <- function(t, origin_date = "2014-01-01 00:00:00") {
  return(lubridate::wday(as.POSIXct(t, tz = "GMT", origin_date), label = T, abbr = T))
}

is_free <- function(object, t) {
  return(vehicles[object$busy_until <= t,])
}

calculate_shift_simdate <- function(y, date_df, type=stop("Argument from or to needed")) {
  ints <- int_dict()
  if (type == "from") {
    i <- 1
  } else if (type == "to") {
    i <- 2
  } else {
    stop("Argument from or to needed")
  }
  dates_abbr <- as.data.frame(strsplit(as.character(date_df[i, ]), split = "-", fixed = T),
    stringsAsFactors = F) # col.names = c("a", "b", "c") = 3 vehicles
  dates <- as.character(ints$e2int[as.character(dates_abbr[1, ])])
  return(lubridate::yday(paste(y, dates, dates_abbr[2, ], sep = "-")))
}

calculate_shift_simtime <- function(y, time_df, type=stop("Argument from or to needed")) {
  ints <- int_dict()
  if (type == "from") {
    i <- 1
  } else if (type == "to") {
    i <- 2
  } else {
    stop("Argument from or to needed")
  }
  time_abbr <- as.data.frame(strsplit(as.character(time_df[i, ]), split = "-", fixed = T),
    stringsAsFactors = F) # col.names = c("a", "b", "c") = 3 vehicles
  time <- as.data.frame(strsplit(as.character(time_abbr), split = ":", fixed = T),
    stringsAsFactors = F)  # col.names = c("a", "b", "c") = 3 vehicles
  return(60 * (60 * as.integer(time[1, ]) + as.integer(time[2, ])))
}

first_day_of_year <- function(y) {
  return(lubridate::ymd_hms(paste0(y, "-01-01 00:00:00")))
}

yearly_intervals <- function(interval) {
  start_date <- lubridate::int_start(interval)
  end_date <- lubridate::int_end(interval)
  my_years <- c(lubridate::year(start_date):lubridate::year(end_date))
  my_array <- my_years[1]
  for (i in 2:(length(my_years))) {
    my_array <- c(my_array, rep(my_years[i], 2))
  }
  my_array <- c(my_array, my_years[length(my_years)])
  # browser()
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

update_schedule <- function(date, schedule) {
  assertthat::assert_that(all(c("update", "shift_from_simdate", "shift_to_simdate",
    "shift_from_simtime", "shift_to_simtime", "shift_weekday", "schedule") %in% names(schedule)))
  # tz <- lubridate::tz(date)
  y <- lubridate::year(date)
  schedule$update <- rep(y, nrow(schedule))
  # browser()
  df <- as.data.frame(strsplit(schedule$schedule, split = "|", fixed = T),
    stringsAsFactors = F) # col.names = c("a", "b", "c") = 3 vehicles

  # Update dates
  date_df <- as.data.frame(strsplit(as.character(df[1, ]), split = "--", fixed = T),
    stringsAsFactors = F) # col.names = c("a", "b", "c") = 3 vehicles
  schedule$shift_from_simdate <- calculate_shift_simdate(y, date_df, "from")
  schedule$shift_to_simdate <- calculate_shift_simdate(y, date_df, "to")

  # Update time
  time_df <- as.data.frame(strsplit(as.character(df[2, ]), split = "--", fixed = T),
    stringsAsFactors = F) # col.names = c("a", "b", "c") = 3 vehicles
  schedule$shift_from_simtime <- calculate_shift_simtime(y, time_df, "from")
  schedule$shift_to_simtime <- calculate_shift_simtime(y, time_df, "to")

  # Update shift_weekdays
  schedule$shift_weekday <- as.character(df[3, ])

  assign("schedule", schedule, envir = .GlobalEnv)
  return(schedule)
}

calculate_daily_times <- function(labor, planned_times, idle2overtime = F) {
  library(lubridate)
  # Check, that only one day is considered
  cond_same_day <- lubridate::wday(lubridate::int_start(planned_times)) == lubridate::wday(lubridate::int_start(labor$labor))
  remaining <- labor$labor[cond_same_day]
  # planned_times %within% remaining # [1] FALSE FALSE FALSE FALSE FALSE not useful
  cond_productive <- remaining %within% planned_times
  # [1]  TRUE  TRUE FALSE FALSE ... productive
  cond_overlap <- lubridate::int_overlaps(remaining, planned_times)
  # [1]  TRUE  TRUE  TRUE FALSE ... Change from F to T !!!
  cond_split <- xor(cond_productive, cond_overlap)
  # [1] FALSE FALSE  TRUE FALSE

  # Separate initial idle-possibility
  start_date_plan <- lubridate::int_start(planned_times)
  start_date_labor <- lubridate::int_start(remaining[1])
  productive <- lubridate::interval(start_date_plan, start_date_plan)
  idle <- lubridate::interval(start_date_plan, start_date_plan)
  overtime <- lubridate::interval(start_date_plan, start_date_plan)
  if (start_date_labor < start_date_plan) {
    stop("start_date_labor < start_date_plan not possible")
  } else if (start_date_labor == start_date_plan) {
    # Do nothing idle time 0
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

# Topic time or "date %in% schedule"
int_in_sint <- function(t, vehicles) {
  return(vehicles$shift_from_simdate <= simTimeR::simDate(t) &
      vehicles$shift_to_simdate >= simTimeR::simDate(t) &
      vehicles$shift_from_simtime <= simTimeR::simTime(t) &
      vehicles$shift_to_simtime > simTimeR::simTime(t) &
      grepl(weekday(t), vehicles$shift_weekday))
} # ................................... rule, vectorized!

int_in_schar <- function(t, vehicles) {
  mydate<- simtime2date(t, origin_date)
  vehicles <- update_schedule(mydate, vehicles)
  return(int_in_sint(t, vehicles))
}

posix_in_sint <- function(date, vehicles) {
  t <- date2simtime(date, origin_date)
  return(int_in_sint(t, vehicles))
}

`%fast_sin%` <- function(t, vehicles) {
  mydate <- simtime2date(t, origin_date)
  if (lubridate::year(mydate) != vehicles$update[1]) { # & vehicles$update[1] != 0?
    vehicles <- update_schedule(mydate, vehicles)
  }
  res <- int_in_sint(t, vehicles)
  return(res)
} # fast wrapper for int_in_ functions

`%sin%` <- function(t, vehicles) {
  if (class(t)[1] == "integer" | class(t)[1] == "numeric") {
    res <- t %fast_sin% vehicles
  } else {
    # if (lubridate::year(t) == vehicles$update[1]) {
#       if (vehicles$update[1] != 0) {
#         res <- posix_in_sint(t, vehicles)
#       } else {
#         # Corresponds to posix_in_schar
#         vehicles <- update_schedule(t, vehicles)
#         res <- int_in_sint(t, vehicles)
#       }
#     }
    if (lubridate::year(t) == vehicles$update[1]) {
      res <- posix_in_sint(t, vehicles)
    } else {
      # Corresponds to posix_in_schar
      vehicles <- update_schedule(t, vehicles)
      res <- posix_in_sint(t, vehicles)
    }
  }
  return(res)
} # wrapper for above 4 functions

# Topic "schedule %in% interval" => Calculate planned labor in interval
sint_in_year_interval <- function(schedule, interval) {
  library(lubridate)
  assertthat::assert_that(nrow(schedule) == 1)
  # Array containing all days in interval:
  start_date <- lubridate::floor_date(lubridate::int_start(interval), "day")
  end_date <- lubridate::floor_date(lubridate::int_end(interval), "day")
  assertthat::assert_that(lubridate::year(start_date) == lubridate::year(end_date - 0.001))
  all_days <- seq(from = start_date, to = end_date, by = "days")

  # Filter days according to weekday, shift_from_simdate and shift_to_simdate
  c1 <- lubridate::wday(all_days, label = T, abbr = T) %in% unlist(strsplit(schedule$shift_weekday, split = ","))
  c2 <- lubridate::yday(all_days) >= schedule$shift_from_simdate
  c3 <- lubridate::yday(all_days) <= schedule$shift_to_simdate
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

sint_in_interval <- function(schedule, interval) {
  stop("In case of change in year, schedule needs to update. Use schar_in_interval")
} # obsolete. Throws error

schar_in_interval <- function(schedule, interval) {
  start_date <- lubridate::floor_date(lubridate::int_start(interval), "day")
  end_date <- lubridate::floor_date(lubridate::int_end(interval), "day")
  if (lubridate::year(start_date) == lubridate::year(end_date - 0.001)) {
    schedule <- update_schedule(lubridate::int_start(interval), schedule)
    res <- sint_in_year_interval(schedule, interval)
  } else {
    # browser()
    my_intervals <- yearly_intervals(interval)
    schedule <- update_schedule(lubridate::int_start(my_intervals[1]), schedule)
    res <- sint_in_year_interval(schedule, my_intervals[1])
    for (i in 2:(length(my_intervals))) {
      start <- lubridate::int_start(my_intervals[i])
      schedule <- update_schedule(start, schedule)
      res <- c(res, sint_in_year_interval(schedule, my_intervals[i]))
    }
  }
  return(res)
}

# Topic "labor %in% schedule" => Calculate overtime, ...
labor_in_year_schar <- function(labor, schedule) {
  # browser()
  assertthat::assert_that(all(class(labor) == c("list", "labor")))
  assertthat::assert_that(labor$vehicle_id == schedule$id)
  # Not true after call for first day???
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
  planned_times <- schar_in_interval(schedule, year_range) # array
  if (length(planned_times) == 0) {
    stop("labor and schedule do not overlap")
  }
  # considered <- logical(length(labor$labor))
  start_date_plan <- lubridate::int_start(planned_times)
  productive <- lubridate::interval(start_date_plan, start_date_plan)
  idle <- lubridate::interval(start_date_plan, start_date_plan)
  overtime <- lubridate::interval(start_date_plan, start_date_plan)
  for (i in 1:length(planned_times)) {
    # Compare workload on a daily basis: labor = productive + idle + overtime
    r <- calculate_daily_times(labor, planned_times[i], idle2overtime = F)
    idle <- c(idle, r$idle); productive <- c(productive, r$productive); overtime <- c(overtime, r$overtime); rm(r)
  }
  labor$idle <- unique(idle[!as.numeric(as.duration(idle), units = "secs") == 0])
  labor$overtime <- unique(overtime[!as.numeric(as.duration(overtime), units = "secs") == 0])
  labor$productive <- unique(productive[!as.numeric(as.duration(productive), units = "secs") == 0])
  return(labor)
}

labor_in_schar <- function(labor, schedule) {
  library(lubridate)
  start_date <- min(lubridate::int_start(labor$labor))
  end_date <- max(lubridate::int_end(labor$labor))
  if (lubridate::year(start_date) == lubridate::year(end_date - 0.001)) {
    labor <- labor_in_year_schar(labor, schedule)
  } else {
    total_span <- lubridate::interval(lubridate::floor_date(start_date, "day"),
      lubridate::ceiling_date(end_date, "day"))
    my_intervals <- yearly_intervals(total_span)
    for (i in 1:(length(my_intervals))) {
      browser()
      schedule <- update_schedule(lubridate::int_start(my_intervals[i]), schedule)
      my_labor <- labor
      my_labor$labor <- labor$labor[labor$labor %within% my_intervals[i]]
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
    labor$idle <- unique(labor$idle[!as.numeric(
      as.duration(labor$idle), units = "secs") == 0])
    labor$overtime <- unique(labor$overtime[!as.numeric(
      as.duration(labor$overtime), units = "secs") == 0])
    labor$productive <- unique(labor$productive[!as.numeric(
      as.duration(labor$productive), units = "secs") == 0])
  }
  return(labor)
}

# Wrapper for whole functionality:
`%scheduled%` <- function(lhs, rhs) {
  if (all(class(lhs) == c("POSIXct", "POSIXt")) &
      class(rhs) == "data.frame") {
    # time or date %in% schedule
    return(lhs %sin% rhs)
  } else if (all(class(lhs) == "numeric") &
      as.character(class(rhs)) == "data.frame") {
    # schedule %in% interval
    lhs %sin% rhs
    return(lhs %sin% rhs)
  } else if (all(class(lhs) == "data.frame") &
      as.character(class(rhs)) == "Interval") {
    # schedule %in% interval
    return(schar_in_interval(lhs, rhs))
  } else if (all(class(lhs) == c("list", "labor")) &
      class(rhs) == "data.frame") {
    return(labor_in_schar(lhs, rhs))
  } else {
    stop("Wrong classes of lhs or rhs.")
  }
}

