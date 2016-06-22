# All names (month, weekday, ...) in english!
Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")


# Synth data
# ==============================================================================

origin_date = lubridate::ymd_hms("2016-01-01 00:00:00")
test_date <- ymd_hms("2016-06-16 14:16:53 CEST") # => "Thurs"

old_vehicles <- data.frame(
  id = c(0,1,911),
  year = rep(0, 3),
  busy_until = c(3e4,0,0), # "2016-01-01 08:20:00 UTC", "Fri"
  shift_from_simdate = c(0, 0, 186), # 186 not calculated.                         Depends on year! Wrong like this
  shift_to_simdate = c(365, 180, 365), # Not calculated.                           Depends on year! Wrong like this
  shift_from_simtime = c(0, 12*3600, 9*3600), # In most cases correct
  shift_to_simtime = c(24*3600, 18*3600, 19*3600),
  shift_weekday = c("Mo, Di, Mi, Do, Fr, Sa, So", "Mo, Di, Mi, Do, Fr", "Sa, So"),
  stringsAsFactors = F)

vehicles <- data.frame(
  id = c(0,1,911),
  update = rep(0, 3), # update => Can be year, month, ...
  busy_until = c(0,0,0), # "2016-01-01 08:20:00 UTC", "Fri"
  shift_from_simdate = c(0, 0, 0), # 186 not calculated.
  shift_to_simdate = c(0, 0, 0), # Not calculated.
  shift_from_simtime = c(0, 0, 0), # In most cases correct
  shift_to_simtime = c(0, 0, 0),
  shift_weekday = character(3),
  schedule = c(
    "Jan-01--Dec-31|00:00--23:59|Mon,Tues,Wed,Thurs,Fri,Sat,Sun", 
    "Jan-01--Jun-30|12:00--18:00|Mon,Tues,Wed,Thurs,Fri", 
    "Jul-06--Dec-31|09:00--19:00|Sat,Sun"),
  stringsAsFactors = F)

labor <- data.frame(
  t <- c(1245, 1405, 3922, 6117, 8611, 8840, 10022, 11764, 12724, 13116),
  dt <- c(1759, 3221, 1224, 2336, 1898, 2016, 3518, 1430, 1824, 1082),
  stringsAsFactors = F)

# Basic functions
# ==============================================================================
dict <- function() {
  dict_g2e <- setNames(
    c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
    c("So", "Mo", "Di", "Mi", "Do", "Fr", "Sa", "Jan", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez"))
  
  dict_e2g <- setNames(
    c("So", "Mo", "Di", "Mi", "Do", "Fr", "Sa", "Jan", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez"), 
    c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  return(list(g2e = dict_g2e, e2g = dict_e2g))
}

int_dict <- function() {
  dict_int2e  <- setNames(
    c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
    c("07", "01", "02", "03", "04", "05", "06", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"))
  
  dict_e2int <- setNames(
    c("07", "01", "02", "03", "04", "05", "06", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
    c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  return(list(e2int = dict_e2int, int2e = dict_int2e))
}

g2e <- function(ger_expr) {
  if (!exists("dict_g2e")) {
    # dictg_g2e <- dict()$g2e
    assign("dict_g2e", dict()$g2e, envir = .GlobalEnv)
  }
  res <- character(length(ger_expr))
  for (i in 1:length(ger_expr)) {
    split <- unlist(strsplit(ger_expr[i], split = ", "))
    assertthat::assert_that(all(split %in% names(dict_g2e)))
    if (length(split) == length(ger_expr[i])) {
      res[i] <- dict_g2e[split]
    } else {
      res[i] <- paste(dict_g2e[split], collapse = ", ")
    }
  }
  return(res)
}

e2g <- function(eng_expr) {
  if (!exists("dict_e2g")) {
    # dictg_e2g <- dict()$e2g
    assign("dict_e2g", dict()$e2g, envir = .GlobalEnv)
  }
  res <- character(length(eng_expr))
  for (i in 1:length(eng_expr)) {
    split <- unlist(strsplit(eng_expr[i], split = ", "))
    assertthat::assert_that(all(split %in% names(dict_e2g)))
    if (length(split) == length(eng_expr[i])) {
      res[i] <- dict_e2g[split]
    } else {
      res[i] <- paste(dict_e2g[split], collapse = ", ")
    }
  }
  return(res)
}

date2simtime <- function(date, origin_date) { # as.simtime?
  return(as.numeric(date - origin_date, units = "secs"))
}

simtime2date <- function(t, origin_date) { # as.date or just t + origin?
  return(origin_date + t)
}

is.schedule <- function(object) {
  res <- F
  if (length(grepl("|", object)) == nrow(object) &
      length(grepl("--", object)) == nrow(object) &
      all(c("schedule", "update") %in% names(vehicles)) &
      any(grepl(" ", object)) == T) {
    res <- T
  }
  return(res)
}

wday <- function(t, origin_date = "2014-01-01 00:00:00") {
  weekdays(as.POSIXct(t, tz = "GMT", origin_date), abbreviate = T)
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
    col.names = c("a", "b", "c"), stringsAsFactors = F)
  dates <- as.character(ints$e2int[as.character(dates_abbr[1, ])])
  return(yday(paste(y, dates, dates_abbr[2, ], sep = "-")))
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
    col.names = c("a", "b", "c"), stringsAsFactors = F)
  time <- as.data.frame(strsplit(as.character(time_abbr), split = ":", fixed = T), 
    col.names = c("a", "b", "c"), stringsAsFactors = F)
  return(60 * (60 * as.integer(time[1, ]) + as.integer(time[2, ])))
}

# Test basic functions
# ==============================================================================
g2e("So") == "Sun"
g2e(c("So", "Di")) == c("Sun", "Tues")
var <- g2e(c("So, Mo, Mi", "Di, Mi, Do, Fr"))
var == c("Sun, Mon, Wed", "Tues, Wed, Thurs, Fri")

e2g(g2e("So")) == "So"
e2g(g2e(c("So", "Di"))) == c("So", "Di")
e2g(var) == c("So, Mo, Mi", "Di, Mi, Do, Fr")

test_sec <- date2simtime(test_date, origin_date)
simtime2date(test_sec, origin_date) == test_date

# Schedule functions
# ==============================================================================
# For names see C:\Users\sqc\switchdrive\Dispo144\Projektmanagement\Interne Besprechungen\2016_06_21 Schedules
# s always for schedule... Schedule needs 5 integers to be well defined

is_on_duty <- function(t, vehicles) {
  return(vehicles[vehicles$shift_from_simdate <= simTimeR::simDate(t) &
      vehicles$shift_to_simdate >= simTimeR::simDate(t) &
      vehicles$shift_from_simtime <= simTimeR::simTime(t) &
      vehicles$shift_to_simtime > simTimeR::simTime(t) &
      grepl(simTimeR::simWeekday(t), vehicles$shift_weekday), ])
}

is_on_duty2 <- function(t, vehicles) {
  return(vehicles[vehicles$shift_from_simdate <= simTimeR::simDate(t) &
      vehicles$shift_to_simdate >= simTimeR::simDate(t) &
      vehicles$shift_from_simtime <= simTimeR::simTime(t) &
      vehicles$shift_to_simtime > simTimeR::simTime(t) &
      grepl(wday(t, origin_date), vehicles$shift_weekday), ])
}

int_in_sint <- function(t, vehicles) { # ........................................ rule, vectorized!
  return(vehicles$shift_from_simdate <= simTimeR::simDate(t) &
      vehicles$shift_to_simdate >= simTimeR::simDate(t) &
      vehicles$shift_from_simtime <= simTimeR::simTime(t) &
      vehicles$shift_to_simtime > simTimeR::simTime(t) &
      grepl(wday(t), vehicles$shift_weekday))
}

update_schedule <- function(date, vehicles) {
  # tz <- lubridate::tz(date)
  y <- lubridate::year(date)
  vehicles$update <- rep(y, nrow(vehicles))
  df <- as.data.frame(strsplit(vehicles$schedule, split = "|", fixed = T), 
    col.names = c("a", "b", "c"), stringsAsFactors = F)
  
  # Update dates
  date_df <- as.data.frame(strsplit(as.character(df[1, ]), split = "--", fixed = T),
    col.names = c("a", "b", "c"), stringsAsFactors = F)
  vehicles$shift_from_simdate <- calculate_shift_simdate(y, date_df, "from")
  vehicles$shift_to_simdate <- calculate_shift_simdate(y, date_df, "to")
  
  # Update time
  time_df <- as.data.frame(strsplit(as.character(df[2, ]), split = "--", fixed = T),
    col.names = c("a", "b", "c"), stringsAsFactors = F)
  vehicles$shift_from_simtime <- calculate_shift_simtime(y, time_df, "from")
  vehicles$shift_to_simtime <- calculate_shift_simtime(y, time_df, "to")
  
  # Update shift_weekdays
  vehicles$shift_weekday <- as.character(df[3, ])
  
  assign("vehicles", vehicles, envir = .GlobalEnv)
  return(vehicles)
}

int_in_schar <- function(t, vehicles) {
  date <- simtime2date(t, origin_date)
  vehicles <- update_schedule(date, vehicles)
  return(int_in_sint(t, vehicles))
}

posix_in_sint <- function(date, vehicles) {
  t <- date2simtime(date, origin_date)
  return(int_in_sint(t, vehicles))
}

`%fast_sin%` <- function(t, vehicles) {
  date <- simtime2date(t, origin_date)
  if (year(date) != vehicles$update[1]) { # & vehicles$update[1] != 0?
    vehicles <- update_schedule(date, vehicles)
  }
  res <- int_in_sint(t, vehicles)
  return(res)
}

`%sin%` <- function(t, vehicles) {
  if (class(t)[1] == "integer") {
    res <- t %fast_sin% vehicles
  } else {
    if (year(t) == vehicles$update[1]) {
      if (vehicles$update[1] != 0) {
        res <- posix_in_sint(t, vehicles)
      } else {
        # Corresponds to posix_in_schar
        vehicles <- update_schedule(t, vehicles)
        res <- int_in_sint(t, vehicles)
      }
    }
  }
  return(res)
}


# Schedule functions
# ==============================================================================
is_on_duty(test_sec, old_vehicles)
test_sec %fast_sin% vehicles
vehicles <- update_schedule(test_date, vehicles)
test_sec %fast_sin% vehicles
test_date %sin% vehicles


# Microbenchmarks
# ==============================================================================
# print(summary(microbenchmark::microbenchmark(
#   g2e(c("So, Mo, Mi", "Di, Mi, Do, Fr")),
#   e2g(var),
#   date2simtime(test_date, origin_date),
#   times = 1000L, unit = "us")))

print(summary(microbenchmark::microbenchmark(
  test_sec %fast_sin% vehicles,
  vehicles[test_sec %fast_sin% vehicles, ],
  my_vehicles <- vehicles[test_sec %fast_sin% vehicles, ],
  update_schedule(test_date, vehicles),
  test_date %sin% vehicles, 
  times = 100L, unit = "us")))

vehicles <- update_schedule(test_date, vehicles)
print(summary(microbenchmark::microbenchmark(
  is_on_duty(test_sec, old_vehicles), # needs german weekdays
  is_on_duty2(test_sec, old_vehicles), # needs german weekdays
  vehicles[int_in_sint(test_sec, vehicles), ],
  vehicles[test_sec %fast_sin% vehicles, ],
  times = 100L, unit = "us")))

