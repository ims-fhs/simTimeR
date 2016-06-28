# imsbasics::clc()
# All names (month, weekday, ...) in english!
# Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")
# library(lubridate)

# Synth data
# ==============================================================================

# origin_date = lubridate::ymd_hms("2016-01-01 00:00:00")
# test_date <- lubridate::ymd_hms("2016-06-16 14:16:53 CEST") # => "Thurs"
# test_date2 <- lubridate::ymd_hms("2016-06-16 23:59:59 CEST") # => "Thurs"
# # lubridate::ymd_hms("2016-06-16 24:00:00 CEST") throws error! => "2016-06-17 00:00:00 CEST"
#
# old_vehicles <- data.frame(
#   id = c(0,1,911),
#   year = rep(0, 3),
#   busy_until = c(3e4,0,0), # "2016-01-01 08:20:00 UTC", "Fri"
#   shift_from_simdate = c(0, 0, 186), # 186 not calculated.                      Depends on year! Wrong like this
#   shift_to_simdate = c(365, 180, 365), # Not calculated.                        Depends on year! Wrong like this
#   shift_from_simtime = c(0, 12*3600, 9*3600), # In most cases correct
#   shift_to_simtime = c(24*3600, 18*3600, 19*3600),
#   shift_weekday = c("Mo, Di, Mi, Do, Fr, Sa, So", "Mo, Di, Mi, Do, Fr", "Sa, So"),
#   stringsAsFactors = F)
#
# vehicles <- data.frame(
#   id = c(0,1,911),
#   update = rep(0, 3), # update => Can be year, month, ...
#   busy_until = c(0,0,0), # "2016-01-01 08:20:00 UTC", "Fri"
#   shift_from_simdate = c(0, 0, 0), # 186 not calculated.
#   shift_to_simdate = c(0, 0, 0), # Not calculated.
#   shift_from_simtime = c(0, 0, 0), # In most cases correct
#   shift_to_simtime = c(0, 0, 0),
#   shift_weekday = character(3),
#   schedule = c( # Needs wrapper 24:00 = 00:00 next day
#     "Jan-01--Dec-31|00:00--24:00|Mon,Tues,Wed,Thurs,Fri,Sat,Sun", # ............ 24:00 works?
#     "Jan-01--Jun-30|12:00--18:00|Mon,Tues,Wed,Thurs,Fri",
#     "Jul-06--Dec-31|09:00--19:00|Sat,Sun"),
#   stringsAsFactors = F)
#
# missions <- data.frame(
#   t_alarm_sec = c(1245, 1405, 3922, 6117, 8611, 8840, 10022, 11764, 12724, 13716),
#   dt_to_completion = c(1759, 3221, 1224, 2336, 1898, 2016, 3518, 1430, 1824, 1082),
#   vehicle_id = c(1, 0, 1, 911, 0, 1, 911, 1, 0, 911),
#   stringsAsFactors = F)
#
# labor0 <- data.frame(
#   t = 0,
#   dt = 365*24*3600,
#   idle = 0,
#   overtime = 0,
#   stringsAsFactors = F)
#
# # Basic functions
# # ==============================================================================
# dict <- function() {
#   dict_g2e <- setNames(
#     c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
#     c("So", "Mo", "Di", "Mi", "Do", "Fr", "Sa", "Jan", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez"))
#
#   dict_e2g <- setNames(
#     c("So", "Mo", "Di", "Mi", "Do", "Fr", "Sa", "Jan", "Feb", "Mar", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez"),
#     c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
#   return(list(g2e = dict_g2e, e2g = dict_e2g))
# }
#
# int_dict <- function() {
#   dict_int2e  <- setNames(
#     c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
#     c("07", "01", "02", "03", "04", "05", "06", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"))
#
#   dict_e2int <- setNames(
#     c("07", "01", "02", "03", "04", "05", "06", "01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
#     c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
#   return(list(e2int = dict_e2int, int2e = dict_int2e))
# }
#
# g2e <- function(ger_expr) {
#   if (!exists("dict_g2e")) {
#     # dictg_g2e <- dict()$g2e
#     assign("dict_g2e", dict()$g2e, envir = .GlobalEnv)
#   }
#   res <- character(length(ger_expr))
#   for (i in 1:length(ger_expr)) {
#     split <- unlist(strsplit(ger_expr[i], split = ", "))
#     assertthat::assert_that(all(split %in% names(dict_g2e)))
#     if (length(split) == length(ger_expr[i])) {
#       res[i] <- dict_g2e[split]
#     } else {
#       res[i] <- paste(dict_g2e[split], collapse = ", ")
#     }
#   }
#   return(res)
# }
#
# e2g <- function(eng_expr) {
#   if (!exists("dict_e2g")) {
#     # dictg_e2g <- dict()$e2g
#     assign("dict_e2g", dict()$e2g, envir = .GlobalEnv)
#   }
#   res <- character(length(eng_expr))
#   for (i in 1:length(eng_expr)) {
#     split <- unlist(strsplit(eng_expr[i], split = ", "))
#     assertthat::assert_that(all(split %in% names(dict_e2g)))
#     if (length(split) == length(eng_expr[i])) {
#       res[i] <- dict_e2g[split]
#     } else {
#       res[i] <- paste(dict_e2g[split], collapse = ", ")
#     }
#   }
#   return(res)
# }
#
# date2simtime <- function(date, origin_date) { # as.simtime?
#   return(as.numeric(date - origin_date, units = "secs"))
# }
#
# simtime2date <- function(t, origin_date) { # as.date or just t + origin?
#   return(origin_date + t)
# }
#
# is.schedule <- function(object) {
#   res <- F
#   if (length(grepl("|", object)) == nrow(object) &
#       length(grepl("--", object)) == nrow(object) &
#       all(c("schedule", "update") %in% names(vehicles)) &
#       any(grepl(" ", object)) == T) {
#     res <- T
#   }
#   return(res)
# }
#
# wday <- function(t, origin_date = "2014-01-01 00:00:00") {
#   weekdays(as.POSIXct(t, tz = "GMT", origin_date), abbreviate = T)
# }
#
# is_free <- function(object, t) {
#   return(vehicles[object$busy_until <= t,])
# }
#
# calculate_shift_simdate <- function(y, date_df, type=stop("Argument from or to needed")) {
#   ints <- int_dict()
#   if (type == "from") {
#     i <- 1
#   } else if (type == "to") {
#     i <- 2
#   } else {
#     stop("Argument from or to needed")
#   }
#   dates_abbr <- as.data.frame(strsplit(as.character(date_df[i, ]), split = "-", fixed = T),
#     stringsAsFactors = F) # col.names = c("a", "b", "c") = 3 vehicles
#   dates <- as.character(ints$e2int[as.character(dates_abbr[1, ])])
#   return(yday(paste(y, dates, dates_abbr[2, ], sep = "-")))
# }
#
# calculate_shift_simtime <- function(y, time_df, type=stop("Argument from or to needed")) {
#   ints <- int_dict()
#   if (type == "from") {
#     i <- 1
#   } else if (type == "to") {
#     i <- 2
#   } else {
#     stop("Argument from or to needed")
#   }
#   time_abbr <- as.data.frame(strsplit(as.character(time_df[i, ]), split = "-", fixed = T),
#     stringsAsFactors = F) # col.names = c("a", "b", "c") = 3 vehicles
#   time <- as.data.frame(strsplit(as.character(time_abbr), split = ":", fixed = T),
#     stringsAsFactors = F)  # col.names = c("a", "b", "c") = 3 vehicles
#   return(60 * (60 * as.integer(time[1, ]) + as.integer(time[2, ])))
# }
#
# first_day_of_year <- function(y) {
#   return(lubridate::ymd_hms(paste0(y, "-01-01 00:00:00")))
# }
#
# yearly_intervals <- function(interval) {
#   start_date <- lubridate::int_start(interval)
#   end_date <- lubridate::int_end(interval)
#   my_years <- c(lubridate::year(start_date):lubridate::year(end_date))
#   my_array <- my_years[1]
#   for (i in 2:(length(my_years))) {
#     my_array <- c(my_array, rep(my_years[i], 2))
#   }
#   my_array <- c(my_array, my_years[length(my_years)])
#   # browser()
#   interval_on_year_base <- first_day_of_year(my_array)
#   interval_on_year_base[1] <- start_date
#   interval_on_year_base[length(interval_on_year_base)] <- end_date
#   res <- lubridate::interval(interval_on_year_base[1],interval_on_year_base[2])
#   for (i in 1:(length(my_array)/2-1)) {
#     res <- c(res, lubridate::interval(interval_on_year_base[2*i+1],interval_on_year_base[2*i+2]))
#   }
#   if (lubridate::int_start(res[length(res)]) == lubridate::int_end(res[length(res)])) {
#     res <- res[-length(res)]
#   }
#   return(res)
# }
#
#
# # Test basic functions
# # ==============================================================================
# g2e("So") == "Sun"
# g2e(c("So", "Di")) == c("Sun", "Tues")
# var <- g2e(c("So, Mo, Mi", "Di, Mi, Do, Fr"))
# var == c("Sun, Mon, Wed", "Tues, Wed, Thurs, Fri")
#
# e2g(g2e("So")) == "So"
# e2g(g2e(c("So", "Di"))) == c("So", "Di")
# e2g(var) == c("So, Mo, Mi", "Di, Mi, Do, Fr")
#
# test_sec <- date2simtime(test_date, origin_date)
# simtime2date(test_sec, origin_date) == test_date
#
# # Schedule functions
# # ==============================================================================
# # For names see C:\Users\sqc\switchdrive\Dispo144\Projektmanagement\Interne Besprechungen\2016_06_21 Schedules
# # s always for schedule... Schedule needs 5 integers to be well defined
#
# is_on_duty <- function(t, vehicles) {
#   return(vehicles[vehicles$shift_from_simdate <= simTimeR::simDate(t) &
#       vehicles$shift_to_simdate >= simTimeR::simDate(t) &
#       vehicles$shift_from_simtime <= simTimeR::simTime(t) &
#       vehicles$shift_to_simtime > simTimeR::simTime(t) &
#       grepl(simTimeR::simWeekday(t), vehicles$shift_weekday), ])
# }
#
# is_on_duty2 <- function(t, vehicles) {
#   return(vehicles[vehicles$shift_from_simdate <= simTimeR::simDate(t) &
#       vehicles$shift_to_simdate >= simTimeR::simDate(t) &
#       vehicles$shift_from_simtime <= simTimeR::simTime(t) &
#       vehicles$shift_to_simtime > simTimeR::simTime(t) &
#       grepl(wday(t, origin_date), vehicles$shift_weekday), ])
# }
#
# int_in_sint <- function(t, vehicles) { # ........................................ rule, vectorized!
#   return(vehicles$shift_from_simdate <= simTimeR::simDate(t) &
#       vehicles$shift_to_simdate >= simTimeR::simDate(t) &
#       vehicles$shift_from_simtime <= simTimeR::simTime(t) &
#       vehicles$shift_to_simtime > simTimeR::simTime(t) &
#       grepl(wday(t), vehicles$shift_weekday))
# }
#
# update_schedule <- function(date, schedule) {
#   assertthat::assert_that(all(c("update", "shift_from_simdate", "shift_to_simdate",
#     "shift_from_simtime", "shift_to_simtime", "shift_weekday", "schedule") %in% names(schedule)))
#   # tz <- lubridate::tz(date)
#   y <- lubridate::year(date)
#   schedule$update <- rep(y, nrow(schedule))
#   # browser()
#   df <- as.data.frame(strsplit(schedule$schedule, split = "|", fixed = T),
#     stringsAsFactors = F) # col.names = c("a", "b", "c") = 3 vehicles
#
#   # Update dates
#   date_df <- as.data.frame(strsplit(as.character(df[1, ]), split = "--", fixed = T),
#     stringsAsFactors = F) # col.names = c("a", "b", "c") = 3 vehicles
#   schedule$shift_from_simdate <- calculate_shift_simdate(y, date_df, "from")
#   schedule$shift_to_simdate <- calculate_shift_simdate(y, date_df, "to")
#
#   # Update time
#   time_df <- as.data.frame(strsplit(as.character(df[2, ]), split = "--", fixed = T),
#     stringsAsFactors = F) # col.names = c("a", "b", "c") = 3 vehicles
#   schedule$shift_from_simtime <- calculate_shift_simtime(y, time_df, "from")
#   schedule$shift_to_simtime <- calculate_shift_simtime(y, time_df, "to")
#
#   # Update shift_weekdays
#   schedule$shift_weekday <- as.character(df[3, ])
#
#   assign("schedule", schedule, envir = .GlobalEnv)
#   return(schedule)
# }
# # shift_from_simdate -1? ....................................................... ???
# # shift_to_simdate -1?
# # shift_to_simtime + 60?
#
# int_in_schar <- function(t, vehicles) {
#   mydate<- simtime2date(t, origin_date)
#   vehicles <- update_schedule(mydate, vehicles)
#   return(int_in_sint(t, vehicles))
# }
#
# posix_in_sint <- function(date, vehicles) {
#   t <- date2simtime(date, origin_date)
#   return(int_in_sint(t, vehicles))
# }
#
# `%fast_sin%` <- function(t, vehicles) {
#   mydate<- simtime2date(t, origin_date)
#   if (year(mydate) != vehicles$update[1]) { # & vehicles$update[1] != 0?
#     vehicles <- update_schedule(mydate, vehicles)
#   }
#   res <- int_in_sint(t, vehicles)
#   return(res)
# }
#
# `%sin%` <- function(t, vehicles) {
#   if (class(t)[1] == "integer") {
#     res <- t %fast_sin% vehicles
#   } else {
#     if (year(t) == vehicles$update[1]) {
#       if (vehicles$update[1] != 0) {
#         res <- posix_in_sint(t, vehicles)
#       } else {
#         # Corresponds to posix_in_schar
#         vehicles <- update_schedule(t, vehicles)
#         res <- int_in_sint(t, vehicles)
#       }
#     }
#   }
#   return(res)
# }
#
#
# # Test schedule functions
# # ==============================================================================
# is_on_duty(test_sec, old_vehicles)
# test_sec %fast_sin% vehicles
# vehicles <- update_schedule(test_date, vehicles)
# test_sec %fast_sin% vehicles
# test_date %sin% vehicles # = TRUE  TRUE FALSE
# test_date2 %sin% vehicles  # = TRUE FALSE FALSE
#
# # ==============================================================================
# # Topic "labor"
# # labor time per year
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
#
# is.labor <- function(labor) {
#   ifelse(all(c("t", "dt", "idle", "overtime") %in% names(labor)), return(T), return(F))
# }
#
# l <- as.labor(missions)
# l2 <- l
#
# l_int <- interval(origin_date+labor0$t, origin_date+labor0$t+labor0$dt)
# is.labor(l)
# schedule0 <- update_schedule(test_date, vehicles)[1, ]
#
# interval(origin_date+l$t, origin_date+l$t+l$dt)
# l2$interval <- interval(origin_date+l$t, origin_date+l$t+l$dt)
# interval(origin_date+l$t, origin_date+l$t+l$dt)/lubridate::dseconds() # duration!
# interval(origin_date+l$t, origin_date+l$t+l$dt)/lubridate::dminutes() # dhours, dweeks
# sum(interval(origin_date+l$t, origin_date+l$t+l$dt)/lubridate::dminutes()) # Sum array
#
# lubridate::yday(lubridate::int_start(l_int))
# lubridate::yday(lubridate::int_end(l_int))
# myDates <- seq(from = lubridate::int_start(l_int), to = lubridate::int_end(l_int), by = "days")
#
# c1 <- lubridate::wday(myDates, label = T, abbr = T) %in% unlist(strsplit(schedule0$shift_weekday, split = ","))
# c2 <- lubridate::yday(myDates) >= lubridate::yday(lubridate::int_start(l_int))
# c3 <- lubridate::yday(myDates) <= lubridate::yday(lubridate::int_end(l_int))
# res <- myDates[c1 & c2 & c3]
# dt_tag <- as.duration(schedule0$shift_to_simtime-schedule0$shift_from_simtime)
# # as.duration(schedule0$shift_to_simtime-schedule0$shift_from_simtime)/lubridate::dhours()
# real_l <- data.frame(t = res, dt = rep(dt_tag, length(res)))
# as.duration(sum(real_l$dt))/lubridate::dhours()
# as.duration(sum(real_l$dt[month(real_l$t, label = T, abbr = T) == "Jan"]))/lubridate::dhours()
# as.duration(sum(real_l$dt[month(real_l$t, label = T, abbr = T) == "Jan"]))/lubridate::ddays()
#
# # Create array of theory labor time:
# all_times <- interval(res + schedule0$shift_from_simtime, res + schedule0$shift_to_simtime)
# as.duration(all_times)/lubridate::dhours() # huge array
# sum(as.duration(all_times)/lubridate::dhours()) # = 8777.9 (366*24-366*1/60 = 8777.9)
# sum(as.duration(all_times)/lubridate::dyears()) # = 1.002043 (365*24 = 8760)
#
#
# # Overtime...
# # int_start, int_end: Start and end (time instant)
# # int_flip: Reverse time order of interval
# # int_shift: Shift interval by duration()
# # int_aligns: TRUE if int1 and int2 begin or end on the same moment
# # union: Unification, maximum possible time span
# # intersect: Intersection
# # setdiff: Non-symmetric difference
# # %within%: Check if lhs FULLY contained in rhs. Also T for "A==B".
# #           e.g. real2 %within% theory_labor - same is FALSE for real1
#
# # Example
# # theory: ...............t0---------------t1..................
# # real 1: .......t2******-------------------******t3.......... => -- = intersect (symmetric) / setdiff bad.
# # real 2: ...............::::::t4-----t5::::.................. => Check with real2 %within% theory_labor == T
# # real 3: .......t2******------t4:::::::::::.................. => :: = setdiff(theory_labor, real3)
# #                                                                 ** = setdiff(real3, theory_labor)
# # real 4: .......t2******------t4:::::t5----******t3.......... => setdiff does not work
# #                                ok.ok?????????????? order important. Compare only 1:1
# # real 5: ..t6...t2........................................... => intersect(theory_labor, real5) = NA--NA !!!
# # real 6: ...............:::::::::::::::::t5----******t3.......... => ok. As real 3
# # test :  ...............t0 ------------- t1..................
#
# # Assumptions:
# # 1. There are much more reals than theory
# # 2. There could be reals with no theory
# # 3. Comparison is done 1:1, not "1:c(1,2)"
#
# # Cases:
# # 1. !theory %within% real => Only Overtime. Delete those for further analysis
# # 2. real %within% theory
# # 3. theory == real
#
# # Combinations
# t0 <- lubridate::ymd_hms("2016-06-05 00:00:00") # theory_labor/lubridate::ddays() == 10
# t1 <- lubridate::ymd_hms("2016-06-15 00:00:00") # real1/lubridate::ddays() == 15
# t2 <- lubridate::ymd_hms("2016-06-01 00:00:00")
# t3 <- lubridate::ymd_hms("2016-06-16 00:00:00")
# t4 <- lubridate::ymd_hms("2016-06-07 00:00:00")
# t5 <- lubridate::ymd_hms("2016-06-11 00:00:00")
# t6 <- lubridate::ymd_hms("2016-05-20 00:00:00")
# theory_labor <- interval(t0, t1)
# test <- interval(t0, t1)
# real1 <- interval(t2, t3)
# real2 <- interval(t4, t5)
# real3 <- interval(t2, t4)
# real4 <- c(interval(t2, t4), interval(t5, t3))
# real5 <- interval(t6, t2)
# real6 <- interval(t5, t3)
# intersect(theory_labor, real1) # 2016-06-05 UTC--2016-06-15 UTC
# intersect(real2, real3) # 2016-06-07 UTC--2016-06-07 UTC => intersect(real2, real3)/lubridate::dseconds() == 0
# intersect(theory_labor, real4) # intersect(theory_labor, real4)/lubridate::ddays() = c(2, 4)
#
# # schedule (class df) and labor (class labor)
# as.labor.list <- function(missions, vehicle) {
#   missions <- missions[missions$vehicle_id == vehicle$id, ]
#   labor <- list(
#     vehicle_id = vehicle$id,
#     schedule = vehicle$schedule,
#     labor = lubridate::interval(origin_date + missions$t_alarm_sec,
#       origin_date + missions$t_alarm_sec + missions$dt_to_completion),
#     productive = NA,
#     idle = NA,
#     overtime = NA,
#     stringsAsFactors = F)
#   class(labor) <- c(class(labor), "labor")
#   return(labor)
# }
#
# labor_id0 <- as.labor.list(missions, vehicles[1, ]) # 24 h shift for vehicle 1
# labor_id1 <- as.labor.list(missions, vehicles[2, ])
# labor_id1$labor <- lubridate::int_shift(labor_id1$labor, lubridate::dhours(12)) # Cannot start before shift
# labor_id911 <- as.labor.list(missions, vehicles[3, ])
# labor_id911$labor <- lubridate::int_shift(labor_id911$labor, lubridate::dseconds(26283))
# schedule0 <- update_schedule(test_date, vehicles)[1, ]
# i0 <- interval(lubridate::ymd_hms("2016-01-01 00:00:00"),
#   lubridate::ymd_hms("2017-01-01 00:00:00"))
# i1 <- interval(lubridate::ymd_hms("2016-01-01 00:00:00"),
#   lubridate::ymd_hms("2016-12-31 12:00:00"))
# i2 <- interval(lubridate::ymd_hms("2016-01-01 12:00:00"),
#   lubridate::ymd_hms("2016-12-31 12:00:00"))
# i3 <- interval(lubridate::ymd_hms("2016-02-01 12:00:00"),
#   lubridate::ymd_hms("2019-10-27 12:00:00"))
# i4 <- interval(lubridate::ymd_hms("2016-02-01 12:00:00"),
#   lubridate::ymd_hms("2019-01-01 00:00:00"))
# i_change <- lubridate::int_shift(i0, lubridate::dyears(0.5)) # start/ends at 12:00!
# yearly_intervals(i3)
# yearly_intervals(i4)
#
#
# sint_in_year_interval <- function(schedule, interval) {
#   assertthat::assert_that(nrow(schedule) == 1)
#   # Array containing all days in interval:
#   start_date <- lubridate::floor_date(lubridate::int_start(interval), "day")
#   end_date <- lubridate::floor_date(lubridate::int_end(interval), "day")
#   assertthat::assert_that(lubridate::year(start_date) == lubridate::year(end_date - 0.001))
#   all_days <- seq(from = start_date, to = end_date, by = "days")
#
#   # Filter days according to weekday, shift_from_simdate and shift_to_simdate
#   c1 <- lubridate::wday(all_days, label = T, abbr = T) %in% unlist(strsplit(schedule$shift_weekday, split = ","))
#   c2 <- lubridate::yday(all_days) >= schedule$shift_from_simdate
#   c3 <- lubridate::yday(all_days) <= schedule$shift_to_simdate
#   real_labor_days <- all_days[c1 & c2 & c3]
#
#   # Set up array of time intervals including shift_from_simtime and shift_to_simtime
#   res <- lubridate::interval(real_labor_days + schedule$shift_from_simtime,
#     real_labor_days + schedule$shift_to_simtime)
#
#   # If intersection is empty delete unused intervals
#   overlaps <- lubridate::int_overlaps(res, interval)
#   if (any(!overlaps)) {
#     res <- res[-which(!overlaps)]
#   }
#
#   # Get rid of wrong or too long intervals
#   not_within_interval <- !res %within% interval
#   if (any(not_within_interval)) {
#     throw_out <- as.numeric(lubridate::duration(lubridate::intersect(
#       res, interval))) == 0 & not_within_interval
#     restrict_to_overlap <- as.numeric(lubridate::duration(lubridate::intersect(
#       res, interval))) > 0 & not_within_interval
#     if (any(throw_out)) {
#       # Delete outliers
#       res <- res[-which(throw_out)]
#     } else if (any(restrict_to_overlap)) {
#       # only take overlapping part of both intervals
#       res[which(restrict_to_overlap)] <- intersect(res[restrict_to_overlap], interval)
#     } else {
#       stop("Unknown case")
#     }
#   }
#   return(res)
# }
#
# sint_in_interval <- function(schedule, interval) {
#   stop("In case of change in year, schedule needs to update. Use schar_in_interval")
# } # obsolete. Throws error
#
# sint_in_year_interval(vehicles[1, ], i0)
# sint_in_year_interval(vehicles[1, ], i1)
# sint_in_year_interval(vehicles[1, ], i2)
# # sint_in_year_interval(vehicles[1, ], i_change) # throws error
# sint_in_year_interval(vehicles[2, ], i0)
# sint_in_year_interval(vehicles[2, ], i1)
# sint_in_year_interval(vehicles[2, ], i2)
# sint_in_year_interval(vehicles[3, ], i0)
# sint_in_year_interval(vehicles[3, ], i1)
# sint_in_year_interval(vehicles[3, ], i2)
#
# schar_in_interval <- function(schedule, interval) {
#   start_date <- lubridate::floor_date(lubridate::int_start(interval), "day")
#   end_date <- lubridate::floor_date(lubridate::int_end(interval), "day")
#   if (lubridate::year(start_date) == lubridate::year(end_date - 0.001)) {
#     res <- sint_in_year_interval(schedule, interval)
#   } else {
#     # browser()
#     my_intervals <- yearly_intervals(interval)
#     schedule <- update_schedule(lubridate::int_start(my_intervals[1]), schedule)
#     res <- sint_in_year_interval(schedule, my_intervals[1])
#     for (i in 2:(length(my_intervals))) {
#       start <- lubridate::int_start(my_intervals[i])
#       schedule <- update_schedule(start, schedule)
#       res <- c(res, sint_in_year_interval(schedule, my_intervals[i]))
#     }
#   }
#   return(res)
# }
#
# schar_in_interval(vehicles[1, ], i0)
# schar_in_interval(vehicles[1, ], i1)
# schar_in_interval(vehicles[1, ], i2)
# schar_in_interval(vehicles[1, ], i_change)
# schar_in_interval(vehicles[2, ], i0)
# schar_in_interval(vehicles[2, ], i1)
# schar_in_interval(vehicles[2, ], i2)
# schar_in_interval(vehicles[3, ], i0)
# schar_in_interval(vehicles[3, ], i1)
# schar_in_interval(vehicles[3, ], i2)
# schar_in_interval(vehicles[3, ], i4)
#
# # From 51f:
# calculate_daily_times <- function(labor, planned_times, idle2overtime = F) {
#   # Check, that only one day is considered
#   cond_same_day <- lubridate::wday(lubridate::int_start(planned_times)) == lubridate::wday(lubridate::int_start(labor$labor))
#   remaining <- labor$labor[cond_same_day]
#   # planned_times %within% remaining # [1] FALSE FALSE FALSE FALSE FALSE not useful
#   cond_productive <- remaining %within% planned_times
#   # [1]  TRUE  TRUE FALSE FALSE ... productive
#   cond_overlap <- lubridate::int_overlaps(remaining, planned_times)
#   # [1]  TRUE  TRUE  TRUE FALSE ... Change from F to T !!!
#   cond_split <- xor(cond_productive, cond_overlap)
#   # [1] FALSE FALSE  TRUE FALSE
#
#   # Separate initial idle-possibility
#   start_date_plan <- lubridate::int_start(planned_times)
#   start_date_labor <- lubridate::int_start(remaining[1])
#   productive <- lubridate::interval(start_date_plan, start_date_plan)
#   idle <- lubridate::interval(start_date_plan, start_date_plan)
#   overtime <- lubridate::interval(start_date_plan, start_date_plan)
#   if (start_date_labor < start_date_plan) {
#     stop("start_date_labor < start_date_plan not possible")
#   } else if (start_date_labor == start_date_plan) {
#     # Do nothing idle time 0
#   } else {
#     idle <- c(idle, lubridate::interval(start_date_plan, start_date_labor))
#   }
#
#   # Handle split in case labor is crossing end of planned_time (*)
#   if (sum(cond_split) == 1) {
#     first_part <- remaining[1:(which(cond_split)-1)]
#     third_part <- remaining[(which(cond_split)+1):length(cond_split)]
#     split_interval <- remaining[cond_split]
#     start <- lubridate::int_start(split_interval)
#     end <- lubridate::int_end(split_interval)
#     intermediate <- lubridate::int_end(planned_times)
#     remaining <- c(first_part,
#       lubridate::interval(start, intermediate),
#       lubridate::interval(intermediate, end), third_part)
#   } else if (sum(cond_split) > 1) {
#     stop("Can't be...")
#   } # else: Do nothing
#
#   start_ref <- lubridate::int_start(planned_times)
#   for (k in 1:length(remaining)) {
#     # browser()
#     my_interval <- remaining[k]
#     my_start <- lubridate::int_start(my_interval)
#     my_stop <- lubridate::int_end(my_interval)
#     if (idle2overtime) {
#       overtime <- c(overtime, lubridate::interval(start_ref, my_start))
#     } else {
#       idle <- c(idle, lubridate::interval(start_ref, my_start))
#     }
#     if (my_stop <= lubridate::int_end(planned_times)) {
#       productive <- c(productive, my_interval)
#     } else {
#       # According to (*) the whole interval is in or out
#       overtime <- c(overtime, my_interval)
#     }
#     start_ref <- my_stop
#   }
#
#   labor$idle <- unique(idle[!as.numeric(as.duration(idle), units = "secs") == 0])
#   labor$overtime <- unique(overtime[!as.numeric(as.duration(overtime), units = "secs") == 0])
#   labor$productive <- unique(productive[!as.numeric(as.duration(productive), units = "secs") == 0])
#   return(labor)
# }
#
# labor_in_year_schar <- function(labor, schedule) {
#   # browser()
#   assertthat::assert_that(all(class(labor) == c("list", "labor")))
#   assertthat::assert_that(labor$vehicle_id == schedule$id)
#   # Not true after call for first day???
#   assertthat::assert_that(as.numeric(as.duration(labor$idle), units = "secs") == 0 |
#       is.na(labor$idle))
#   assertthat::assert_that(as.numeric(as.duration(labor$overtime), units = "secs") == 0 |
#       is.na(labor$overtime))
#   start_date <- min(lubridate::int_start(labor$labor))
#   end_date <- max(lubridate::int_end(labor$labor))
#   assertthat::assert_that(lubridate::year(start_date) == lubridate::year(end_date - 0.001))
#   schedule <- update_schedule(start_date, schedule)
#
#   year_range <- lubridate::interval(lubridate::floor_date(start_date, "day"),
#     lubridate::ceiling_date(end_date, "day"))
#   planned_times <- schar_in_interval(schedule, year_range) # array
#   # considered <- logical(length(labor$labor))
#   start_date_plan <- lubridate::int_start(planned_times)
#   productive <- lubridate::interval(start_date_plan, start_date_plan)
#   idle <- lubridate::interval(start_date_plan, start_date_plan)
#   overtime <- lubridate::interval(start_date_plan, start_date_plan)
#   for (i in 1:length(planned_times)) {
#     # Compare workload on a daily basis: labor = productive + idle + overtime
#     r <- calculate_daily_times(labor, planned_times[i], idle2overtime = F)
#     idle <- c(idle, r$idle); productive <- c(productive, r$productive); overtime <- c(overtime, r$overtime); rm(r)
#   }
#   labor$idle <- unique(idle[!as.numeric(as.duration(idle), units = "secs") == 0])
#   labor$overtime <- unique(overtime[!as.numeric(as.duration(overtime), units = "secs") == 0])
#   labor$productive <- unique(productive[!as.numeric(as.duration(productive), units = "secs") == 0])
#   return(labor)
# }
# # Test:
# new_labor1 <- labor_in_year_schar(labor_id1, vehicles[2, ])
#
# labor_in_schar <- function(labor, schedule) {
#   start_date <- min(lubridate::int_start(labor$labor))
#   end_date <- max(lubridate::int_end(labor$labor))
#   if (lubridate::year(start_date) == lubridate::year(end_date - 0.001)) {
#     labor <- labor_in_year_schar(labor, schedule)
#   } else {
#     total_span <- lubridate::interval(lubridate::floor_date(start_date, "day"),
#       lubridate::ceiling_date(end_date, "day"))
#     my_intervals <- yearly_intervals(total_span)
#     for (i in 1:(length(my_intervals))) {
#       browser()
#       schedule <- update_schedule(lubridate::int_start(my_intervals[i]), schedule)
#       my_labor <- labor
#       my_labor$labor <- labor$labor[labor$labor %within% my_intervals[i]]
#       my_labor$idle <- lubridate::interval("1900-01-01 00:00:00", "1900-01-01 00:00:00")
#       my_labor$overtime <- lubridate::interval("1900-01-01 00:00:00", "1900-01-01 00:00:00")
#       my_labor$productive <- lubridate::interval("1900-01-01 00:00:00", "1900-01-01 00:00:00")
#       my_labor <- labor_in_year_schar(my_labor, schedule)
#       if (!all(is.na(labor$idle))) {
#         labor$idle <- c(labor$idle, my_labor$idle)
#       } else {
#         labor$idle <- my_labor$idle
#       }
#       if (!all(is.na(labor$overtime))) {
#         labor$overtime <- c(labor$overtime, my_labor$overtime)
#       } else {
#         labor$overtime <- my_labor$overtime
#       }
#       if (!all(is.na(labor$productive))) {
#         labor$productive <- c(labor$productive, my_labor$productive)
#       } else {
#         labor$productive <- my_labor$productive
#       }
#       rm(my_labor)
#     }
#     labor$idle <- unique(labor$idle[!as.numeric(
#       as.duration(labor$idle), units = "secs") == 0])
#     labor$overtime <- unique(labor$overtime[!as.numeric(
#       as.duration(labor$overtime), units = "secs") == 0])
#     labor$productive <- unique(labor$productive[!as.numeric(
#       as.duration(labor$productive), units = "secs") == 0])
#   }
#   return(labor)
# }
# # Test:
# new_labor1b <- labor_in_schar(labor_id1, vehicles[2, ])
# labor_id0$labor <- c(labor_id0$labor,
#   lubridate::int_shift(labor_id0$labor, lubridate::ddays(380)))
# new_labor0 <- labor_in_schar(labor_id0, vehicles[1, ])

