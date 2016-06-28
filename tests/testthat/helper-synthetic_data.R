# helper-synthetic_data for synthetic data like data.frames, ... .................. Clean up everything
# Use helper file to set global test parameters available in all testthat sections

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

vehicles <- data.frame(
  id = c(0,1,911),
  update = rep(0, 3), # update => Can be year, month, ...
  busy_until = c(0,0,0), # "2016-01-01 08:20:00 UTC", "Fri"
  shift_from_simdate = c(0, 0, 0), # 186 not calculated.
  shift_to_simdate = c(0, 0, 0), # Not calculated.
  shift_from_simtime = c(0, 0, 0), # In most cases correct
  shift_to_simtime = c(0, 0, 0),
  shift_weekday = character(3),
  schedule = c( # Needs wrapper 24:00 = 00:00 next day
    "Jan-01--Dec-31|00:00--24:00|Mon,Tues,Wed,Thurs,Fri,Sat,Sun", # ............ 24:00 works?
    "Jan-01--Jun-30|12:00--18:00|Mon,Tues,Wed,Thurs,Fri",
    "Jul-06--Dec-31|09:00--19:00|Sat,Sun"),
  stringsAsFactors = F)

missions <- data.frame(
  t_alarm_sec = c(1245, 1405, 3922, 6117, 8611, 8840, 10022, 11764, 12724, 13716),
  dt_to_completion = c(1759, 3221, 1224, 2336, 1898, 2016, 3518, 1430, 1824, 1082),
  vehicle_id = c(1, 0, 1, 911, 0, 1, 911, 1, 0, 911),
  stringsAsFactors = F)

labor0 <- data.frame(
  t = 0,
  dt = 365*24*3600,
  idle = 0,
  overtime = 0,
  stringsAsFactors = F)

