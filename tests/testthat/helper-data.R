# Helper file. Set global test parameters available in all testthat sections

origin_date = lubridate::ymd_hms("2016-01-01 00:00:00")

schedule <- data.frame(
  id = c(0, 1, 911),
  shift_date = c("Jan-01 - Dec-31", "Jan-01 - Jun-30", "Jul-06 - Dec-31"),
  shift_time = c("00:00 - 23:59", "12:00 - 18:00", "09:00 - 19:00"),
  shift_weekday = c("Mon, Tues, Wed, Thurs, Fri, Sat, Sun", "Mon, Tues, Wed, Thurs, Fri", "Sat, Sun"),
  stringsAsFactors = F)

vehicles0 <- data.frame(
  id = c(0,1,911),
  year = rep(0, 3),
  busy_until = c(0,0,0), # "2016-01-01 08:20:00 UTC", "Fri"
  shift_from_simdate = c(0, 0, 0), # 186 not calculated.                         Depends on year! Wrong like this
  shift_to_simdate = c(0, 0, 0), # Not calculated.                               Depends on year! Wrong like this
  shift_from_simtime = c(0, 0, 0), # In most cases correct
  shift_to_simtime = c(0, 0, 0),
  shift_weekday = c("Mon, Tues, Wed, Thurs, Fri, Sat, Sun", "Mon, Tues, Wed, Thurs, Fri", "Sat, Sun"),
  # shift_weekday = c("Mo, Di, Mi, Do, Fr, Sa, So", "Mo, Di, Mi, Do, Fr", "Sa, So"),
  lat = c(47.1, 47.3,46),
  lng = c(7.9, 8.1,8),
  schedule = c(paste(schedule[1,], collapse = "| "), paste(schedule[2,], collapse = "| "),
    paste(schedule[3,], collapse = "| ")),
  stringsAsFactors = F)

vehicles <- data.frame(
  id = c(0,1,911),
  year = rep(0, 3),
  busy_until = c(3e4,0,0), # "2016-01-01 08:20:00 UTC", "Fri"
  shift_from_simdate = c(0, 0, 186), # 186 not calculated.                       Depends on year! Wrong like this
  shift_to_simdate = c(365, 180, 365), # Not calculated.                         Depends on year! Wrong like this
  shift_from_simtime = c(0, 12*3600, 9*3600), # In most cases correct
  shift_to_simtime = c(24*3600, 18*3600, 19*3600),
  # shift_weekday = c("Mon, Tues, Wed, Thurs, Fri, Sat, Sun", "Mon, Tues, Wed, Thurs, Fri", "Sat, Sun"),
  shift_weekday = c("Mo, Di, Mi, Do, Fr, Sa, So", "Mo, Di, Mi, Do, Fr", "Sa, So"),
  lat = c(47.1, 47.3,46),
  lng = c(7.9, 8.1,8), stringsAsFactors = F)

test_date <- lubridate::ymd_hms("2016-06-16 14:16:53 CEST") # => "Thurs"

