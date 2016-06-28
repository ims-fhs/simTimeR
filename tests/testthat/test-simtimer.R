context("Synthetic data dummy")
test_that("Synthetic data dummy", {
  expect_equal(vehicles$id[1], 0)
})

context("basic functions")
test_that("as.simTime() works", {
  expect_equal(as.simTime("2014-01-01 00:00:00 CET"), 0)
  expect_equal(as.simTime("2014-01-02 00:00:00 CET"), 0)
  expect_equal(as.simTime("2014-12-31 23:59:59 CET"), (24*60*60 - 1))
})

test_that("as.simDate() works", {
  expect_equal(as.simDate("2014-01-01 00:00:00 CET"), 0)
  expect_equal(as.simDate("2014-01-02 00:00:00 CET"), 1)
  expect_equal(as.simDate("2014-12-31 23:59:59 CET"), 364)

  #Possible implementation:
  expect_equal(as.simDate("2014-01-01 00:00:00 CET"),
    strtoi(format(as.POSIXct("2014-01-01 00:00:00 CET"), "%j")) - 1)
  expect_equal(as.simDate("2014-01-02 00:00:00 CET"),
    strtoi(format(as.POSIXct("2014-01-02 00:00:00 CET"), "%j")) - 1)
  expect_equal(as.simDate("2014-12-31 23:59:59 CET"),
    strtoi(format(as.POSIXct("2014-12-31 23:59:59 CET"), "%j")) - 1)

  # Expect warning as implementation with POSIXct possible:
  # expect_warning(as.simDate("2014-01-01 00:00:00 CET"), )
})

test_that("as.simDateTime() works", {
  date_t0 <- as.POSIXct("2014-01-01 00:00:00 CET")
  dt1 <- as.numeric(as.POSIXct("2014-01-01 00:00:00 CET") - date_t0, units = "secs")
  expect_equal(as.simDateTime("2014-01-01 00:00:00 CET"), dt1)

  dt2 <- as.numeric(as.POSIXct("2014-01-02 00:00:00 CET") - date_t0, units = "secs")
  expect_equal(as.simDateTime("2014-01-02 00:00:00 CET"), dt2)

  dt3 <- as.numeric(as.POSIXct("2014-12-31 23:59:59 CET") - date_t0, units = "secs")
  expect_equal(as.simDateTime("2014-12-31 23:59:59 CET"), dt3)

  # Expect warning as implementation with POSIXct possible:
  # expect_warning(as.simDateTime("2014-01-01 00:00:00 CET"), )
})

test_that("as.charTime() delivers the right time as str", {
  expect_equal(as.charTime(26400), "07:20")
  expect_equal(as.charTime(68700), "19:05")

  #Possible implementation:
  expect_equal(as.charTime(26400),
    format(as.POSIXct("2014-01-01 00:00:00 CET") + 26400, "%H:%M"))
  expect_equal(as.charTime(68700),
    format(as.POSIXct("2014-01-01 00:00:00 CET") + 68700, "%H:%M"))

  # Expect warning as implementation with POSIXct possible:
  # expect_warning(as.charTime(26400), )
})

test_that("simDate() delivers the right day of the year", {
  expect_equal(simDate(0), 0)
  expect_equal(simDate(24*60*60), 1)
  expect_equal(simDate(365*24*60*60 - 1), 364)

  #Possible implementation:
  expect_equal(simDate(0),
    strtoi(format(as.POSIXct("2014-01-01 00:00:00 CET") + 0, "%j"))-1)
  expect_equal(simDate(24*60*60),
    strtoi(format(as.POSIXct("2014-01-01 00:00:00 CET") + 24*60*60, "%j"))-1)
  expect_equal(simDate(365*24*60*60),
    strtoi(format(as.POSIXct("2014-01-01 00:00:00 CET") + 365*24*60*60-1, "%j"))) #.....?

  # Expect warning as implementation with POSIXct possible:
  # expect_warning(simDate(0), )
})

test_that("simTime() delivers the right number of seconds of the day", {
  expect_equal(simTime(0), 0)
  expect_equal(simTime(24*60*60), 0)
  expect_equal(simTime(365*24*60*60 - 1), 24*60*60 - 1)
})

test_that("simWeekday() works", {
  expect_equal(simWeekday(as.simDateTime("2014-01-01 00:00:00 CET")), "Mi")
  expect_equal(simWeekday(as.simDateTime("2014-01-02 00:00:00 CET")), "Do")
  expect_equal(simWeekday(as.simDateTime("2014-12-31 23:59:59 CET")), "Mi")

  #Possible implementation:
  expect_equal(simWeekday(as.simDateTime("2014-01-01 00:00:00 CET")),
    format(as.POSIXct("2014-01-01 00:00:00 CET"), "%a"))
  expect_equal(simWeekday(as.simDateTime("2014-01-02 00:00:00 CET")),
    format(as.POSIXct("2014-01-02 00:00:00 CET"), "%a"))
  expect_equal(simWeekday(as.simDateTime("2014-12-31 23:59:59 CET")),
    format(as.POSIXct("2014-12-31 23:59:59 CET"), "%a"))
})

