test_that("simTime() delivers the right time", {
  expect_equal(simTime(0), 0)
  expect_equal(simTime(24*60*60), 0)
  expect_equal(simTime(365*24*60*60 - 1), 24*60*60 - 1)
})

test_that("simDate() delivers the right date", {
  expect_equal(simDate(0), 0)
  expect_equal(simDate(24*60*60), 1)
  expect_equal(simDate(365*24*60*60 - 1), 364)
})


test_that("as.charTime() works", {
  expect_equal(as.charTime(26400), "07:20")
  expect_equal(as.charTime(68700), "19:05")
})



test_that("as.simTime() works", {
  expect_equal(as.simTime("2014-01-01 00:00:00 CET"), 0)
  expect_equal(as.simTime("2014-01-02 00:00:00 CET"), 0)
  expect_equal(as.simTime("2014-12-31 23:59:59 CET"), (24*60*60 - 1))
})


test_that("as.simDate() works", {
  expect_equal(as.simDate("2014-01-01 00:00:00 CET"), 0)
  expect_equal(as.simDate("2014-01-02 00:00:00 CET"), 1)
  expect_equal(as.simDate("2014-12-31 23:59:59 CET"), 364)
})

test_that("as.simDateTime() works", {
  expect_equal(as.simDateTime("2014-01-01 00:00:00 CET"), 0)
  expect_equal(as.simDateTime("2014-01-02 00:00:00 CET"), 24*60*60)
  expect_equal(as.simDateTime("2014-12-31 23:59:59 CET"), 365*24*60*60-1)
})

test_that("simWeekday() works", {
  expect_equal(simWeekday(as.simDateTime("2014-01-01 00:00:00 CET")), "Mi")
  expect_equal(simWeekday(as.simDateTime("2014-01-02 00:00:00 CET")), "Do")
  expect_equal(simWeekday(as.simDateTime("2014-12-31 23:59:59 CET")), "Mi")
})
