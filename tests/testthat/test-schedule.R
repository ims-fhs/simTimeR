context("uuid-check for testdata")
test_that("uuid of testdata didn't change", {
  expect_equal(data$uuid, "ba6b7622-452d-11e6-8bc9-c94b5972ecfb")
  expect_equal(class(data$uuid), "character")
})

context("language and dictionary")
test_that("language and dictionary", {
  expect_equal(g2e("So"), "Sun")
    expect_equal(g2e(c("So", "Di")), c("Sun", "Tues"))
  expect_equal(var <- g2e(c("So, Mo, Mi", "Di, Mi, Do, Fr")),
    c("Sun, Mon, Wed", "Tues, Wed, Thurs, Fri"))
  expect_equal(e2g(g2e("So")), "So")
  expect_equal(e2g(g2e(c("So", "Di"))), c("So", "Di"))
  expect_equal(e2g(var), c("So, Mo, Mi", "Di, Mi, Do, Fr"))
  expect_equal(simtime2date(date2simtime(test_date, origin_date),
    origin_date), test_date)
})

context("test time/date in schedule")
test_that("test time/date in schedule", {
  expect_equal(as.character(weekday(test_sec, origin_date)), "Thurs")
  expect_equal(vehicles$shift_from_simdate, c(0,0,0))
  expect_equal(vehicles$shift_from_simtime, c(0,0,0))
  # browser()
  vehicles <- update_schedule(test_date, vehicles)
  expect_equal(vehicles$shift_from_simdate, c(0, 0, 187))
  expect_equal(vehicles$shift_from_simtime, c(0, 43200, 32400))
  expect_equal(is_on_duty(test_sec, vehicles), c(F, F, F))
  expect_equal(int_in_sint(test_sec, vehicles), c(T, T, F))
  expect_equal(test_sec %fast_sin% vehicles, c(T, T, F))

  expect_equal(test_sec %fast_sin% vehicles, c(T, T, F))
  expect_equal(test_date %sin% vehicles, c(T, T, F))
  expect_equal(test_date2 %sin% vehicles, c(T, F, F))
})

context("test schedule in interval")
test_that("test schedule in interval", {
  expect_equal(length(yearly_intervals(test_interval1)), 4)
  expect_equal(as.character(class(yearly_intervals(test_interval1))), "Interval")
  expect_equal(length(yearly_intervals(test_interval2)), 3)
  expect_equal(length(yearly_intervals(test_interval3)), 5)

  expect_equal(sum(schar_in_interval(vehicles[1, ], test_interval2015)/dhours()), 365*24)
  expect_equal(sum(schar_in_interval(vehicles[1, ], test_interval2016)/dhours()), 366*24) #2016 is leap year
  expect_equal(sum(schar_in_interval(vehicles[2, ], test_interval2016)/dhours()), 780)
  expect_equal(sum(schar_in_interval(vehicles[3, ], test_interval2016)/dhours()), 510)
  expect_equal(sum(schar_in_interval(vehicles[2, ], test_interval1)/dhours()), 2988) # Estimate: (3*365/2+2*30.5)*5/7*6 = 2607.857
})

context("test labor in schedule")
test_that("test labor in schedule", {

  expect_equal(length(labor1$labor), 3)
  expect_equal(labor1$overtime, NA)

  new_labor1 <- labor_in_schar(labor1, vehicles[1, ])
  expect_equal(length(new_labor1$labor), 3)
  expect_equal(length(new_labor1$overtime), 0)
  expect_equal(length(new_labor1$idle), 3)

  # It is forbidden to work before the shift starts and
  expect_error(labor_in_schar(labor2, vehicles[2, ]), )
  # labor and schedule need to overlap
  expect_error(labor_in_schar(labor3, vehicles[3, ]), "labor and schedule do not overlap")

  # Further tests needed
})

context("%scheduled%")
test_that("test %scheduled%", {
  expect_equal(test_date2 %scheduled% vehicles, c(T, F, F))
  expect_equal(date2simtime(test_date2, origin_date) %scheduled% vehicles, c(T, F, F))

  expect_equal(sum(vehicles[1, ] %scheduled% test_interval2016/dhours()), 366*24) #2016 is leap year
  expect_equal(sum(vehicles[2, ] %scheduled% test_interval2016/dhours()), 780)
  expect_equal(sum(vehicles[3, ] %scheduled% test_interval2016/dhours()), 510)
  expect_equal(length(vehicles[1, ] %scheduled% test_interval2016), 366)
  expect_equal(as.character(class(vehicles[1, ] %scheduled% test_interval2016)), "Interval")

  new_labor1 <- labor1 %scheduled% vehicles[1, ]
  expect_equal(length(new_labor1$labor), 3)
  expect_equal(length(new_labor1$overtime), 0)
  expect_equal(length(new_labor1$idle), 3)
})

context("update_schedule")
test_that("test update_schedule", {
  # skip("At the moment...")
  # browser()
  v <- update_schedule(origin_date, r_vehicles)

})
