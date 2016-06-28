context("schedule")
test_that("schedule works", {
#   expect_equal(as.simTime("2014-01-01 00:00:00 CET"), 0)
#   expect_equal(as.simTime("2014-01-02 00:00:00 CET"), 0)
#   expect_equal(as.simTime("2014-12-31 23:59:59 CET"), (24*60*60 - 1))
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

  # shift_from_simdate -1? ....................................................... ???
  # shift_to_simdate -1?
  # shift_to_simtime + 60?

  # Test schedule functions
  # ==============================================================================
  # is_on_duty(test_sec, old_vehicles)
  # test_sec %fast_sin% vehicles
  # vehicles <- update_schedule(test_date, vehicles)
  # test_sec %fast_sin% vehicles
  # test_date %sin% vehicles # = TRUE  TRUE FALSE
  # test_date2 %sin% vehicles  # = TRUE FALSE FALSE

  # i3 <- interval(lubridate::ymd_hms("2016-02-01 12:00:00"),
    #   lubridate::ymd_hms("2019-10-27 12:00:00"))
    # i4 <- interval(lubridate::ymd_hms("2016-02-01 12:00:00"),
    #   lubridate::ymd_hms("2019-01-01 00:00:00"))
    # i_change <- lubridate::int_shift(i0, lubridate::dyears(0.5)) # start/ends at 12:00!
    # yearly_intervals(i3)
    # yearly_intervals(i4)

  # new_labor1 <- labor_in_year_schar(labor_id1, vehicles[2, ])

  # new_labor1b <- labor_in_schar(labor_id1, vehicles[2, ])
  # labor_id0$labor <- c(labor_id0$labor,
  #   lubridate::int_shift(labor_id0$labor, lubridate::ddays(380)))
  # new_labor0 <- labor_in_schar(labor_id0, vehicles[1, ])

})

