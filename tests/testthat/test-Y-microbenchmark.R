context("microbenchmark")
test_that("microbenchmark test for simTimeR package.", { # ..................... Dummy!
  skip("Test only if runtime has to be checked")

  # Option for microbenchmark:
  options(microbenchmark.unit = "s")

  # ============================================================================
  # build_scenario:
#   t1 <- summary(microbenchmark::microbenchmark(
#     data911::build_scenario(sim_missions, sim_vehicles, origin_date, "root"),
#     times = 10L, unit = "ms"))$uq
#   print(paste0("build_scenario() on ", nrow(sim_missions), " missions took: ",
#     round(t1, 2), " ms."))
#   expect_equal(t1 < t_40ms, T)
#   expect_equal(t1, 40, tolerance = 1e-3)

  # print(summary(microbenchmark::microbenchmark(
  #   s_in(test_date, schedule[1, ]),
  #   test_date %s_in% schedule,
  #   is_on_duty(vehicles, test_sec),
  #   is_on_duty2(test_sec, vehicles),
  #   times = 100L, unit = "ms")))

  # Microbenchmarks
  # ==============================================================================
  # print(summary(microbenchmark::microbenchmark(
  #   g2e(c("So, Mo, Mi", "Di, Mi, Do, Fr")),
  #   e2g(var),
  #   date2simtime(test_date, origin_date),
  #   times = 1000L, unit = "us")))

  # print(summary(microbenchmark::microbenchmark(
  #   test_sec %fast_sin% vehicles,
  #   vehicles[test_sec %fast_sin% vehicles, ],
  #   my_vehicles <- vehicles[test_sec %fast_sin% vehicles, ],
  #   update_schedule(test_date, vehicles),
  #   test_date %sin% vehicles,
  #   times = 100L, unit = "us")))
  #
  # vehicles <- update_schedule(test_date, vehicles)
  # print(summary(microbenchmark::microbenchmark(
  #   is_on_duty(test_sec, old_vehicles), # needs german weekdays
  #   is_on_duty2(test_sec, old_vehicles), # needs german weekdays
  #   vehicles[int_in_sint(test_sec, vehicles), ],
  #   vehicles[test_sec %fast_sin% vehicles, ],
  #   times = 100L, unit = "us")))


})

