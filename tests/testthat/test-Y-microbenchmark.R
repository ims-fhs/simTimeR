context("microbenchmark")
test_that("microbenchmark test for simTimeR package.", {
  # skip("Test only if runtime has to be checked")

  # Option for microbenchmark:
  options(microbenchmark.unit = "s")

  # ============================================================================
  # build_scenario:
  t1 <- summary(microbenchmark::microbenchmark(
    data911::build_scenario(sim_missions, sim_vehicles, t_ref, "root"),
    times = 10L, unit = "ms"))$uq
  print(paste0("build_scenario() on ", nrow(sim_missions), " missions took: ",
    round(t1, 2), " ms."))
  expect_equal(t1 < t_40ms, T)
  expect_equal(t1, 40, tolerance = 1e-3)

  # print(summary(microbenchmark::microbenchmark(
  #   s_in(test_date, schedule[1, ]),
  #   test_date %s_in% schedule,
  #   is_on_duty(vehicles, test_sec),
  #   is_on_duty2(test_sec, vehicles),
  #   times = 100L, unit = "ms")))
})

