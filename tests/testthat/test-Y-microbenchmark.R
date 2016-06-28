context("microbenchmark")
test_that("microbenchmark test for simTimeR package.", { # ..................... Dummy!
  # skip("Test only if runtime has to be checked")
  library(schedule)
  # Option for microbenchmark:
  options(microbenchmark.unit = "s")

#   print(summary(microbenchmark::microbenchmark(
#     schedule::g2e(c("So, Mo, Mi", "Di, Mi, Do, Fr")),
#     schedule::e2g(var),
#     schedule::date2simtime(test_date, origin_date),
#     times = 1000L, unit = "us")))

#   print(summary(microbenchmark::microbenchmark(
#     test_date2 %scheduled% vehicles,
#     date2simtime(test_date2, origin_date) %scheduled% vehicles,
#     sum(vehicles[1, ] %scheduled% test_interval0/dhours()),
#     as.character(class(vehicles[1, ] %scheduled% test_interval0)),
#     labor1 %scheduled% vehicles[1, ],
#     times = 100L, unit = "us")))

#   t1 <- summary(microbenchmark::microbenchmark(
#     data911::build_scenario(sim_missions, sim_vehicles, t_ref, "root"),
#     times = 10L, unit = "ms"))$uq
#   #   print(paste0("build_scenario() on ", nrow(sim_missions), " missions took: ",
#   #     round(t1, 2), " ms."))
#   expect_equal(t1 < t_40ms, T)
})

