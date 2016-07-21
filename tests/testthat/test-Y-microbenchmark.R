context("microbenchmark")
test_that("microbenchmark test for simTimeR package.", {
  # skip("Skip for development of tests")

  # Option for microbenchmark:

  t1 <- summary(microbenchmark::microbenchmark(
    sim_wday(24*60*60),
    times = 10L, unit = "us"))$uq
    print(paste0("sim_wday took: ", round(t1, 2), " us."))
  expect_equal(t1 < t_500us, T)
})

