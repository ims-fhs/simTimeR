context("microbenchmark")
test_that("microbenchmark test for simtimer package.", {
  # skip("Skip for development of tests")

  t1 <- summary(microbenchmark::microbenchmark(
    sim_wday(24*60*60),
    times = 10L, unit = "us"))$uq
  expect_equal(t1 < t_500us, T)
})

