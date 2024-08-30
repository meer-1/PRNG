
library(testthat)
library(PRNG)
library(nortest)  # For additional normality tests

# Uniformity Test
test_that("Uniformity Test", {
  random_numbers <- prunf(n = 100, Time = FALSE)
  ks_result <- ks.test(random_numbers, "punif", 0, 1)
  expect_true(ks_result$p.value > 0.05, "Generated random numbers do not follow a uniform distribution")
})

# Independence Test
test_that("Independence Test", {
  random_numbers <- prunf(N = 1000,Time = FALSE)
  autocorr_result <- acf(random_numbers, plot = FALSE)
  expect_true(all(abs(autocorr_result$acf[2:10]) < 0.1), "Consecutive random numbers are not independent.")
})

# Seed Sensitivity Test
test_that("Seed Sensitivity Test", {
  seed1 <- prunf(n = 1000, x00 = 0.1, Time = FALSE)
  seed2 <- prunf(n = 1000, x00 = 0.1001, Time = FALSE)
  expect_false(identical(seed1, seed2), "Small changes in seed do not result in different sequences.")
})

# Performance Test
test_that("Performance Test", {
  start_time <- Sys.time()
  prunf(n = 100000, Time = FALSE)
  end_time <- Sys.time()
  expect_true(difftime(end_time, start_time, units = "secs") < 1, "Random number generation is too slow.")
})

test_that("Edge Cases Test", {
  expect_error(prunf(N = -10), "N must be a positive integer")
  expect_error(prunf(N = 0), "N must be a positive integer")
  expect_error(prunf(x00 = -0.1), "x00 must be in the range \\[0, 1\\]")
  expect_error(prunf(x00 = 1.1), "x00 must be in the range \\[0, 1\\]")
  expect_error(prunf(x01 = -0.1), "x01 must be in the range \\[0, 1\\]")
  expect_error(prunf(x01 = 1.1), "x01 must be in the range \\[0, 1\\]")
  expect_error(prunf(x02 = -0.1), "x02 must be in the range \\[0, 1\\]")
  expect_error(prunf(x02 = 1.1), "x02 must be in the range \\[0, 1\\]")
  expect_error(prunf(a1 = 3.4), "a1 must be in the range \\[3.5, 4\\]")
  expect_error(prunf(a1 = 4.1), "a1 must be in the range \\[3.5, 4\\]")
  expect_error(prunf(a2 = 0.4), "a2 must be >= 0.5")
})

test_that("Valid Inputs Test", {
  expect_silent(prunf(N = 10, x00 = 0.5, x01 = 0.5, x02 = 0.5, a1 = 3.8, a2 = 0.7))
})


# Helper function to run multiple normality tests and return TRUE if all pass
run_normality_tests <- function() {
  normal_numbers <- prnorm(n = 2000)
  shapiro_p <- shapiro.test(normal_numbers)$p.value
  ad_p <- ad.test(normal_numbers)$p.value  # Anderson-Darling test
  ks_p <- ks.test(normal_numbers, "pnorm", mean(normal_numbers), sd(normal_numbers))$p.value  # KS test
  
  # All p-values should be greater than 0.05 for the sample to be considered normally distributed
  all(c(shapiro_p, ad_p, ks_p) > 0.01)
}

test_that("Distribution Test - Normal", {
  # Run the test multiple times to account for randomness
  results <- replicate(100, run_normality_tests())
  
  # Check if a reasonable proportion of results are TRUE
  expect_true(mean(results) > 0.6, 
              "Generated numbers do not follow a normal distribution in the majority of tests")
})