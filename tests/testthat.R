if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)
  library(PRNG)
  test_check("PRNG")
} else {
  message("Skipping tests because 'testthat' package is not available.")
}
