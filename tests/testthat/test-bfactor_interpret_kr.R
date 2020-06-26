
context("bfactor_interpret_kr")

testthat::test_that("bfactor_interpret_kr test 1", {
  expect_equal(
    bfactor_interpret_kr(c(0, 2, 4, 21, 151)),
    c("Negative", "Weak", "Positive", "Strong", "Very Strong"))
  }
)

context("bfactor_interpret_kr")

testthat::test_that("bfactor_interpret_kr test 1", {
  expect_equal(
    bfactor_interpret_kr(c(0.99, 2.99, 19.99, 149, 1510)),
    c("Negative", "Weak", "Positive", "Strong", "Very Strong"))
}
)
