
context("bfactor_interpret_kr")

testthat::test_that("bfactor_interpret_kr test 1", {
  expect_equal(
    bfactor_interpret_kr(c(0, 2, 4, 21, 151)),
    c("Negative", "Weak", "Positive", "Strong", "Very Strong"))
  }
)

testthat::test_that("bfactor_interpret_kr test 1", {
  expect_equal(
    bfactor_interpret_kr(c(0.99, 2.99, 19.99, 149, 1510)),
    c("Negative", "Weak", "Positive", "Strong", "Very Strong"))
}
)

testthat::test_that("bfactor_interpret_kr error message 1", {
  expect_error(
    bfactor_interpret_kr(NULL)
  )}
)

testthat::test_that("bfactor_interpret_kr error message 2 - empty vector", {
  expect_error(
    bfactor_interpret_kr(vector())
  )}
)

testthat::test_that("bfactor_interpret_kr error message 2 - empty list", {
  expect_error(
    bfactor_interpret_kr(list())
  )}
)

testthat::test_that("bfactor_interpret_kr error message 3 - NA", {
  expect_error(
    bfactor_interpret_kr(NA)
  )}
)

testthat::test_that("bfactor_interpret_kr error message 3 - NaN", {
  expect_error(
    bfactor_interpret_kr(NaN)
  )}
)

testthat::test_that("bfactor_interpret_kr error message 4 - factor", {
  expect_error(
    bfactor_interpret_kr(factor(10))
  )}
)

testthat::test_that("bfactor_interpret_kr error message 4 - char", {
  expect_error(
    bfactor_interpret_kr("10")
  )}
)

testthat::test_that("bfactor_interpret_kr error message 4 - list", {
  expect_error(
    bfactor_interpret_kr(list(10))
  )}
)

testthat::test_that("bfactor_interpret_kr error message 5", {
  expect_error(
    bfactor_interpret_kr(-0.6)
  )}
)

testthat::test_that("bfactor_interpret_kr NA warning", {
  expect_warning(
    bfactor_interpret_kr(c(10, NA))
  )}
)
