
testthat::context("bcal")

testthat::test_that("bcal table", {
  testthat::expect_equal(
    round(bcal(c(0.2, 0.1, 0.05, 0.01)), 3),
    c(0.875, 0.626, 0.407, 0.125)            ## c(0.870, 0.626, 0.407, 0.125)
  )
}
)

testthat::test_that("bcal 0.005", {
  testthat::expect_equal(
    round(bcal(0.005), 3),
    0.072
  )
}
)

testthat::test_that("bcal 0.001", {
  testthat::expect_equal(
    round(bcal(0.001), 4),
    0.0188
  )
}
)
