
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

testthat::test_that("bcal w/ NULL", {
  testthat::expect_error(
    bcal(NULL)
  )
}
)

testthat::test_that("bcal empty vector", {
  testthat::expect_error(
    bcal(vector())
  )
}
)

testthat::test_that("bcal w/ empty list", {
  testthat::expect_error(
    bcal(list())
  )
}
)

testthat::test_that("bcal w/ list", {
  testthat::expect_error(
    bcal(list(1))
  )
}
)

testthat::test_that("bcal w/ char", {
  testthat::expect_error(
    bcal("test")
  )
}
)

testthat::test_that("bcal w/ factor", {
  testthat::expect_error(
    bcal(factor(1))
  )
}
)

testthat::test_that("bcal w/ p<0", {
  testthat::expect_error(
    bcal(c(.1, -.1))
  )
}
)

testthat::test_that("bcal w/ p>1", {
  testthat::expect_error(
    bcal(c(.1, 1.1))
  )
}
)

testthat::test_that("bcal NA warning", {
  testthat::expect_warning(
    bcal(c(.1, NA))
  )
}
)

testthat::test_that("bcal NaN warning", {
  testthat::expect_warning(
    bcal(c(.1, NaN))
  )
}
)










