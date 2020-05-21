
testthat::context("pcal")

testthat::test_that("pcal: 0.05 and 0.01", {
  testthat::expect_equal(
      round(pcal(c(.05, .01)), 2),
      c(.29, .11)
      )
  }
)

testthat::test_that("pcal 0.001", {
  testthat::expect_equal(
    round(pcal(.001), 4),
    0.0184
  )
}
)

testthat::test_that("pcal table", {
  testthat::expect_equal(
    round(pcal(c(.2, .1, .05, .01, .005)), 3),
    c(0.467, 0.385, 0.289, 0.111, 0.067)
  )
}
)

testthat::test_that("pcal bl1", {
  testthat::expect_equal(
    round(pcal(sapply(lapply(datalist_bl1, chisq_test_multinomial, categories = 1:9, null_par = theta_benford(1)), FUN = "[[", "p.value")), 3),
    c(0.000, 0.000, 0.000, 0.000, 0.002, 0.002, 0.000, 0.002, 0.000, 0.105, 0.000, 0.002)
  )
}
)

testthat::test_that("pcal bl2", {
  testthat::expect_equal(
    round(pcal(sapply(lapply(datalist_bl2, chisq_test_multinomial, categories = 0:9, null_par = theta_benford(2)), FUN = "[[", "p.value")), 3),
    c(0.357, 0.481, 0.331, 0.069, 0.453, 0.152, 0.500, 0.346, 0.089, 0.313, 0.461, 0.500)
  )
}
)















