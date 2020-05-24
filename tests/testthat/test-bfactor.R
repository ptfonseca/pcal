
context("bfactor_to_prob")

testthat::test_that("bfactor_to_prob test 1", {
  testthat::expect_equal(
    mapply(bfactor_to_prob, 1, prior_prob = seq(0, 1, .1)),
    c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
  )
}
)

testthat::test_that("bfactor_to_prob test 2", {
  testthat::expect_equal(
    mapply(bfactor_to_prob, 1, prior_prob = seq(0, .5, .1)),
    c(
      bfactor_to_prob(1,  0),
      bfactor_to_prob(1, .1),
      bfactor_to_prob(1, .2),
      bfactor_to_prob(1, .3),
      bfactor_to_prob(1, .4),
      bfactor_to_prob(1, .5)
    )
  )
}
)

testthat::test_that("bfactor_to_prob test 3", {
  testthat::expect_equal(
    bfactor_to_prob(c(1, 1, 1, 1, 1), prior_prob = seq(.1, .5, .1)),
    c(
      bfactor_to_prob(1, .1),
      bfactor_to_prob(1, .2),
      bfactor_to_prob(1, .3),
      bfactor_to_prob(1, .4),
      bfactor_to_prob(1, .5)
    )
  )
}
)

testthat::test_that("bfactor_to_prob test 4", {
  testthat::expect_equal(
    bfactor_to_prob(c(1, 1, 1, 1, 1), prior_prob = seq(.1, .5, .1)),
   seq(.1, .5, .1)
  )
}
)

testthat::test_that("bfactor_to_prob test 5", {
  testthat::expect_equal(
    bfactor_to_prob(1, c(.1, .2, .3)),
    mapply(bfactor_to_prob, 1, prior_prob = c(.1, .2, .3))
  )
}
)

testthat::test_that("bfactor_to_prob test 6", {
  testthat::expect_equal(
    bfactor_to_prob(1, c(.1, .2, .3)),
    c(0.1, 0.2, 0.3)
  )
}
)

testthat::test_that("bfactor_to_prob test 7", {
  testthat::expect_equal(
    bfactor_to_prob(1, c(.1, .2, .3)),
    seq(.1, .3, .1)
  )
}
)

testthat::test_that("bfactor_to_prob test incorrect len error", {
  expect_error(bfactor_to_prob(c(1, 2), c(.1, .2, .3)))
})

testthat::test_that("bfactor_to_prob with NA", {
  testthat::expect_equal(
    suppressWarnings(round(bfactor_to_prob(c(1, NA, 2)), 2)),
    c(0.50, NA, 0.67)
  )
}
)

testthat::test_that("bfactor_to_prob with NaN", {
  testthat::expect_equal(
    suppressWarnings(round(bfactor_to_prob(c(1, NaN, 2)), 2)),
    c(0.50, NaN, 0.67)
  )
}
)

testthat::test_that("bfactor_to_prob NA warning", {
  expect_warning(bfactor_to_prob(c(1, NA, 2)))

})

testthat::test_that("bfactor_to_prob NaN warning", {
  expect_warning(bfactor_to_prob(c(1, NaN, 2)))
})







