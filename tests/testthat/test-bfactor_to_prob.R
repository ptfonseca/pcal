
context("bfactor_to_prob - function")

test_that("bfactor_to_prob test 1", {
  expect_equal(
    mapply(bfactor_to_prob, 1, prior_prob = seq(0, 1, .1)),
    c(0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
  )
})

test_that("bfactor_to_prob test 2", {
  expect_equal(
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
})

test_that("bfactor_to_prob test 3", {
  expect_equal(
    bfactor_to_prob(c(1, 1, 1, 1, 1), prior_prob = seq(.1, .5, .1)),
    c(
      bfactor_to_prob(1, .1),
      bfactor_to_prob(1, .2),
      bfactor_to_prob(1, .3),
      bfactor_to_prob(1, .4),
      bfactor_to_prob(1, .5)
    )
  )
})

test_that("bfactor_to_prob test 4", {
  expect_equal(
   bfactor_to_prob(c(1, 1, 1, 1, 1), prior_prob = seq(.1, .5, .1)),
   seq(.1, .5, .1)
  )
})

test_that("bfactor_to_prob test 5", {
  expect_equal(
    bfactor_to_prob(1, c(.1, .2, .3)),
    mapply(bfactor_to_prob, 1, prior_prob = c(.1, .2, .3))
  )
})

test_that("bfactor_to_prob test 6", {
  expect_equal(
    bfactor_to_prob(1, c(.1, .2, .3)),
    c(0.1, 0.2, 0.3)
  )
})

test_that("bfactor_to_prob test 7", {
  expect_equal(
    bfactor_to_prob(1, c(.1, .2, .3)),
    seq(.1, .3, .1)
  )
})

test_that("bfactor_to_prob w/ NA", {
  expect_equal(
    suppressWarnings(round(bfactor_to_prob(c(1, NA, 2)), 2)),
    c(0.50, NA, 0.67)
  )
})

test_that("bfactor_to_prob with NaN", {
  expect_equal(
    suppressWarnings(round(bfactor_to_prob(c(1, NaN, 2)), 2)),
    c(0.50, NaN, 0.67)
  )
})

context("bfactor_to_prob - 'bf' error and warning messages")

test_that("bfactor_to_prob - bf w/ NA", {
  expect_warning(
    bfactor_to_prob(c(1, NA, 2)),
    "There are NA or NaN values in bf."
    )
})

test_that("bfactor_to_prob - bf w/ NaN", {
  expect_warning(
    bfactor_to_prob(c(1, NaN, 2)),
    "There are NA or NaN values in bf."
    )
})

test_that("bfactor_to_prob - bf w/ NULL", {
  expect_error(
    bfactor_to_prob(NULL),
    "Invalid argument: bf is NULL."
    )
})

test_that("bfactor_to_prob - empty bf vector", {
  expect_error(
    bfactor_to_prob(vector()),
    "Invalid argument: bf is empty."
    )
})

test_that("bfactor_to_prob - empty bf list", {
  expect_error(
    bfactor_to_prob(list()),
    "Invalid argument: bf is empty."
    )
})

test_that("bfactor_to_prob - empty bf factor", {
  expect_error(
    bfactor_to_prob(factor()),
    "Invalid argument: bf is empty."
    )
})

test_that("bfactor_to_prob empty bf character", {
  expect_error(
    bfactor_to_prob(character()),
    "Invalid argument: bf is empty."
    )
})

test_that("bfactor_to_prob - bf w/ NA 1", {
  expect_error(
    bfactor_to_prob(NA),
    "all elements of bf are NA or NaN."
    )
})

test_that("bfactor_to_prob - bf w/ NA 2", {
  expect_error(
    bfactor_to_prob(c(NA, NA)),
    "all elements of bf are NA or NaN."
  )
})

test_that("bfactor_to_prob - bf NaN 1", {
  expect_error(
    bfactor_to_prob(NaN),
    "all elements of bf are NA or NaN."
  )
})

test_that("bfactor_to_prob - bf NaN 2", {
  expect_error(
    bfactor_to_prob(c(NaN, NaN)),
    "all elements of bf are NA or NaN."
    )
})

test_that("bfactor_to_prob - bf w/ factor", {
  expect_error(
    bfactor_to_prob(factor(1)),
    "Invalid argument: bf must be an atomic vector."
    )
})

test_that("bfactor_to_prob bf w/ character", {
  expect_error(
    bfactor_to_prob("1"),
    "Invalid argument: bf must be numeric."
    )
})

test_that("bfactor_to_prob - bf < 0 1", {
  expect_error(
    bfactor_to_prob(-1),
    "all elements of bf must be non-negative."
    )
})

test_that("bfactor_to_prob - bf < 0 2", {
  expect_error(
    bfactor_to_prob(-0.001),
    "all elements of bf must be non-negative."
    )
})

test_that("bfactor_to_prob - bf w/ NA warning", {
  expect_warning(
    bfactor_to_prob(c(.1, NA)),
    "There are NA or NaN values in bf."
    )
})

context("bfactor_to_prob - 'prior_prob' error and warning messages")

test_that("bfactor_to_prob - prior_prob w/ NULL", {
  expect_error(
    bfactor_to_prob(.1, NULL),
    "Invalid argument: prior_prob is NULL."
    )
})

test_that("bfactor_to_prob - empty prior_prob vector", {
  expect_error(
    bfactor_to_prob(.1, vector()),
    "Invalid argument: prior_prob is empty."
    )
})

test_that("bfactor_to_prob  - empty prior_prob list", {
  expect_error(
    bfactor_to_prob(.1, list()),
    "Invalid argument: prior_prob is empty."
    )
})

test_that("bfactor_to_prob - empty prior factor", {
  expect_error(
    bfactor_to_prob(.1, factor()),
    "Invalid argument: prior_prob is empty."
    )
})

test_that("bfactor_to_prob - prior_prob w/ NA", {
  expect_warning(
    bfactor_to_prob(c(.1, .2), c(.1, NA)),
    "There are NA or NaN values in prior_prob."
    )
})

test_that("bfactor_to_prob - prior_prob w/ NaN", {
  expect_warning(
    bfactor_to_prob(c(.1, .2), c(.1, NaN)),
    "There are NA or NaN values in prior_prob."
    )
})

test_that("bfactor_to_prob - prior_prob w/ character", {
  expect_error(
    bfactor_to_prob(c(.1, .2), c(".1")),
    "Invalid argument: prior_prob must be numeric."
    )
})

test_that("bfactor_to_prob - prior_prob w/ factor", {
  expect_error(
    bfactor_to_prob(c(.1, .2), factor(.1)),
    "Invalid argument: prior_prob must be an atomic vector."
    )
})

test_that("bfactor_to_prob  - prior_prob < 0", {
  expect_error(
    bfactor_to_prob(c(.1, .2), -.1),
    "all elements of prior_prob must be in the \\[0, 1\\] interval."
    )
})

test_that("bfactor_to_prob  - prior_prob > 1", {
  expect_error(
    bfactor_to_prob(c(.1, .2), 1.1),
    "all elements of prior_prob must be in the \\[0, 1\\] interval."
    )
})

test_that("bfactor_to_prob  - prior_prob w/ only NA", {
  expect_error(
    bfactor_to_prob(c(.1, .2), NA),
    "Invalid argument: All elements of prior_prob are NA or NaN."
  )
})

context("bfactor_to_prob - miscellaneous")

test_that("bfactor_to_prob - length(bf) vs length(prior_prob) 1", {
  expect_error(
    bfactor_to_prob(c(.1, .2, .3), c(.1, .1))
    )
})

test_that("bfactor_to_prob -  length(bf) vs length(prior_prob) 2", {
  expect_error(
    bfactor_to_prob(c(.1, .2), c(.1, .2, .3))
    )
})

test_that("bfactor_to_prob -  length(bf) vs length(prior_prob) 3", {
  expect_error(
    bfactor_to_prob(c(1, 2), c(.1, .2, .3))
    )
})