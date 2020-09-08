
context("bcal - function")

test_that("bcal article table", {
  expect_equal(
    round(bcal(c(0.2, 0.1, 0.05, 0.01)), 3),
    c(0.875, 0.626, 0.407, 0.125)
    )
})

test_that("bcal 0.005 threshold", {
  expect_equal(
    round(bcal(0.005), 3),
    0.072
    )
})

test_that("bcal 0.001 threshold", {
  expect_equal(
    round(bcal(0.001), 4),
    0.0188
    )
})

context("bcal - error and warning messages")

test_that("bcal w/ NULL", {
  expect_error(
    bcal(NULL),
    "Invalid argument: p is NULL."
    )
})

test_that("bcal w/ NA", {
  expect_error(
    bcal(NA),
    "Invalid argument: all elements of p are NA or NaN."
    )
})

test_that("bcal w/ empty vector", {
  expect_error(
    bcal(vector()),
    "Invalid argument: p is empty."
    )
})

test_that("bcal w/ empty char", {
  expect_error(
    bcal(character()),
    "Invalid argument: p is empty."
  )
})

test_that("bcal w/ empty factor", {
  expect_error(
    bcal(factor()),
    "Invalid argument: p is empty."
  )
})

test_that("bcal w/ empty list", {
  expect_error(
    bcal(list()),
    "Invalid argument: p is empty."
  )
})

test_that("bcal w/ char", {
  expect_error(
    bcal("1"),
    "Invalid argument: p must be numeric."
  )
})

test_that("bcal w/ factor", {
  expect_error(
    bcal(factor(1)),
    "Invalid argument: p must be an atomic vector."
  )
})

test_that("bcal w/ list", {
  expect_error(
    bcal(list(1)),
    "Invalid argument: p must be an atomic vector."
  )
})

test_that("bcal w/ p<0", {
  expect_error(
    bcal(c(.1,-.1)),
    "Invalid argument: all elements of p must be in the \\[0, 1\\] interval."
    )
})

test_that("bcal w/ p>1", {
  expect_error(
    bcal(c(.1, 1.1)),
    "Invalid argument: all elements of p must be in the \\[0, 1\\] interval."
    )
})

test_that("p NA warning", {
  expect_warning(
    bcal(c(.1, NA)),
    "There are NA or NaN values in p."
    )
})

test_that("p NaN warning", {
  expect_warning(
    bcal(c(.1, NaN)),
    "There are NA or NaN values in p."
    )
})





