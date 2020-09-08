
context("bfactor_log_interpret Jeffreys")

test_that("bfactor_log_interpret test 1", {
  expect_equal(
    bfactor_log_interpret(
      c(
        -3.10,
        -1.78,
        1.06,
        -1.40,
        1.21,
        0.89,
        -2.37,
        1.23,
        -8.88,
        3.81,
        -8.38,
        0.62),
      base = 10),
    c(
      "Negative",
      "Negative",
      "Strong",
      "Negative",
      "Strong",
      "Substantial",
      "Negative",
      "Strong",
      "Negative",
      "Decisive",
      "Negative",
      "Substantial"
    )
  )
})

test_that("bfactor_log_interpret test 2", {
  expect_equal(
    bfactor_log_interpret(
      c(0.07, 1.29, 1.32, -0.62, -1.78, 1.55, -3.02, 1.25, 1.48),
       base = 10),
    c(
      "Weak",
      "Strong",
      "Strong",
      "Negative",
      "Negative",
      "Very Strong",
      "Negative",
      "Strong",
      "Strong"
    )
  )
})

test_that("bfactor_log_interpret test 3", {
    expect_equal(
    bfactor_log_interpret(
      c(0.07, 1.29, 1.32, -0.62, -1.78, 1.55, -3.02, 1.25, 1.48),
      base = 10),
    bfactor_log_interpret(
      c(0.07, 1.29, 1.32, -0.62, -1.78, 1.55, -3.02, 1.25, 1.48),
      base = 10,
      scale = "jeffreys")
  )
})

test_that("bfactor_log_interpret test 4", {
  expect_error(
    expect_equal(
      bfactor_log_interpret(
        c(0.07, 1.29, 1.32, -0.62, -1.78, 1.55, -3.02, 1.25, 1.48),
        base = 10,
        scale = "kass-raftery"),
      bfactor_log_interpret(
        c(0.07, 1.29, 1.32, -0.62, -1.78, 1.55, -3.02, 1.25, 1.48),
        base = 10,
        scale = "jeffreys")
      )
    )
})

test_that("bfactor_log_interpret error message 1", {
  expect_error(
    bfactor_log_interpret(NULL)
  )}
)

test_that("bfactor_log_interpret error message 2 - empty vector", {
  expect_error(
    bfactor_log_interpret(vector())
  )}
)

test_that("bfactor_log_interpret error message 2 - empty list", {
  expect_error(
    bfactor_log_interpret(list())
  )}
)

test_that("bfactor_log_interpret error message 3 - NA", {
  expect_error(
    bfactor_log_interpret(NA)
  )}
)

test_that("bfactor_log_interpret error message 3 - NaN", {
  expect_error(
    bfactor_log_interpret(NaN)
  )}
)

test_that("bfactor_log_interpret error message 4 - factor", {
  expect_error(
    bfactor_log_interpret(factor(10))
  )}
)

test_that("bfactor_log_interpret error message 4 - char", {
  expect_error(
    bfactor_log_interpret("10")
  )}
)

test_that("bfactor_log_interpret error message 4 - list", {
  expect_error(
    bfactor_log_interpret(list(10))
  )}
)

test_that("bfactor_log_interpret base arg NA warning", {
  expect_warning(
    bfactor_log_interpret(c(10, NA))
  )}
)

test_that("bfactor_log_interpret base arg error message 1", {
  expect_error(
    bfactor_log_interpret(bf = .2, base = NULL)
    )
  }
)

test_that("bfactor_log_interpret base arg error message 2", {
  expect_error(
    bfactor_log_interpret(bf = .2, base = NA)
  )}
)

test_that("bfactor_log_interpret base arg error message 3", {
  expect_error(
    bfactor_log_interpret(bf = .2, base = c(10, 10))
  )}
)

test_that("bfactor_log_interpret base arg error message 4", {
  expect_error(
    bfactor_log_interpret(bf = .2, base = factor(10))
  )}
)

test_that("bfactor_log_interpret base arg error message 5", {
  expect_error(
    bfactor_log_interpret(bf = .2, base = "10")
  )}
)

context("bfactor_log_interpret K-R")

test_that("bfactor_log_interpret error message 1", {
  expect_error(
    bfactor_log_interpret(NULL, scale = "kass-raftery")
  )}
)

test_that("bfactor_log_interpret error message 2 - empty vector", {
  expect_error(
    bfactor_log_interpret(vector(), scale = "kass-raftery")
  )}
)

test_that("bfactor_log_interpret error message 2 - empty list", {
  expect_error(
    bfactor_log_interpret(list(), scale = "kass-raftery")
  )}
)

test_that("bfactor_log_interpret error message 3 - NA", {
  expect_error(
    bfactor_log_interpret(NA, scale = "kass-raftery")
  )}
)

test_that("bfactor_log_interpret error message 3 - NaN", {
  expect_error(
    bfactor_log_interpret(NaN, scale = "kass-raftery")
  )}
)

test_that("bfactor_log_interpret error message 4 - factor", {
  expect_error(
    bfactor_log_interpret(factor(10), scale = "kass-raftery")
  )}
)

test_that("bfactor_log_interpret error message 4 - char", {
  expect_error(
    bfactor_log_interpret("10", scale = "kass-raftery")
  )}
)

test_that("bfactor_log_interpret error message 4 - list", {
  expect_error(
    bfactor_log_interpret(list(10), scale = "kass-raftery")
  )}
)

test_that("bfactor_log_interpret base arg NA warning", {
  expect_warning(
    bfactor_log_interpret(c(10, NA), scale = "kass-raftery")
  )}
)

test_that("bfactor_log_interpret base arg error message 1", {
  expect_error(
    bfactor_log_interpret(bf = .2, base = NULL, scale = "kass-raftery")
    )}
)

test_that("bfactor_log_interpret base arg error message 2", {
  expect_error(
    bfactor_log_interpret(bf = .2, base = NA, scale = "kass-raftery")
  )}
)

test_that("bfactor_log_interpret base arg error message 3", {
  expect_error(
    bfactor_log_interpret(bf = .2, base = c(10, 10), scale = "kass-raftery")
  )}
)

test_that("bfactor_log_interpret base arg error message 4", {
  expect_error(
    bfactor_log_interpret(bf = .2, base = factor(10), scale = "kass-raftery")
  )}
)

test_that("bfactor_log_interpret base arg error message 5", {
  expect_error(
    bfactor_log_interpret(bf = .2, base = "10", scale = "kass-raftery")
  )}
)

context("test scale")

test_that("test scale 1", {
  expect_equal(
    bfactor_log_interpret(1.23, scale = "kass-raftery"),
    bfactor_log_interpret(1.23, scale = "kass-RAftery")
  )}
)

test_that("test scale 2", {
  expect_equal(
    bfactor_log_interpret(1.23, scale = "Kass-raftery"),
    bfactor_log_interpret(1.23, scale = "Kass-Raftery")
  )}
)

test_that("test scale 3", {
  expect_equal(
    bfactor_log_interpret(1.23, scale = "kass-raftery"),
    bfactor_log_interpret(1.23, scale = "Kass-Raftery")
  )}
)

test_that("test scale 4", {
  expect_equal(
    bfactor_log_interpret(1.23, scale = "Jeffreys"),
    bfactor_log_interpret(1.23, scale = "jeffreys")
  )}
)

test_that("test scale 5", {
  expect_error(
    bfactor_log_interpret(1.23, scale = NULL)
  )}
)

test_that("test scale 6", {
  expect_error(
    bfactor_log_interpret(1.23, scale = NA)
  )}
)

test_that("test scale 7", {
  expect_error(
    bfactor_log_interpret(1.23, scale = NaN)
  )}
)

test_that("test scale 8", {
  expect_error(
    bfactor_log_interpret(1.23, scale = 1)
  )}
)


test_that("test scale 9", {
  expect_error(
    bfactor_log_interpret(1.23, scale = "scale")
  )}
)

test_that("test scale 10", {
  expect_error(
    bfactor_log_interpret(1.23, scale = character())
  )}
)


test_that("test scale 11", {
  expect_error(
    bfactor_log_interpret(1.23, scale = "")
  )}
)

test_that("test scale 12", {
  expect_error(
    bfactor_log_interpret(1.23, scale = c("jeffreys", "kass-raftery"))
  )}
)


test_that("test scale 13", {
  expect_error(
    bfactor_log_interpret(1.23, scale = "")
  )}
)

test_that("test scale 14", {
  expect_error(
    bfactor_log_interpret(1.23, scale = list())
  )}
)

test_that("test scale 15", {
  expect_error(
    bfactor_log_interpret(1.23, scale = list("jeffreys"))
  )}
)

test_that("test scale 16", {
  expect_error(
    bfactor_log_interpret(1.23, scale = factor("jeffreys"))
  )}
)

test_that("test scale 17", {
  expect_error(
    bfactor_log_interpret(1.23, base = 10, scale = 1)
  )}
)


test_that("test scale 18", {
  expect_error(
    bfactor_log_interpret(1.23, base = 10, scale = "scale")
  )}
)

test_that("test scale 19", {
  expect_error(
    bfactor_interpret(1.23, scale = NULL)
  )}
)

test_that("test scale 20", {
  expect_error(
    bfactor_interpret(1.23, scale = NA)
  )}
)

test_that("test scale 21", {
  expect_error(
    bfactor_interpret(1.23, scale = 1)
  )}
)


test_that("test scale 22", {
  expect_error(
    bfactor_interpret(1.23, scale = "scale")
  )}
)

test_that("test scale 23", {
  expect_error(
    bfactor_interpret(1.23, scale = character())
  )}
)


test_that("test scale 24", {
  expect_error(
    bfactor_interpret(1.23, scale = "")
  )}
)

test_that("test scale 25", {
  expect_error(
    bfactor_interpret(1.23, scale = c("jeffreys", "kass-raftery"))
  )}
)