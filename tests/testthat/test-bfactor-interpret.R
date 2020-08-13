
context("bfactor_interpret function - jeffreys")

test_that("bfactor_interpret test 1", {
  expect_equal(
    bfactor_interpret(10 ^ c(-3.10, -1.78, 1.06, -1.40, 1.21, 0.89, -2.37, 1.23, -8.88, 3.81, -8.38, 0.62)),
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

test_that("bfactor_interpret test 2", {
  expect_equal(
    bfactor_interpret(10 ^ c(0.07, 1.29, 1.32, -0.62, -1.78, 1.55, -3.02, 1.25, 1.48)),
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

test_that("bfactor_interpret scale", {
  expect_equal(
    bfactor_interpret(10 ^ c(-3.10, -1.78, 1.06, -1.40, 1.21, 0.89, -2.37, 1.23, -8.88, 3.81, -8.38, 0.62)),
    bfactor_interpret(10 ^ c(-3.10, -1.78, 1.06, -1.40, 1.21, 0.89, -2.37, 1.23, -8.88, 3.81, -8.38, 0.62), scale = "jeffreys")
  )
})

test_that("bfactor_interpret scale case sensitiveness", {
  expect_equal(
    bfactor_interpret(10 ^ c(-3.10, -1.78, 1.06, -1.40, 1.21, 0.89, -2.37, 1.23, -8.88, 3.81, -8.38, 0.62), scale = "Jeffreys"),
    bfactor_interpret(10 ^ c(-3.10, -1.78, 1.06, -1.40, 1.21, 0.89, -2.37, 1.23, -8.88, 3.81, -8.38, 0.62), scale = "jeffreys")
  )
})

test_that("bfactor_interpret decisive", {
  expect_equal(
    bfactor_interpret(200),
    "Decisive"
  )
})

context("bfactor_interpret error and warning messages - jeffreys")

test_that("bfactor_interpret NULL test 1", {
  expect_error(
    bfactor_interpret(NULL)
  )}
)

test_that("bfactor_interpret NULL test 2", {
  expect_error(
    bfactor_interpret(NULL, scale = "jeffreys")
  )}
)

test_that("bfactor_interpret - empty vector", {
  expect_error(
    bfactor_interpret(vector())
  )}
)

test_that("bfactor_interpret -  empty list", {
  expect_error(
    bfactor_interpret(list())
  )}
)

test_that("bfactor_interpret - NA test 1", {
  expect_error(
    bfactor_interpret(NA)
  )}
)

test_that("bfactor_interpret NA test 2", {
  expect_error(
    bfactor_interpret(NA, scale = "jeffreys")
  )}
)

test_that("bfactor_interpret NaN", {
  expect_error(
    bfactor_interpret(NaN)
  )}
)

test_that("bfactor_interpret  factor", {
  expect_error(
    bfactor_interpret(factor(10))
  )}
)

test_that("bfactor_interpret character", {
  expect_error(
    bfactor_interpret("10")
  )}
)

test_that("bfactor_interpret - list", {
  expect_error(
    bfactor_interpret(list(10))
  )}
)

test_that("bfactor_interpret - negative bf", {
  expect_error(
    bfactor_interpret(-0.6)
  )}
)

test_that("bfactor_interpret NA warning", {
  expect_warning(
    bfactor_interpret(c(10, NA))
  )}
)

context("bfactor_interpret function kass-raftery")

test_that("bfactor_interpret kass-raftery test 1", {
  expect_equal(
    bfactor_interpret(c(0, 2, 4, 21, 151), scale = "kass-raftery"),
    c("Negative", "Weak", "Positive", "Strong", "Very Strong"))
  }
)

test_that("bfactor_interpret kass-raftery test 2", {
  expect_equal(
    bfactor_interpret(c(0.99, 2.99, 19.99, 149, 1510), scale = "kass-raftery"),
    c("Negative", "Weak", "Positive", "Strong", "Very Strong"))
}
)

test_that("bfactor_interpret kass-raftery decisive", {
  expect_equal(
    bfactor_interpret(200, scale = "kass-raftery"),
    "Very Strong"
  )
})

context("bfactor_interpret error and warning messages - kass raftery")

test_that("bfactor_interpret kass-raftery NULL", {
  expect_error(
    bfactor_interpret(NULL, scale = "kass-raftery")
  )}
)

test_that("bfactor_interpret kass-raftery empty vector", {
  expect_error(
    bfactor_interpret(vector(), scale = "kass-raftery")
  )}
)

test_that("bfactor_interpret kass-raftery empty list", {
  expect_error(
    bfactor_interpret(list(), scale = "kass-raftery")
  )}
)

test_that("bfactor_interpret kass-raftery NA", {
  expect_error(
    bfactor_interpret(NA, scale = "kass-raftery")
  )}
)

test_that("bfactor_interpret kass-raftery NaN", {
  expect_error(
    bfactor_interpret(NaN, scale = "kass-raftery")
  )}
)

test_that("bfactor_interpret kass-raftery factor", {
  expect_error(
    bfactor_interpret(factor(10), scale = "kass-raftery")
  )}
)

test_that("bfactor_interpret kass-raftery character", {
  expect_error(
    bfactor_interpret("10", scale = "kass-raftery")
  )}
)

test_that("bfactor_interpret kass-raftery list", {
  expect_error(
    bfactor_interpret(list(10), scale = "kass-raftery")
  )}
)

test_that("bfactor_interpret kass-raftery negative bf", {
  expect_error(
    bfactor_interpret(-0.6, scale = "kass-raftery")
  )}
)

test_that("bfactor_interpret kass-raftery NA warning", {
  expect_warning(
    bfactor_interpret(c(10, NA), scale = "kass-raftery")
  )}
)
