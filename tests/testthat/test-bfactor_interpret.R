
testthat::test_that("bfactor_interpret test 1", {
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

testthat::test_that("bfactor_interpret test 2", {
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

testthat::test_that("bfactor_log_interpret test 1", {
  expect_equal(
    bfactor_log_interpret(c(-3.10, -1.78, 1.06, -1.40, 1.21, 0.89, -2.37, 1.23, -8.88, 3.81, -8.38, 0.62), base = 10),
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

testthat::test_that("bfactor_log_interpret test 2", {
  expect_equal(
    bfactor_log_interpret(c(0.07, 1.29, 1.32, -0.62, -1.78, 1.55, -3.02, 1.25, 1.48), base = 10),
    c(
      "Weak",
      "Strong",
      "Strong",
      "Negative",
      "Negative",
      "Strong",
      "Negative",
      "Strong",
      "Strong"
    )
  )
})

testthat::test_that("bfactor_interpret error message 1", {
  expect_error(
    bfactor_interpret(NULL)
    )}
)

testthat::test_that("bfactor_interpret error message 2 - empty vector", {
  expect_error(
    bfactor_interpret(vector())
  )}
)

testthat::test_that("bfactor_interpret error message 2 - empty list", {
  expect_error(
    bfactor_interpret(list())
  )}
)

testthat::test_that("bfactor_interpret error message 3 - NA", {
  expect_error(
    bfactor_interpret(NA)
  )}
)

testthat::test_that("bfactor_interpret error message 3 - NaN", {
  expect_error(
    bfactor_interpret(NaN)
  )}
)

testthat::test_that("bfactor_interpret error message 4 - factor", {
  expect_error(
    bfactor_interpret(factor(10))
  )}
)

testthat::test_that("bfactor_interpret error message 4 - char", {
  expect_error(
    bfactor_interpret("10")
  )}
)

testthat::test_that("bfactor_interpret error message 4 - list", {
  expect_error(
    bfactor_interpret(list(10))
  )}
)

testthat::test_that("bfactor_interpret error message 5", {
  expect_error(
    bfactor_interpret(-0.6)
  )}
)

testthat::test_that("bfactor_interpret NA warning", {
  expect_warning(
    bfactor_interpret(c(10, NA))
  )}
)

testthat::test_that("bfactor_log_interpret error message 1", {
  expect_error(
    bfactor_log_interpret(NULL)
  )}
)

testthat::test_that("bfactor_log_interpret error message 2 - empty vector", {
  expect_error(
    bfactor_log_interpret(vector())
  )}
)

testthat::test_that("bfactor_log_interpret error message 2 - empty list", {
  expect_error(
    bfactor_log_interpret(list())
  )}
)

testthat::test_that("bfactor_log_interpret error message 3 - NA", {
  expect_error(
    bfactor_log_interpret(NA)
  )}
)

testthat::test_that("bfactor_log_interpret error message 3 - NaN", {
  expect_error(
    bfactor_log_interpret(NaN)
  )}
)

testthat::test_that("bfactor_log_interpret error message 4 - factor", {
  expect_error(
    bfactor_log_interpret(factor(10))
  )}
)

testthat::test_that("bfactor_log_interpret error message 4 - char", {
  expect_error(
    bfactor_log_interpret("10")
  )}
)

testthat::test_that("bfactor_log_interpret error message 4 - list", {
  expect_error(
    bfactor_log_interpret(list(10))
  )}
)

testthat::test_that("bfactor_log_interpret base arg NA warning", {
  expect_warning(
    bfactor_log_interpret(c(10, NA))
  )}
)

testthat::test_that("bfactor_log_interpret base arg error message 1", {
  expect_error(bfactor_log_interpret(bf = .2, base = NULL))}
  )

testthat::test_that("bfactor_log_interpret base arg error message 2", {
  expect_error(
    bfactor_log_interpret(bf = .2, base = NA)
)}
)

testthat::test_that("bfactor_log_interpret base arg error message 3", {
  expect_error(
    bfactor_log_interpret(bf = .2, base = c(10, 10))
  )}
)

testthat::test_that("bfactor_log_interpret base arg error message 4", {
  expect_error(
    bfactor_log_interpret(bf = .2, base = factor(10))
  )}
)

testthat::test_that("bfactor_log_interpret base arg error message 5", {
  expect_error(
    bfactor_log_interpret(bf = .2, base = "10")
  )}
)








