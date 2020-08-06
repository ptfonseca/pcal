
context("check_prob")

test_that("check silent 1", {expect_silent(check_prob(0))})
test_that("check silent 2", {expect_silent(check_prob(1))})
test_that("check silent 3", {expect_silent(check_prob(c(0.1, 0.2, 0.3, 0.4, 0.5)))})

test_that("check warning", {expect_warning(check_prob(c(0.1, 0.2, NA, 0.4, 0.5)))})

test_that("check error message 1", {expect_error(check_prob(check_prob(NULL)))})
test_that("check error message 2", {expect_error(check_prob(TRUE))})
test_that("check error message 3", {expect_error(check_prob("0.5"))})
test_that("check error message 4", {expect_error(check_prob(factor(0.5)))})
test_that("check error message 5", {expect_error(check_prob(matrix(0.5)))})
test_that("check error message 6", {expect_error(check_prob(list(0.5)))})
test_that("check error message 7", {expect_error(check_prob(NA))})
test_that("check error message 8", {expect_error(check_prob(NaN))})
test_that("check error message 9", {expect_error(check_prob(1.1))})
test_that("check error message 10", {expect_error(check_prob(-0.5))})
test_that("check error message 11", {expect_error(check_prob(c(-0.9, 0, 0.1, 0.2, 0.3, 0.4, 0.5)))})
test_that("check error message 12", {expect_error(check_prob())})


