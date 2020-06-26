
context("correspondence")

testthat::test_that("", {
  expect_equal(
    pcal(.1),
    bfactor_to_prob(bcal(.1))
  )
})

testthat::test_that("", {
  expect_equal(
    pcal(seq(0, 1, .05)),
    bfactor_to_prob(bcal(seq(0, 1, .05)))
  )
})

testthat::test_that("", {
  expect_equal(
    pcal(seq(0, 1, .05), seq(1, 0, -.05)),
    bfactor_to_prob(bcal(seq(0, 1, .05)), seq(1, 0, -.05))
  )
})

testthat::test_that("", {
  expect_equal(
    pcal(.1, seq(1, 0, -.05)),
    bfactor_to_prob(bcal(.1), seq(1, 0, -.05))
  )
})


