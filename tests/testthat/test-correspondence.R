
context("correspondence between bcal and pcal")

test_that("test 1", {
  expect_equal(
    pcal(.1),
    bfactor_to_prob(bcal(.1))
  )
})

test_that("test 2", {
  expect_equal(
    pcal(seq(0, 1, .05)),
    bfactor_to_prob(bcal(seq(0, 1, .05)))
  )
})

test_that("test 3", {
  expect_equal(
    pcal(seq(0, 1, .05), seq(1, 0, -.05)),
    bfactor_to_prob(bcal(seq(0, 1, .05)), seq(1, 0, -.05))
  )
})

test_that(" test 4", {
  expect_equal(
    pcal(.1, seq(1, 0, -.05)),
    bfactor_to_prob(bcal(.1), seq(1, 0, -.05))
  )
})


