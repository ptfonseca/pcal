
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

testthat::test_that("bfactor_to_prob test incorrect length error", {
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

testthat::test_that("bfactor_to_prob NULL bf", {
  expect_error(bfactor_to_prob(NULL))
})

testthat::test_that("bfactor_to_prob empty bf - vector", {
  expect_error(bfactor_to_prob(vector()))
})

testthat::test_that("bfactor_to_prob empty bf - list", {
  expect_error(bfactor_to_prob(list()))
})

testthat::test_that("bfactor_to_prob NA bf t1", {
  expect_error(bfactor_to_prob(c(NA, NA)))
})

testthat::test_that("bfactor_to_prob NA bf t2", {
  expect_error(bfactor_to_prob(NA))
})

testthat::test_that("bfactor_to_prob NaN bf t1", {
  expect_error(bfactor_to_prob(c(NaN, NaN)))
})

testthat::test_that("bfactor_to_prob NaN bf t2", {
  expect_error(bfactor_to_prob(NaN))
})

testthat::test_that("bfactor_to_prob factor bf", {
  expect_error(bfactor_to_prob(factor(1)))
})

testthat::test_that("bfactor_to_prob factor bf", {
  expect_error(bfactor_to_prob("1"))
})

testthat::test_that("bfactor_to_prob bf<0 t1", {
  expect_error(bfactor_to_prob(-1))
})

testthat::test_that("bfactor_to_prob bf<0 t2", {
  expect_error(bfactor_to_prob(-0.001))
})

testthat::test_that("bfactor_to_prob bf - NA warning", {
  expect_warning(bfactor_to_prob(c(.1, NA)))
})

testthat::test_that("bfactor_to_prob NULL prior_prob", {
  expect_error(bfactor_to_prob(.1, NULL))
})

testthat::test_that("bfactor_to_prob empty prior_prob - vector", {
  expect_error(bfactor_to_prob(.1, vector()))
})

testthat::test_that("bfactor_to_prob empty prior_prob - list", {
  expect_error(bfactor_to_prob(.1, list()))
})

testthat::test_that("bfactor_to_prob NA prior_prob", {
  expect_error(bfactor_to_prob(c(.1, .2), c(.1, NA)))
})

testthat::test_that("bfactor_to_prob NA prior_prob", {
  expect_error(bfactor_to_prob(c(.1, .2), c(.1, NaN)))
})

testthat::test_that("bfactor_to_prob char prior_prob", {
  expect_error(bfactor_to_prob(c(.1, .2), c(".1")))
})

testthat::test_that("bfactor_to_prob factor prior_prob", {
  expect_error(bfactor_to_prob(c(.1, .2), factor(.1)))
})

testthat::test_that("bfactor_to_prob prior_prob < 0", {
  expect_error(bfactor_to_prob(c(.1, .2), -.1))
})

testthat::test_that("bfactor_to_prob prior_prob > 1", {
  expect_error(bfactor_to_prob(c(.1, .2), 1.1))
})

testthat::test_that("bfactor_to_prob prior_prob > 1", {
  expect_error(bfactor_to_prob(c(.1, .2, .3), c(.1, .1)))
})

testthat::test_that("bfactor_to_prob prior_prob > 1", {
  expect_error(bfactor_to_prob(c(.1, .2), c(.1, .2, .3)))
})


















































