
context("pcal - function")

test_that("pcal: 0.05 and 0.01", {
  expect_equal(
      round(pcal(c(.05, .01)), 2),
      c(.29, .11)
      )
  }
)

test_that("pcal 0.001", {
  expect_equal(
    round(pcal(.001), 4),
    0.0184
  )
}
)

test_that("pcal table sellke (2001)", {
  expect_equal(
    round(pcal(c(.2, .1, .05, .01, .005)), 3),
    c(0.467, 0.385, 0.289, 0.111, 0.067)
  )
}
)

test_that("pcal bl1", {
  expect_equal(
    round(pcal(sapply(lapply(lapply(datalist_bl1, table), chisq.test, p = theta_bl1), FUN = "[[", "p.value")), 3),
    c(0.000, 0.000, 0.000, 0.000, 0.002, 0.002, 0.000, 0.002, 0.000, 0.105, 0.000, 0.002)
  )
}
)

test_that("pcal bl2", {
  expect_equal(
    round(pcal(sapply(lapply(lapply(datalist_bl2, table), chisq.test, p = theta_bl2), FUN = "[[", "p.value")), 3),
    c(0.357, 0.481, 0.331, 0.069, 0.453, 0.152, 0.500, 0.346, 0.089, 0.313, 0.461, 0.500)
  )
}
)

context("pcal - p error and warning messages")

test_that("p NULL", {
  expect_error(
    pcal(NULL)
  )
}
)

test_that("NaN p", {
  expect_error(
    pcal(NA)
  )
}
)

test_that("NaN p", {
  expect_error(
    pcal(NaN)
  )
}
)

test_that("NaN & NA p", {
  expect_error(
    pcal(c(NA, NaN))
  )
}
)

test_that("NA_integer p", {
  expect_error(
    pcal(NA_integer_)
  )
}
)

test_that("p empty vector", {
  expect_error(
    pcal(vector())
  )
}
)

test_that("p empty list", {
  expect_error(
    pcal(list())
  )
}
)

test_that("p empty factor", {
  expect_error(
    pcal(factor())
  )
}
)

test_that("p empty character", {
  expect_error(
    pcal(character())
  )
}
)

test_that("p list", {
  expect_error(
    pcal(list(1))
  )
}
)

test_that("p character", {
  expect_error(
    pcal("test")
  )
}
)

test_that("p factor", {
  expect_error(
    pcal(factor(1))
  )
}
)

test_that("p < 0", {
  expect_error(
    pcal(c(.1, -.1))
  )
}
)

test_that("p > 1", {
  expect_error(
    pcal(c(.1, 1.1))
  )
}
)

test_that("NA warning", {
  expect_warning(
    pcal(c(.1, NA))
  )
}
)

test_that("NaN warning", {
  expect_warning(
    pcal(c(.1, NaN))
  )
}
)

context("pcal - prior_prob error messages")

test_that("NULL prior_prob", {
  expect_error(
    pcal(.1, NULL)
  )
}
)

test_that("prior_prob empty vector", {
  expect_error(
    pcal(.1, vector())
  )
}
)

test_that("prior_prob empty list ", {
  expect_error(
    pcal(.1, list())
  )
}
)

test_that("prior_prob empty factor ", {
  expect_error(
    pcal(.1, factor())
  )
}
)

test_that("prior_prob empty character ", {
  expect_error(
    pcal(.1, character())
  )
}
)

test_that("NA prior_prob", {
  expect_error(
    pcal(.1, NA)
  )
}
)

test_that("NaN prior_prob", {
  expect_error(
    pcal(.1, NaN)
  )
}
)

test_that("char prior_prob", {
  expect_error(
    pcal(.1, "test")
  )
}
)

test_that("factor prior_prob", {
  expect_error(
    pcal(.1, factor(.1))
  )
}
)

test_that("list prior_prob", {
  expect_error(
    pcal(.1, list(.1))
  )
}
)

test_that("prior_prob > 1 ", {
  expect_error(
    pcal(.1, 1.1)
  )
}
)

test_that("prior_prob < 1 ", {
  expect_error(
    pcal(.1, -.1)
  )
}
)

context("pcal - miscellaneous")

test_that("length(p) vs length(prior_prob) test 1", {
  expect_error(
    pcal(c(.1, .1, .1, .1), c(.1, .2, .3))
  )
}
)

test_that("length(p) vs length(prior_prob) test 2 ", {
  expect_error(
    pcal(c(.1, .1), c(.1, .2, .3))
  )
}
)



















