
context("pcal")

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

test_that("pcal table", {
  expect_equal(
    round(pcal(c(.2, .1, .05, .01, .005)), 3),
    c(0.467, 0.385, 0.289, 0.111, 0.067)
  )
}
)

test_that("pcal bl1", {

  theta_bl1 <- sapply(1:9, function(x){log10(1+1/(x))})

  expect_equal(
    round(pcal(sapply(lapply(lapply(datalist_bl1, table), chisq.test, p = theta_bl1), FUN = "[[", "p.value")), 3),
    c(0.000, 0.000, 0.000, 0.000, 0.002, 0.002, 0.000, 0.002, 0.000, 0.105, 0.000, 0.002)
  )
}
)

test_that("pcal bl2", {

  theta_bl2 <- sapply(0:9, function(x){sum(log10(1+1/(10*(1:9)+x)))})

  expect_equal(
    round(pcal(sapply(lapply(lapply(datalist_bl2, table), chisq.test, p = theta_bl2), FUN = "[[", "p.value")), 3),
    c(0.357, 0.481, 0.331, 0.069, 0.453, 0.152, 0.500, 0.346, 0.089, 0.313, 0.461, 0.500)
  )
}
)

test_that("pcal w/ NULL p", {
  expect_error(
    pcal(NULL)
  )
}
)

test_that("pcal p empty vector", {
  expect_error(
    pcal(vector())
  )
}
)

test_that("pcal p empty list", {
  expect_error(
    pcal(list())
  )
}
)

test_that("pcal p w/ list", {
  expect_error(
    pcal(list(1))
  )
}
)

test_that("pcal p w/ char", {
  expect_error(
    pcal("test")
  )
}
)

test_that("pcal p w/ factor", {
  expect_error(
    pcal(factor(1))
  )
}
)

test_that("pcal p w/ p<0", {
  expect_error(
    pcal(c(.1, -.1))
  )
}
)

test_that("pcal p w/ p>1", {
  expect_error(
    pcal(c(.1, 1.1))
  )
}
)

test_that("pcal p NA warning", {
  expect_warning(
    pcal(c(.1, NA))
  )
}
)

test_that("pcal p NaN warning", {
  expect_warning(
    pcal(c(.1, NaN))
  )
}
)

test_that("pcal NULL prior_prob", {
  expect_error(
    pcal(.1, NULL)
  )
}
)

test_that("pcal prior_prob empty vector", {
  expect_error(
    pcal(.1, vector())
  )
}
)

test_that("pcal prior_prob - empty list ", {
  expect_error(
    pcal(.1, list())
  )
}
)

test_that("pcal prior_prob - NA ", {
  expect_error(
    pcal(.1, NA)
  )
}
)

test_that("pcal prior_prob - NaN ", {
  expect_error(
    pcal(.1, NaN)
  )
}
)

test_that("pcal prior_prob - char ", {
  expect_error(
    pcal(.1, "test")
  )
}
)

test_that("pcal prior_prob - factor ", {
  expect_error(
    pcal(.1, factor(.1))
  )
}
)

test_that("pcal prior_prob - list ", {
  expect_error(
    pcal(.1, list(.1))
  )
}
)

test_that("pcal prior_prob > 1 ", {
  expect_error(
    pcal(.1, 1.1)
  )
}
)

test_that("pcal prior_prob < 1 ", {
  expect_error(
    pcal(.1, -.1)
  )
}
)

test_that("pcal prior_prob length t1", {
  expect_error(
    pcal(c(.1, .1, .1, .1), c(.1, .2, .3))
  )
}
)

test_that("pcal prior_prob length t2 ", {
  expect_error(
    pcal(c(.1, .1), c(.1, .2, .3))
  )
}
)



















