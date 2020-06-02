
testthat::context("pcal")

testthat::test_that("pcal: 0.05 and 0.01", {
  testthat::expect_equal(
      round(pcal(c(.05, .01)), 2),
      c(.29, .11)
      )
  }
)

testthat::test_that("pcal 0.001", {
  testthat::expect_equal(
    round(pcal(.001), 4),
    0.0184
  )
}
)

testthat::test_that("pcal table", {
  testthat::expect_equal(
    round(pcal(c(.2, .1, .05, .01, .005)), 3),
    c(0.467, 0.385, 0.289, 0.111, 0.067)
  )
}
)

testthat::test_that("pcal bl1", {

  theta_bl1 <- sapply(1:9, function(x){log10(1+1/(x))})

  testthat::expect_equal(
    round(pcal(sapply(lapply(lapply(datalist_bl1, table), chisq.test, p = theta_bl1), FUN = "[[", "p.value")), 3),
    c(0.000, 0.000, 0.000, 0.000, 0.002, 0.002, 0.000, 0.002, 0.000, 0.105, 0.000, 0.002)
  )
}
)

testthat::test_that("pcal bl2", {

  theta_bl2 <- sapply(0:9, function(x){sum(log10(1+1/(10*(1:9)+x)))})

  testthat::expect_equal(
    round(pcal(sapply(lapply(lapply(datalist_bl2, table), chisq.test, p = theta_bl2), FUN = "[[", "p.value")), 3),
    c(0.357, 0.481, 0.331, 0.069, 0.453, 0.152, 0.500, 0.346, 0.089, 0.313, 0.461, 0.500)
  )
}
)

testthat::test_that("pcal w/ NULL p", {
  testthat::expect_error(
    pcal(NULL)
  )
}
)

testthat::test_that("pcal p empty vector", {
  testthat::expect_error(
    pcal(vector())
  )
}
)

testthat::test_that("pcal p empty list", {
  testthat::expect_error(
    pcal(list())
  )
}
)

testthat::test_that("pcal p w/ list", {
  testthat::expect_error(
    pcal(list(1))
  )
}
)

testthat::test_that("pcal p w/ char", {
  testthat::expect_error(
    pcal("test")
  )
}
)

testthat::test_that("pcal p w/ factor", {
  testthat::expect_error(
    pcal(factor(1))
  )
}
)

testthat::test_that("pcal p w/ p<0", {
  testthat::expect_error(
    pcal(c(.1, -.1))
  )
}
)

testthat::test_that("pcal p w/ p>1", {
  testthat::expect_error(
    pcal(c(.1, 1.1))
  )
}
)

testthat::test_that("pcal p NA warning", {
  testthat::expect_warning(
    pcal(c(.1, NA))
  )
}
)

testthat::test_that("pcal p NaN warning", {
  testthat::expect_warning(
    pcal(c(.1, NaN))
  )
}
)

testthat::test_that("pcal NULL prior_prob", {
  testthat::expect_error(
    pcal(.1, NULL)
  )
}
)

testthat::test_that("pcal prior_prob empty vector", {
  testthat::expect_error(
    pcal(.1, vector())
  )
}
)

testthat::test_that("pcal prior_prob - empty list ", {
  testthat::expect_error(
    pcal(.1, list())
  )
}
)

testthat::test_that("pcal prior_prob - NA ", {
  testthat::expect_error(
    pcal(.1, NA)
  )
}
)

testthat::test_that("pcal prior_prob - NaN ", {
  testthat::expect_error(
    pcal(.1, NaN)
  )
}
)

testthat::test_that("pcal prior_prob - char ", {
  testthat::expect_error(
    pcal(.1, "test")
  )
}
)

testthat::test_that("pcal prior_prob - factor ", {
  testthat::expect_error(
    pcal(.1, factor(.1))
  )
}
)

testthat::test_that("pcal prior_prob - list ", {
  testthat::expect_error(
    pcal(.1, list(.1))
  )
}
)

testthat::test_that("pcal prior_prob > 1 ", {
  testthat::expect_error(
    pcal(.1, 1.1)
  )
}
)

testthat::test_that("pcal prior_prob < 1 ", {
  testthat::expect_error(
    pcal(.1, -.1)
  )
}
)

testthat::test_that("pcal prior_prob length t1", {
  testthat::expect_error(
    pcal(c(.1, .1, .1, .1), c(.1, .2, .3))
  )
}
)

testthat::test_that("pcal prior_prob length t2 ", {
  testthat::expect_error(
    pcal(c(.1, .1), c(.1, .2, .3))
  )
}
)



















