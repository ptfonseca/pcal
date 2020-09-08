
context("pcal - function")

test_that("pcal table sellke (2001)", {
  expect_equal(
    round(pcal(c(.2, .1, .05, .01, .005)), 3),
    c(0.467, 0.385, 0.289, 0.111, 0.067)
    )
})

test_that("pcal 0.05 and 0.01 thresholds", {
  expect_equal(
      round(pcal(c(.05, .01)), 2),
      c(.29, .11)
      )
})

test_that("pcal 0.001 threshold", {
  expect_equal(
    round(pcal(.001), 4),
    0.0184
  )
})

test_that("pcal bl1", {
  expect_equal(
    round(
      pcal(
        sapply(
          lapply(
            lapply(datalist_bl1, table),
            chisq.test,
            p = theta_bl1
          ),
        FUN = "[[", "p.value")
      ),
    3
  ),
  c(
    0.000,
    0.000,
    0.000,
    0.000,
    0.002,
    0.002,
    0.000,
    0.002,
    0.000,
    0.105,
    0.000,
    0.002
   )
  )
})

test_that("pcal bl2", {
  expect_equal(
    round(
      pcal(
        sapply(
          lapply(
            lapply(datalist_bl2, table),
            chisq.test,
          p = theta_bl2),
        FUN = "[[", "p.value")),
      3
      ),
    c(
      0.357,
      0.481,
      0.331,
      0.069,
      0.453,
      0.152,
      0.500,
      0.346,
      0.089,
      0.313,
      0.461,
      0.500
    )
  )
})

context("pcal - 'p' error and warning messages")

test_that("p w/ NULL", {
  expect_error(
    pcal(NULL),
    "Invalid argument: p is NULL."
  )
})

test_that("p w/ NA", {
  expect_error(
    pcal(NA),
    "Invalid argument: all elements of p are NA or NaN."
  )
})

test_that("p w/ NaN", {
  expect_error(
    pcal(NaN),
    "Invalid argument: all elements of p are NA or NaN."
  )
})

test_that("p w/ NaN & NA", {
  expect_error(
    pcal(c(NA, NaN)),
    "Invalid argument: all elements of p are NA or NaN."
  )
})

test_that("p w/ NA_integer", {
  expect_error(
    pcal(NA_integer_),
    "Invalid argument: all elements of p are NA or NaN."
  )
})

test_that("p w/ empty vector", {
  expect_error(
    pcal(vector()),
    "Invalid argument: p is empty."
  )
})

test_that("p w/ empty list", {
  expect_error(
    pcal(list()),
    "Invalid argument: p is empty."
  )
})

test_that("p  w/empty factor", {
  expect_error(
    pcal(factor()),
    "Invalid argument: p is empty."
  )
})

test_that("p w/ empty character", {
  expect_error(
    pcal(character()),
    "Invalid argument: p is empty."
  )
})

test_that("p w/ list", {
  expect_error(
    pcal(list(1)),
    "Invalid argument: p must be an atomic vector."
  )
})

test_that("p w/ character", {
  expect_error(
    pcal("test"),
    "Invalid argument: p must be numeric."
  )
})

test_that("p w/ factor", {
  expect_error(
    pcal(factor(1)),
    "Invalid argument: p must be an atomic vector."
  )
})

test_that("p < 0", {
  expect_error(
    pcal(c(.1, -.1)),
    "Invalid argument: all elements of p must be in the \\[0, 1\\] interval."
  )
})

test_that("p > 1", {
  expect_error(
    pcal(c(.1, 1.1)),
    "Invalid argument: all elements of p must be in the \\[0, 1\\] interval."
  )
})

test_that("NA warning", {
  expect_warning(
    pcal(c(.1, NA)),
    "There are NA or NaN values in p."
  )
})

test_that("NaN warning", {
  expect_warning(
    pcal(c(.1, NaN)),
    "There are NA or NaN values in p."
  )
})

context("pcal - 'prior_prob' error messages")

test_that("prior_prob w/ NULL", {
  expect_error(
    pcal(.1, NULL),
    "Invalid argument: prior_prob is NULL."
  )
}
)

test_that("prior_prob w/ empty vector", {
  expect_error(
    pcal(.1, vector()),
    "Invalid argument: prior_prob is empty."
  )
}
)

test_that("prior_prob w/ empty list ", {
  expect_error(
    pcal(.1, list()),
    "Invalid argument: prior_prob is empty."
  )
}
)

test_that("prior_prob ew/ mpty factor ", {
  expect_error(
    pcal(.1, factor()),
    "Invalid argument: prior_prob is empty."
  )
}
)

test_that("prior_prob w/ empty character ", {
  expect_error(
    pcal(.1, character()),
    "Invalid argument: prior_prob is empty."
  )
}
)

test_that("prior_prob w/ only NA", {
  expect_error(
    pcal(.1, NA),
    "Invalid argument: All elements of prior_prob are NA or NaN."
  )
}
)

test_that("prior_prob w/ only NaN", {
  expect_error(
    pcal(.1, NaN),
    "Invalid argument: All elements of prior_prob are NA or NaN."
  )
}
)

test_that("prior_prob w/ char", {
  expect_error(
    pcal(.1, "test"),
    "Invalid argument: prior_prob must be numeric."
  )
}
)

test_that("prior_prob w/ factor", {
  expect_error(
    pcal(.1, factor(.1)),
    "Invalid argument: prior_prob must be an atomic vector."
  )
}
)

test_that("prior_prob w/ list", {
  expect_error(
    pcal(.1, list(.1)),
    "Invalid argument: prior_prob must be an atomic vector."
  )
}
)

test_that("prior_prob > 1 ", {
  expect_error(
    pcal(.1, 1.1),
    "Invalid argument:
      all elements of prior_prob must be in the \\[0, 1\\] interval."
  )
}
)

test_that("prior_prob < 1 ", {
  expect_error(
    pcal(.1, -.1),
    "Invalid argument:
      all elements of prior_prob must be in the \\[0, 1\\] interval."
  )
}
)

test_that("prior_prob w/ NA ", {
  expect_warning(
    pcal(c(.1, .2), c(.1, NA)),
    "There are NA or NaN values in prior_prob."
  )
}
)

test_that("prior_prob w/ NaN ", {
  expect_warning(
    pcal(c(.1, .2), c(.1, NaN)),
    "There are NA or NaN values in prior_prob."
  )
}
)

context("pcal - miscellaneous")

test_that("length(p) vs length(prior_prob) test 1", {
  expect_error(
    pcal(c(.1, .1, .1, .1), c(.1, .2, .3))
  )
})

test_that("length(p) vs length(prior_prob) test 2 ", {
  expect_error(
    pcal(c(.1, .1), c(.1, .2, .3))
  )
})