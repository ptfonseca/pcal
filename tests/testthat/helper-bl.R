

theta_bl1 <- log10(1 + 1 / (1:9))
theta_bl2 <- sapply(0:9, function(x) {
  sum(log10(1 + 1 / (10 * (1:9) + x)))
  })
