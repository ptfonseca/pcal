

theta_bl1 <- sapply(1:9, function(x){log10(1+1/(x))})
theta_bl2 <- sapply(0:9, function(x){sum(log10(1+1/(10*(1:9)+x)))})
