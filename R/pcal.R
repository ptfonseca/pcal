
pcal <- function(p, prob = 0.5){ # P-Value Calibrations from Selke, Bayarri & Berger (2001)
  # Calibrates p-values into lower bounds for posterior probability of the null.

  ifelse(p > 1 , stop("p>1 -- Invalid p-value.", call. = FALSE),
         ifelse(p < 0, stop("p<0 -- invalid p-value.", call. = FALSE),
                ifelse(p == 0, 0,
                       ifelse(p<(1/exp(1)), return((1+((1-prob)/prob)*(1/bcal(p)))^(-1)),
                              return((1+((1-prob)/prob)*(1/bcal(p)))^(-1)))
                )
         )
  )
}
