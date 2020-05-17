
## Auxiliary functions

bcal <- function(p, l = NULL) { # P-Value Calibration from Selke, Bayarri & Berger (2001)
# Calibrates p-values into lower bounds for Bayes Factors in favour of the null

  if (missing(l)){

    ifelse(p>1, stop("p>1", call. = FALSE),
           ifelse(p<0, stop("p<0", call. = FALSE),
                  ifelse(p==0, 0,
                         ifelse(p < (1 / exp(1)), -1*exp(1)*p*log(p), 1)
                  )
           )
    )

  } else  {

    ifelse(p>1, stop("p>1", call. = FALSE),
           ifelse(p<0, stop("p<0", call. = FALSE),
                  ifelse(p==0, log(0, base=l),
                         ifelse(p < (1 / exp(1)), log(-1*exp(1)*p*log(p),base = l), log(1, base=l))
                  )
           )
    )

  }
}

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



