
#' @noRd

mantissa <- function(x){

  x <- abs(x)
  e <- ifelse(x == 0, 0, floor(log10(x)))
  m <- x / 10^e
  round(m, 10)
}

#' @noRd

msdigit <- function(x){

  x <- x[x != 0]
  x <- floor(mantissa(x))
  return(x)
}

#' @noRd

smsdigit <- function(x){

  x <- x[x != 0 & (x %% 10 == 0 | mantissa(x) != floor(mantissa(x)))]
  x <- floor((mantissa(x)*10)) %% 10
  return(x)
}

#' @noRd

theta_benford <- function(d){

  if (d == 1){
    theta_benford <- sapply(1:9, function(x){log10(1+1/(x))})
  } else if (d == 2){
    theta_benford <- sapply(0:9, function(x){sum(log10(1+1/(10*(1:9)+x)))})
  }
  return(theta_benford)
}

#' @noRd

chisq_test_multinomial <- function(x, categories, null_par){

  data.name <- deparse(substitute(x))
  x <- x[!is.na(x)]
  categories <- factor(categories, levels = categories)
  counts <- table(x[!is.na(x)])[levels(categories)]

  tst <- chisq.test(counts,  p = null_par)

  results <- list(
    method = "Pearson's Chi-Squared Test for Multinomial Proportions",
    alternative = "two.sided",
    data.name = data.name,
    statistic = c("Chi-squared" = unname(tst$statistic)),
    p.value =  tst$p.value,
    sample.size = c("n" = length(x))
  )

  class(results) <- "htest"
  results

}
