
#' @title Lower bounds on Bayes for Testing Point Null Hypotheses
#'
#' @description Calibrate p-values under a robust Bayesian perspective so that they can be interpreted as lower bounds on the odds provided by the data (Bayes factors) in favor of point null hypotheses.
#'
#' @param p A numeric vector with values in the [0,1] interval.
#'
#' @details To obtain a robust measure of the evidence provided by the data in favor of a point the null hypothesis we can derive the lower bound on the odds of the data (or Bayes factor) that is found when changing the prior distribution of the parameters of interest under the alternative hypothesis over a wide class of distributions. \code{bcal} approximates such a lower bound using the p-value calibration developed in \insertCite{sellke2001;textual}{pcal}: \deqn{B(p) = -exp(1) p log (p)} for \eqn{p < 1/exp(1)}, where \code{p} is a  p-value on a classical test statistic, and \code{bcal(p) = 1} otherwise. \insertCite{sellke2001;textual}{pcal} note that a scenario in which they definitely recommend use of this calibration is then investigating fit to the null model, with no explicit alternative in mind. \insertCite{pericchiTorres2011;textual}{pcal} warn, however, that despite useful the p-values calibrations developed in \insertCite{sellke2001;textual}{pcal} do not depend on sample size and hence the lower bounds obtained with large sample sizes may be conservative
#'
#' @return Returns a numeric vector with length as \code{p}.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso {\itemize
#' \item \code{\link[pcal]{pcal}} for a p-value calibration that returns lower bounds on the posterior probability of the null.
#' \item \code{\link[pcal]{bfactor_to_prob}} to turn Bayes factors into posterior probabilities.}
#'
#' @examples
#' # Calibration of typical "threshold" p-values:
#' bcal(c(.1, .05, .01, .005, .001))
#'
#' # Application: chi-squared goodness-of-fit test,
#' # lower bound on the Bayes factor in favor of the null:
#' x <- matrix(c(12, 41, 25, 33), ncol = 2)
#' bcal(chisq.test(x)[["p.value"]])
#'
#' @importFrom Rdpack reprompt
#' @export

bcal <- function(p) {  bfactor_to_prob

  if(is.null(p)){
    stop("Invalid argument: 'p' is NULL")
  }
  if(isFALSE(is.numeric(p) && is.vector(p))){
    stop("Invalid argument: 'p' must be a numeric vector")
  }
  if(all(is.na(p))){
    stop("All elements of 'p' are NA or NaN")
  }
  if(any(is.na(p))){
    warning("Some elements of 'p' are NA or NaN")
  }
  if(any(p > 1)){
    stop("Invalid argument: 'p' > 1")
  }
  if(any(p < 0)){
    stop("Invalid argument: 'p' < 0")
  }

  ifelse(p == 0, 0,
         ifelse(p < (exp(1) ^ (-1)), -exp(1) * log(p) * p,
                1))
}





