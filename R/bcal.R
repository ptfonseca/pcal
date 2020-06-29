
#' @title Lower Bounds on Bayes Factors for Point Null Hypotheses
#'
#' @description Calibrate p-values under a robust Bayesian perspective so that they can be interpreted as lower bounds on Bayes factors in favor of point null hypotheses.
#'
#' @param p A numeric vector with values in the \[0,1\] interval.
#'
#' @details `bcal` uses the calibration of p-values into lower bounds for Bayes factors developed in \insertCite{sellke2001;textual}{pcal}: \deqn{B(p) = -e \, p \, log (p)}{B(p) = -e p log (p)} for `p` < (1/e) and \deqn{B(p) = 1} otherwise, where `p` is a  p-value on a classical test statistic and \eqn{B(p)} approximates the smallest Bayes factor that is found by changing the prior distribution of the parameter of interest (under the alternative hypothesis) over wide classes of distributions.
#'
#' \insertCite{sellke2001;textual}{pcal} noted that a scenario in which they definitely recommend this calibration is when investigating fit to the null model/hypothesis with no explicit alternative in mind. \insertCite{pericchiTorres2011;textual}{pcal} warn that despite the usefulness and appropriateness of this p-value calibration it does not depend on sample size and hence the lower bounds obtained with large samples may be conservative.
#'
#' @return `bcal` returns a numeric vector with the same `length` as `p`.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso
#' * \code{\link[pcal]{pcal}} for a p-value calibration that returns lower bounds on the posterior probabilities of point null hypotheses.
#' * \code{\link[pcal]{bfactor_interpret}} and \code{\link[pcal]{bfactor_interpret_kr}} for the interpretation of Bayes factors.
#' * \code{\link[pcal]{bfactor_log_interpret}} and \code{\link[pcal]{bfactor_log_interpret_kr}} for the interpretation of the logarithms of Bayes factors.
#' * \code{\link[pcal]{bfactor_to_prob}} to turn Bayes factors into posterior probabilities.
#'
#' @examples
#' # Calibration of a typical "threshold" p-value:
#' bcal(.05)
#'
#' # Calibration of typical "threshold" p-values:
#' bcal(c(.1, .05, .01, .005, .001))
#'
#' # Application: chi-squared goodness-of-fit test,
#' # lower bound on the Bayes factor in favor of the null hypothesis:
#' x <- matrix(c(12, 41, 25, 33), ncol = 2)
#' bcal(chisq.test(x)[["p.value"]])
#'
#' @export

bcal <- function(p) {

  if(is.null(p)){
    stop("Invalid argument: 'p' is NULL")
  }
  if(length(p) == 0){
    stop("Invalid argument: 'p' is empty")
  }
  if(any(!is.numeric(p), !is.vector(p),  all(is.na(p)))){
    stop("Invalid argument: 'p' must be a numeric vector")
  }
  if(any(p[!is.na(p)] < 0, p[!is.na(p)] > 1)){
    stop("Invalid argument: all elements of 'p' must be in the [0, 1] interval.")
  }
  if(any(is.na(p))){
    warning("There are NA or NaN values in 'p'")
  }

  ifelse(p == 0, 0,
         ifelse(p < (exp(1) ^ (-1)), -exp(1) * log(p) * p,
                1))
}
