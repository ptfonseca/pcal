
#' @title Lower Bounds on the Posterior Probabilities of Point Null Hypotheses
#'
#' @description Calibrate p-values under a robust perspective so that they can be interpreted as either lower bounds on the posterior probabilities of point null hypotheses or as lower bounds on the probabilities of type I errors.
#'
#' @param p A numeric vector with values in the \[0,1\] interval.
#' @param prior_prob A numeric vector with values in the \[0,1\] interval. If `length(p) == 1` then `prior_prob` can be of any positive length, but if `length(p) > 1` then the `length` of `prior_prob` can only be `1` or equal to the `length` of `bf`.
#'
#' @details \insertCite{sellke2001;textual}{pcal} developed a calibration of p-values into lower bounds for the posterior probabilities of point null hypotheses or lower bounds for the probabilities of type I errors for the case when both the null and the alternative hypotheses have 0.5 prior probability. `pcal` generalizes the aforementioned calibration for prior probabilities other than 0.5.
#'
#' `pcal` starts by transforming the values in `p` into lower bounds on Bayes factors using `bcal` and then uses `bfactor_to_prob` together with prior probabilities `prior_prob` to turn those Bayes factors into posterior probabilities. For each element of `p`, `pcal` returns an  approximation of the smallest posterior probability of the null hypothesis that is found by changing the prior distribution of the parameter of interest (under the alternative hypothesis) over wide classes of distributions.
#'
#' The output of `pcal` can also be interpreted as lower bounds on the probabilities of type I errors. Note that the output of this calibration has both Bayesian and Frequentist interpretations. \insertCite{sellke2001;textual}{pcal} noted that a scenario in which they definitely recommend this calibration is when investigating fit to the null model with no explicit alternative in mind. \insertCite{pericchiTorres2011;textual}{pcal} warn that despite the usefulness and appropriateness of this p-value calibration it does not depend on sample size, and hence the lower bounds obtained with large samples may be conservative.
#'
#' @return If `length(p) > 1` then `pcal` returns a numeric vector with the same `length` as `p`, otherwise it returns a numeric vector with the same `length` as `prior_prob`.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso {\link[pcal]{bcal}} for a p-value calibration that returns lower bounds on Bayes factors in favor of point null hypotheses.
#'
#' @examples
#' # Calibration of a typical "threshold" p-value:
#' pcal(.05)
#'
#' # Calibration of typical "threshold" p-values:
#' pcal(c(.1, .05, .01, .005, .001))
#'
#' # Application: chi-squared goodness-of-fit test,
#' # lower bound on the posterior probability of the null hypothesis:
#' x <- matrix(c(12, 41, 25, 33), ncol = 2)
#' pcal(chisq.test(x)[["p.value"]])
#'
#' @export
#' @importFrom Rdpack reprompt

pcal <- function(p, prior_prob = 0.5){

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
  if(is.null(prior_prob)){
    stop("Invalid argument: 'prior_prob' is NULL")
  }
  if(length(prior_prob) == 0){
    stop("Invalid argument: 'prior_prob' is empty")
  }
  if(any(is.na(prior_prob))){
    stop("Invalid argument: There are NA or NaN values in 'prior_prob'")
  }
  if(any(isFALSE(is.numeric(prior_prob) && is.vector(prior_prob)))){
    stop("Invalid argument: 'prior_prob' must be a numeric vector")
  }
  if(any(prior_prob < 0, prior_prob > 1)){
    stop("Invalid argument: all elements of 'prior_prob' must be in the [0, 1] interval")
  }
  if(isTRUE(length(p) > 1) && isFALSE(length(prior_prob) %in% c(1, length(p)))){
    stop("Invalid argument: if length(p) > 1 then length(prior_prob) can only be 1 or equal to length(p)")
  }

  suppressWarnings(bfactor_to_prob(bcal(p), prior_prob))

}
