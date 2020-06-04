
#' @title Lower Bounds on the Posterior Probabilities of Point Null Hypotheses
#'
#' @description Calibrate p-values under a robust Bayesian perspective so that they can be interpreted as either lower bounds on the probability of a type I error or lower bounds on the posterior probabilities of point null hypotheses.
#'
#' @param p A numeric vector with values in the \[0,1\] interval.
#' @param prior_prob A numeric vector with values in the \[0,1\] interval. If \code{length(p)} = 1 then \code{prior_prob} can be of any positive length, but if \code{length(p)} > 1 then \code{length(prior_prob)} can only be 1 or equal to \code{length(p)}.
#'
#' @details \code{pcal} generalizes the calibration of p-values into posterior probabilities developed in \insertCite{sellke2001;textual}{pcal} for prior probabilities other than 0.5. \code{pcal} starts by transforming \code{p} into lower bounds on Bayes factors using \code{bcal} and then uses \code{bfactor_to_prob} together with prior probabilities \code{prior_prob} to turn those Bayes factors into posterior probabilities. The result is an  approximation of the smallest posterior probability of the null hypothesis that is found by changing the prior distribution of the parameter of interest (under the alternative hypothesis) over wide classes of distributions. The output of \code{pcal} can also be interpreted as the lower bound of the probability of a type I error. \insertCite{sellke2001;textual}{pcal} note that a scenario in which they definitely recommend this calibration is when investigating fit to the null model with no explicit alternative in mind. \insertCite{pericchiTorres2011;textual}{pcal} warn that despite the usefulness and appropriateness of this p-value calibration it does not depend on sample size, and hence for large sample sizes the obtained lower bounds may be conservative.
#'
#' @return If \code{length(p)} > 1 then \code{pcal} returns a numeric vector with the same \code{length} as \code{p}, otherwise it returns a numeric vector with the same \code{length} as \code{prior_prob}.
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
#' # lower bound on the posterior probability of the null:
#' x <- matrix(c(12, 41, 25, 33), ncol = 2)
#' pcal(chisq.test(x)[["p.value"]])
#'
#' @importFrom Rdpack reprompt
#' @export
#'

pcal <- function(p, prior_prob = 0.5){

  if(is.null(p)){
    stop("Invalid argument: 'p' is NULL", call. = FALSE)
  }
  if(length(p) == 0){
    stop("Invalid argument: 'p' is empty", call. = FALSE)
  }
  if(any(!is.numeric(p), !is.vector(p),  all(is.na(p)))){
    stop("Invalid argument: 'p' must be a numeric vector", call. = FALSE)
  }
  if(any(p[!is.na(p)] < 0, p[!is.na(p)] > 1)){
    stop("Invalid argument: all elements of 'p' must be in the [0, 1] interval.", call. = FALSE)
  }
  if(any(is.na(p))){
    warning("There are NA or NaN values in 'p'", call. = FALSE)
  }
  if(is.null(prior_prob)){
    stop("Invalid argument: 'prior_prob' is NULL", call. = FALSE)
  }
  if(length(prior_prob) == 0){
    stop("Invalid argument: 'prior_prob' is empty", call. = FALSE)
  }
  if(any(is.na(prior_prob))){
    stop("Invalid argument: There are NA or NaN values in 'prior_prob'", call. = FALSE)
  }
  if(any(isFALSE(is.numeric(prior_prob) && is.vector(prior_prob)))){
    stop("Invalid argument: 'prior_prob' must be a numeric vector", call. = FALSE)
  }
  if(any(prior_prob < 0, prior_prob > 1)){
    stop("Invalid argument: all elements of 'prior_prob' must be in the [0, 1] interval", call. = FALSE)
  }
  if(isTRUE(length(p) > 1) && isFALSE(length(prior_prob) %in% c(1, length(p)))){
    stop("Invalid argument: if length(p) > 1 then length(prior_prob) can only be 1 or equal to length(p)", call. = FALSE)
  }

  suppressWarnings(bfactor_to_prob(bcal(p), prior_prob))

}








