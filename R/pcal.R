
#' @title Lower Bounds on the Posterior Probabilities of Point Null Hypotheses
#'
#' @description Calibrate p-values under a robust Bayesian perspective so that they can be interpreted as either lower bounds on the probability of a type I error or lower bounds on the posterior probability of the null hypothesis.
#'
#' @param p A numeric vector with values in the [0,1] interval.
#' @param prior_prob A numeric vector with values in the [0,1] interval.
#'
#' @details
#'
#' @return Returns a numeric vector with length as \code{p}.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso {\link[pcal]{bcal}} for a p-value calibration that returns lower bounds on the odds provided by the data (Bayes factors) in favor of point null hypotheses.
#'
#' @examples
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
  if(is.null(prior_prob)){
    stop("Invalid argument: 'prior_prob' is NULL.", call. = FALSE)
  }
  if(any(is.na(prior_prob))){
    stop("Invalid argument: NA or NaN values in 'prior_prob'.", call. = FALSE)
  }
  if(isFALSE(is.numeric(prior_prob) && is.vector(prior_prob))){
    stop("Invalid argument: 'prior_prob' must be a numeric vector.", call. = FALSE)
  }
  if(isTRUE(length(p) > 1) && isFALSE(length(prior_prob) %in% c(1, length(p)))){
    stop("Invalid argument: if length(p) > 1 then length(prior_prob) can only be 1 or length(p)", call. = FALSE)
  }
  if(any(prior_prob < 0, prior_prob > 1)){
    stop("Invalid argument: all elements of 'prior_prob' must be in the [0, 1] interval.", call. = FALSE)
  }

  ifelse(p == 0, 0, bfactor_to_prob(bcal(p)))

}








