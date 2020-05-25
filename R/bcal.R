
#' @title Calibration of P-Values for Testing Point Null Hypotheses
#'
#' @description Calibrate p-values under a robust Bayesian perspective so that they can be interpreted as lower bounds on the odds provided by the data (Bayes factors) in favor of point null hypotheses.
#'
#' @param p A numeric vector with values in the [0,1] interval.
#'
#' @details A useful way to calibrate a p-value under a robust Bayesian perspective is by using the bound that is found as the minimum Bayes factor in favour of the null that is obtained by changing priors over large classes of distributions under \eqn{H_{1}{H1}}. This Bayesian calibration is obtained with \deqn{B(p) = -exp(1) p log (p)} when \eqn{p < 1/exp{1}}  \insertCite{pericchiTorres2011}{pcal}  \insertCite{sellke2001}{pcal}
#'
#' @return Returns a numeric vector with length as \code{p}.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso {\itemize
#' \item \code{\link[pcal]{pcal}} For a p-value calibration that returns lower bounds on the posterior probability of the null.
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





