
#' Calibration of P-Values for Testing Point Null Hypotheses
#'
#' \code{bcal} calibrates p-values under a robust Bayesian perspective so that they can be interpreted as the lower bound on the odds provided by the data (Bayes factor) in favor of a point null hypothesis.
#'
#' A useful way to calibrate a p-value under a robust Bayesian perspective is by using the bound that is found as the minimum Bayes factor in favour of the null that is obtained by changing priors over large classes of distributions under \eqn{H_{1}{H1}}. This Bayesian calibration is obtained with \deqn{B(p) = -exp(1) p log (p)} when \eqn{p < 1/exp{1}}  \insertCite{pericchiTorres2011}{pcal}  \insertCite{sellke2001}{pcal}
#'
#' @param p A numeric vector. All elements must be between zero and one (inclusive).
#'
#' @return Returns a numeric vector with length equal to \code{length(p)}

#' @examples
#'
#' # Calibration of p-values
#'
#' round(bcal(c(.1, .05, .01)), 3)
#'
#' df <- read.csv("https://goo.gl/j6lRXD")
#' table(df$treatment, df$improvement)
#' bcal(chisq.test(df$treatment, df$improvement, correct=FALSE)[["p.value"]])
#'
#' @importFrom Rdpack reprompt
#' @seealso {\itemize
#' \item \code{\link[pcal]{pcal} to calibrate p-values
#' \item \code{\link[pcal]{bfactor_to_prob}} to transform Bayes factors in posterior probabilities of the null hypothesis}
#' @references
#' \insertAllCited{}
#' @export

bcal <- function(p) {  bfactor_to_prob

  if(is.null(p)){
    stop("Invalid argument: 'p' is NULL")
  }
  if(!is.numeric(p)){
    stop("Invalid argument: 'p' must be a numeric vector")
  }
  if(any(p > 1)){
    stop("Invalid argument: 'p' > 1")
  }
  if(any(p < 0)){
    stop("Invalid argument: 'p' < 0")
  }
  if(all(is.na(p))){
    warning("All elements of 'p' are NA")
  }
  if(any(is.na(p))){
    warning("Some elements of 'p' are NA")
  }

  ifelse(p == 0, 0,
         ifelse(p < (exp(1) ^ (-1)), -exp(1) * log(p) * p,
                1))

}





