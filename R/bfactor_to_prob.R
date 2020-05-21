
#' Posterior Probabilities of Precise Null Hypotheses in terms of Bayes Factors
#'
#' Update the prior probability of a precise null hypothesis to the posterior probability using a Bayes factor.
#'
#' \code{bfactor_to_prob} takes as input a Bayes factor in favor of the null hypothesis (\code{bf}) and the prior probability of the null hypothesis (\code{null_prob}). It then computes the posterior probability of the null hypothesis using the following relation from \insertCite{bergerSelke1987;textual}{pcal}: \deqn{(1 + (1 - null_prob) / null_prob * (1 / bf)) ^(-1)} The argument \code{null_prob} is optional and set to 0.5 by default, implying equiprobability of events.
#'
#' @param bf A non-negative numeric vector or matrix.
#' @param null_prob A numeric vector of length 1. Must be between 0 and 1 (inclusive).
#'
#' @return \code{bfactor_to_prob} returns:
#' \itemize{
#'   \item A numeric vector with the same \code{length} as \code{bf} if \code{bf} is a vector.
#'   \item A numeric matrix with the same \code{dim} as \code{bf} if \code{bf} is a matrix.}
#'
#' @examples
#' # With Bayes factors that favor the null hypothesis:
#' round(bfactor_to_prob(seq(2, 50, 2.5)), 3)
#'
#' # When the Bayes factor is indifferent between null and alternative:
#' bfactor_to_prob(1)
#'
#' # With Bayes factors that favor the alternative hypothesis:
#' round(bfactor_to_prob(seq(0, 1, .05)), 3)
#'
#' # Application: chi-squared goodness of fit test
#' x <- matrix(c(12, 41, 25, 33), ncol = 2)
#' bfactor_to_prob(bcal(chisq.test(x)$p.value))
#'
#' @seealso \code{\link{bcal}} for the calculation of Bayes factors.
#' @references
#' \insertAllCited{}
#' @export

bfactor_to_prob <- function(bf, null_prob = .5) {

  if(any(!is.atomic(bf), !is.vector(bf) && !is.matrix(bf))){
    stop("Invalid argument: 'x' must be an atomic vector or matrix.")
  }
  if(any(bf < 0)){
    stop("Invalid argument: Negative Bayes factor")
  }
  if(isFALSE(typeof(bf) %in% c("double", "integer"))){
    stop("Invalid argument: typeof(bf) must be 'integer', 'double'.")
  }
  if(isFALSE(length(null_prob) == 1)){
    stop("Error: 'null_prob' must be of length 1. ")
  }
  if(any(null_prob < 0, null_prob > 1, isFALSE(typeof(null_prob) %in% c("double", "integer")), is.null(null_prob), is.na(null_prob))){
    stop("Error: 'null_prob' must be between zero and one.")
  }

  (1 + (1 - null_prob) / null_prob * (1 / bf)) ^(-1)

}
