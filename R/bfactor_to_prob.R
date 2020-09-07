
#' @title Turn Bayes Factors Into Posterior Probabilities
#'
#' @description \loadmathjax Update prior probabilities of models/hypotheses to posterior probabilities using Bayes factors.
#'
#' @param bf A numeric vector of non-negative values.
#' @param prior_prob A numeric vector with values in the \[0,1\] interval. If `length(bf) == 1` then `prior_prob` can be of any positive \code{\link[base]{length}}, but if `length(bf) > 1` then the \code{\link[base]{length}} of `prior_prob` can only be `1` or equal to the \code{\link[base]{length}} of `bf`.
#'
#' @details `bfactor_to_prob` computes posterior probabilities of hypotheses using a vectorized version of the following equation from \insertCite{bergerDelampady1987;textual}{pcal}:
#'
#' \mjdeqn{\operatorname{P} \left( \operatorname{H_0} | x \right) = \left( 1 + \frac{1 - \pi_0}{\pi_0} \, \frac {1}{\operatorname{B}_{01} \left( x \right) } \right) ^{-1}}{1111}
#'
#' where \mjeqn{\operatorname{B}_{01} \left( x \right)}{1111} is a Bayes factor if favor of the null hypothesis given the data \mjeqn{x}{x}, \mjeqn{\pi_0}{pi_0} is the prior probability of the null hypothesis and \mjeqn{1 - \pi_0}{1 - pi_0} is the prior probability of the alternative hypothesis. Using `bfactor_to_prob`, the posterior probabilities of the null hypotheses can be obtained with `bfactor_to_prob(bf, prior_prob)` and the posterior probabilities of the alternative hypotheses can be obtained with `bfactor_to_prob(bf, prior_prob)`.
#'
#' The `prior_prob` argument is optional and is set to 0.5 by default, implying prior equiprobability of hypotheses. `prior_prob` can only be of \code{\link[base]{length}} equal to `length(bf)`, in which case each prior probability in `prior_prob` will be updated using the corresponding element of `bf`, or of \code{\link[base]{length}} `1`, in which case it will be recycled (if `length(bf) > 1`) and each element of `bf` will update the same `prior_prob` value.
#'
#' @return If `length(bf) > 1` then `bfactor_to_prob` returns a numeric vector with the same \code{\link[base]{length}} as `bf`, otherwise it returns a numeric vector with the same \code{\link[base]{length}} as `prior_prob`. Warning messages are thrown if there are `NA` or `NaN` values in `bf` or in `prior_prob`.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso
#' * \code{\link[pcal]{bfactor_interpret}} for the interpretation of Bayes factors.
#'
#' @examples
#' # With a Bayes factor that is indifferent between the null and the alternative hypotheses:
#' bfactor_to_prob(1)
#'
#' # Same as above but the null hypothesis has high prior probability:
#' bfactor_to_prob(1, .99)
#'
#' # Posterior probability of the null as a function of different prior probabilities:
#' bfactor_to_prob(1, seq(.5, 1, .1))
#'
#' # With Bayes factors that favor the null hypothesis:
#' round(bfactor_to_prob(seq(2, 50, 2.5)), 3)
#'
#' # Same as above but the null hypothesis has low prior probability:
#' round(bfactor_to_prob(seq(2, 50, 2.5), prior_prob = .01), 3)
#'
#' # Posterior probabilities obtained with Bayes factors that favor the alternative hypothesis:
#' round(bfactor_to_prob(seq(0, 1, .05)), 3)
#'
#' # Same as above but the null hypothesis has high prior probability:
#' round(bfactor_to_prob(seq(0, 1, .05), prior_prob = .99), 3)
#'
#' # Application: chi-squared goodness-of-fit test,
#' # lower bound on the posterior probability of the null hypothesis:
#' x <- matrix(c(12, 41, 25, 33), ncol = 2)
#' bfactor_to_prob(bcal(chisq.test(x)[["p.value"]]), prior_prob = .9)
#'
#' @export

bfactor_to_prob <- function(bf, prior_prob = .5) {

  if(is.null(bf)) {
    stop("Invalid argument: bf is NULL.")
  }
  if(any(isFALSE(is.atomic(bf)), isFALSE(is.vector(bf)))) {
    stop("Invalid argument: bf must be an atomic vector.")
  }
  if(length(bf) == 0) {
    stop("Invalid argument: bf is empty.")
  }
  if(all(is.na(bf))) {
    stop("Invalid argument: all elements of bf are NA or NaN.")
  }
  if(isFALSE(is.numeric(bf))) {
    stop("Invalid argument: bf must be numeric.")
  }
  if(any(bf[!is.na(bf)] < 0)) {
    stop("Invalid argument: all elements of bf must be non-negative.")
  }
  if(any(is.na(bf))) {
    warning("There are NA or NaN values in bf.")
  }

  pp_filtered <- prior_prob[!is.na(prior_prob)]

  if(is.null(prior_prob)) {
    stop("Invalid argument: prior_prob is NULL.")
  }
  if(any(isFALSE(is.atomic(prior_prob)), isFALSE(is.vector(prior_prob)))) {
    stop("Invalid argument: prior_prob must be an atomic vector.")
  }
  if(length(prior_prob) == 0) {
    stop("Invalid argument: prior_prob is empty.")
  }
  if(all(is.na(prior_prob))) {
    stop("Invalid argument: All elements of prior_prob are NA or NaN.")
  }
  if(isFALSE(is.numeric(prior_prob))) {
    stop("Invalid argument: prior_prob must be numeric.")
  }
  if(any(pp_filtered < 0, pp_filtered > 1)) {
    stop("Invalid argument: all elements of prior_prob must be in the [0, 1] interval.")
  }
  if(any(is.na(prior_prob))) {
    warning("There are NA or NaN values in prior_prob.")
  }

  if(isTRUE(length(bf) > 1) && isFALSE(length(prior_prob) %in% c(1, length(bf)))) {
    stop("Invalid argument: if length(bf) > 1 then length(prior_prob) can only be 1 or equal to length(bf).")
  }

  (1 + (1 - prior_prob) / prior_prob * (1 / bf)) ^ (-1)

}
