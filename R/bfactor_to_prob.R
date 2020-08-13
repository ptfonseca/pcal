
#' @title Turn Bayes Factors Into Posterior Probabilities
#'
#' @description Update the prior probabilities of models/hypotheses to posterior probabilities using Bayes factors.
#'
#' @param bf A numeric vector of non-negative values.
#' @param prior_prob A numeric vector with values in the \[0,1\] interval. If `length(bf) == 1` then `prior_prob` can be of any positive \code{\link[base]{length}}, but if `length(bf) > 1` then the \code{\link[base]{length}} of `prior_prob` can only be `1` or equal to the \code{\link[base]{length}} of `bf`.
#'
#' @details `bfactor_to_prob` computes the posterior probability of the null hypothesis using the following equation from \insertCite{bergerDelampady1987;textual}{pcal}: \deqn{P(\textrm{null} \, \textrm{hypothesis}|\textrm{data}) = \left(1 + \frac{1 - {null\_prob}}{null\_prob} \times \frac{1}{bf}\right)^{-1}}{P(null hypothesis | data) = (1 + (1 - prior_prob) / prior_prob * (1 / bf)) ^(-1)} where `bf` is a Bayes factor if favor of the null hypothesis and `prior_prob` is the prior probability of the null hypothesis. The alternative hypothesis has prior probability `1 - prior_prob` and posterior probability `1 - bfactor_to_prob(bf, prior_prob)`.
#'
#' The `prior_prob` argument is optional and is set to 0.5 by default, implying prior equiprobability of hypotheses. `prior_prob` can only be of \code{\link[base]{length}} equal to `length(bf)`, in which case each prior probability in `prior_prob` will be updated using the corresponding element of `bf`, or of \code{\link[base]{length}} `1`, in which case it will be recycled (if `length(bf) > 1`) and each element of `bf` will update the same `prior_prob` value.
#'
#' @return If `length(bf) > 1` then `bfactor_to_prob` returns a numeric vector with the same \code{\link[base]{length}} as `bf`, otherwise it returns a numeric vector with the same \code{\link[base]{length}} as `prior_prob`.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso
#' * \code{\link[pcal]{bfactor_interpret}} for the interpretation of Bayes factors.
#' * \code{\link[pcal]{bfactor_log_interpret}} for the interpretation of the logarithms of Bayes factors.
#' * \code{\link[pcal]{bcal}} for a p-value calibration that returns lower bounds on Bayes factors in favor of point null hypotheses.
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

  check_bf(bf)
  check_prior_prob(prior_prob)

  if(isTRUE(length(bf) > 1) && isFALSE(length(prior_prob) %in% c(1, length(bf)))){
    stop("Invalid argument: if length(bf) > 1 then length(prior_prob) can only be 1 or equal to length(bf)")
  }

  (1 + (1 - prior_prob) / prior_prob * (1 / bf)) ^(-1)

}
