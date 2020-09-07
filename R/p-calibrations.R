
#' @title Lower Bounds on Bayes Factors for Point Null Hypotheses
#'
#' @description \loadmathjax Calibrate p-values under a robust Bayesian perspective so that they can be directly interpreted as lower bounds on Bayes factors in favor of point null hypotheses.
#'
#' @param p A numeric vector with values in the \[0,1\] interval.
#'
#' @details `bcal` is a vectorized implementation of the calibration of p-values into lower bounds of Bayes factors developed in \insertCite{sellke2001;textual}{pcal}: \mjdeqn{\operatorname{B}(p) = -\operatorname{e} \, p \, \operatorname{log} (p)}{B(p) = -e p log(p)} for \mjeqn{p < (1/\operatorname{e})}{p < (1 / e)} and \mjdeqn{\operatorname{B}(p) = 1}{B(p) = 1} otherwise, where \mjeqn{p}{p} is a  p-value on a classical test statistic and \mjeqn{\operatorname{B}(p)}{B(p)} approximates the smallest Bayes factor that is found by changing the prior distribution of the parameter of interest (under the alternative hypothesis) over wide classes of distributions.
#'
#' \insertCite{sellke2001;textual}{pcal} noted that a scenario in which they definitely recommend this calibration is when investigating fit to the null model/hypothesis with no explicit alternative in mind. \insertCite{pericchiTorres2011;textual}{pcal} warn that despite the usefulness and appropriateness of this p-value calibration it does not depend on sample size and hence the lower bounds obtained with large samples may be conservative.
#'
#' @return `bcal` returns a numeric vector with the same \code{\link[base]{length}} as `p` and throws a warning message if there are `NA` or `NaN` values in `p`.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso
#' * \code{\link[pcal]{bfactor_interpret}} for the interpretation of Bayes factors.
#' * \code{\link[pcal]{bfactor_to_prob}} to turn Bayes factors into posterior probabilities.
#' * \code{\link[pcal]{pcal}} for a p-value calibration that returns lower bounds for the posterior probabilities of point null hypotheses.
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
#' data <- matrix(c(18, 12, 10, 12, 10, 23), ncol = 2)
#' bcal(chisq.test(data)[["p.value"]])
#'
#' @export

bcal <- function(p) {

  p_filtered <- p[!is.na(p)]

  if(is.null(p)) {
    stop("Invalid argument: p is NULL.")
  }
  if(any(isFALSE(is.atomic(p)), isFALSE(is.vector(p)))) {
    stop("Invalid argument: p must be an atomic vector.")
  }
  if(length(p) == 0) {
    stop("Invalid argument: p is empty.")
  }
  if(all(is.na(p))) {
    stop("Invalid argument: all elements of p are NA or NaN.")
  }
  if(isFALSE(is.numeric(p))) {
    stop("Invalid argument: p must be numeric.")
  }
  if(any(p_filtered < 0, p_filtered > 1)) {
    stop("Invalid argument: all elements of p must be in the [0, 1] interval.")
  }
  if(any(is.na(p))) {
    warning("There are NA or NaN values in p.")
  }

  ifelse(p == 0, 0,
         ifelse(p < (exp(1) ^ (-1)), - exp(1) * p * log(p),
                1))
}

#' @title Lower Bounds on Posterior Probabilities for Point Null Hypotheses
#'
#' @description \loadmathjax Calibrate p-values under a robust perspective so that they can be directly interpreted as either lower bounds on the posterior probabilities of point null hypotheses or lower bounds on the probabilities of type I errors.
#'
#' @param prior_prob A numeric vector with values in the \[0,1\] interval. If `length(p) == 1` then `prior_prob` can be of any positive \code{\link[base]{length}}, but if `length(p) > 1` then the \code{\link[base]{length}} of `prior_prob` can only be `1` or equal to the \code{\link[base]{length}} of `p`.
#' @inheritParams bcal
#'
#' @details \insertCite{sellke2001;textual}{pcal} developed the following calibration of p-values into lower bounds for the posterior probabilities of point null hypotheses (or lower bounds for the probabilities of type I errors): \mjdeqn{\alpha(p) = (1 + \lbrack -e \,  p \operatorname{log}(p) \rbrack^{-1})^{-1}}{alpha(p) = (1 + \[-e p log(p)\]^(-1))^(-1)} where \mjeqn{p}{p} is a  p-value on a classical test statistic. This calibration assumes that both the null and the alternative hypotheses have 0.5 prior probability. We generalized the aforementioned calibration for prior probabilities other than 0.5: \mjdeqn{\alpha(p) = \left(1 + \frac{1 - \pi_0}{\pi_0} \, \lbrack -e \,  p \operatorname{log}(p) \rbrack^{-1}\right)^{-1}}{alpha(p) = (1 + (1 - pi_0)/pi_0 \[-e p log(p)\]^(-1))^(-1)} where \mjeqn{\pi_0}{pi_0} is the prior probability of the null hypothesis and the alternative hypothesis has prior protability \mjeqn{1 - \pi_0}{1 - pi_0}. `pcal` is a vectorized implementation of the latter formula. Note that `pcal(p, prior_prob)` is equivalent to \code{\link[pcal]{bfactor_to_prob}(\link[pcal]{bcal}(p), prior_prob)}.
#'
#' For each element of `p`, `pcal` returns an  approximation of the smallest posterior probability of the null hypothesis that is found by changing the prior distribution of the parameter of interest (under the alternative hypothesis) over wide classes of distributions.  Alternatively, the output of `pcal` can also be interpreted as lower bounds on the probabilities of type I errors, which means that this calibration has both Bayesian and Frequentist interpretations.
#'
#' The `prior_prob` argument is optional and is set to 0.5 by default, implying prior equiprobability of hypotheses. `prior_prob` can only be of \code{\link[base]{length}} equal to  the `length` of `p`, in which case each prior probability in `prior_prob` is used in the calibration of the corresponding element of `p`, or of \code{\link[base]{length}} `1`, in which case it will be recycled (if `length(p) > 1`) and the same `prior_prob` value is used in the calibration of all the elements of `p`.
#'
#' \insertCite{sellke2001;textual}{pcal} noted that a scenario in which they definitely recommend this calibration is when investigating fit to the null model with no explicit alternative in mind. \insertCite{pericchiTorres2011;textual}{pcal} warn that despite the usefulness and appropriateness of this p-value calibration it does not depend on sample size, and hence the lower bounds obtained with large samples may be conservative.
#'
#' @return If `length(p) > 1` then `pcal` returns a numeric vector with the same \code{\link[base]{length}} as `p`, otherwise it returns a numeric vector with the same \code{\link[base]{length}} as `prior_prob`. Warning messages are thrown if there are `NA` or `NaN` values in `p` or in `prior_prob`.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso
#' * \code{\link[pcal]{bcal}} for a p-value calibration that returns lower bounds on Bayes factors in favor of point null hypotheses.
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
#' data <- matrix(c(18, 12, 10, 12, 10, 23), ncol = 2)
#' pcal(chisq.test(data)[["p.value"]])
#'
#' @export

pcal <- function(p, prior_prob = 0.5) {

  p_filtered <- p[!is.na(p)]

  if(is.null(p)) {
    stop("Invalid argument: p is NULL.")
  }
  if(any(isFALSE(is.atomic(p)), isFALSE(is.vector(p)))) {
    stop("Invalid argument: p must be an atomic vector.")
  }
  if(length(p) == 0) {
    stop("Invalid argument: p is empty.")
  }
  if(all(is.na(p))) {
    stop("Invalid argument: all elements of p are NA or NaN.")
  }
  if(isFALSE(is.numeric(p))) {
    stop("Invalid argument: p must be numeric.")
  }
  if(any(p_filtered < 0, p_filtered > 1)) {
    stop("Invalid argument: all elements of p must be in the [0, 1] interval.")
  }
  if(any(is.na(p))) {
    warning("There are NA or NaN values in p.")
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

  if(isTRUE(length(p) > 1) && isFALSE(length(prior_prob) %in% c(1, length(p)))) {
    stop("Invalid argument: if length(p) > 1 then length(prior_prob) can only be 1 or equal to length(p).")
  }

  lb_bf <-  ifelse(p == 0, 0,
                   ifelse(p < (exp(1) ^ (-1)), - exp(1) * p * log(p),
                          1))

  (1 + (1 - prior_prob) / prior_prob * (1 / lb_bf)) ^ (-1)

}


