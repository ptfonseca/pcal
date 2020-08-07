
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
#' @return `bcal` returns a numeric vector with the same \code{\link[base]{length}} as `p`.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso
#' * \code{\link[pcal]{check_prob}} to check if an object is a numeric vector of valid probability (or p-value) values.
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

  check_prob(p)

  ifelse(p == 0, 0,
         ifelse(p < (exp(1) ^ (-1)), -exp(1) * log(p) * p,
                1))
}

#' @title Lower Bounds on the Posterior Probabilities of Point Null Hypotheses
#'
#' @description Calibrate p-values under a robust perspective so that they can be interpreted as either lower bounds on the posterior probabilities of point null hypotheses or as lower bounds on the probabilities of type I errors.
#'
#' @param p A numeric vector with values in the \[0,1\] interval.
#' @param prior_prob A numeric vector with values in the \[0,1\] interval. If `length(p) == 1` then `prior_prob` can be of any positive \code{\link[base]{length}}, but if `length(p) > 1` then the \code{\link[base]{length}} of `prior_prob` can only be `1` or equal to the \code{\link[base]{length}} of `p`.
#'
#' @details \insertCite{sellke2001;textual}{pcal} developed a calibration of p-values into lower bounds for the posterior probabilities of point null hypotheses or lower bounds for the probabilities of type I errors for the case when both the null and the alternative hypotheses have 0.5 prior probability. `pcal` generalizes the aforementioned calibration for prior probabilities other than 0.5.
#'
#' `pcal` starts by transforming the values in `p` into lower bounds on Bayes factors using {\link[pcal]{bcal}} and then uses {\link[pcal]{bfactor_to_prob}} together with prior probabilities `prior_prob` to turn those Bayes factors into posterior probabilities. For each element of `p`, `pcal` returns an  approximation of the smallest posterior probability of the null hypothesis that is found by changing the prior distribution of the parameter of interest (under the alternative hypothesis) over wide classes of distributions.
#'
#' The `prior_prob` argument is optional and is set to 0.5 by default, implying prior equiprobability of hypotheses. `prior_prob` can only be of \code{\link[base]{length}} equal to  the `length` of `p`, in which case each prior probability in `prior_prob` is used in the calibration of the corresponding element of `p`, or of \code{\link[base]{length}} `1`, in which case it will be recycled (if `length(p) > 1`) and the same `prior_prob` value is used in the calibration of all the elements of `p`.
#'
#' The output of `pcal` can also be interpreted as lower bounds on the probabilities of type I errors. Note that the output of this calibration has both Bayesian and Frequentist interpretations. \insertCite{sellke2001;textual}{pcal} noted that a scenario in which they definitely recommend this calibration is when investigating fit to the null model with no explicit alternative in mind. \insertCite{pericchiTorres2011;textual}{pcal} warn that despite the usefulness and appropriateness of this p-value calibration it does not depend on sample size, and hence the lower bounds obtained with large samples may be conservative.
#'
#' @return If `length(p) > 1` then `pcal` returns a numeric vector with the same \code{\link[base]{length}} as `p`, otherwise it returns a numeric vector with the same \code{\link[base]{length}} as `prior_prob`.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso
#' * \code{\link[pcal]{check_prob}} to check if an object is a numeric vector of valid probability (or p-value) values.
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
#' x <- matrix(c(12, 41, 25, 33), ncol = 2)
#' pcal(chisq.test(x)[["p.value"]])
#'
#' @export
#' @importFrom Rdpack reprompt

pcal <- function(p, prior_prob = 0.5){

  check_prob(p)

  check_prior_prob(prior_prob)

  if(isTRUE(length(p) > 1) && isFALSE(length(prior_prob) %in% c(1, length(p)))){
    stop("Invalid argument: if length(p) > 1 then length(prior_prob) can only be 1 or equal to length(p)")
  }

  suppressWarnings(bfactor_to_prob(bcal(p), prior_prob))

}

