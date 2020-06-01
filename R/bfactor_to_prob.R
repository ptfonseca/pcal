
#' @title Posterior Probabilities of Point Null Hypotheses
#'
#' @description Update the prior probabilities of point null hypotheses to posterior probabilities using Bayes factors.
#'
#' @param bf A numeric vector of non-negative values.
#' @param prior_prob A numeric vector with values in the [0,1] interval. If \code{length(bf)} = 1 then \code{prior_prob} can be of any positive length, but if \code{length(bf)} > 1 then \code{length(prior_prob)} must be either 1 or equal to \code{length(bf)}.
#'
#' @details \code{bfactor_to_prob} computes the posterior probabilities of point null hypotheses using the following equation from \insertCite{bergerDelampady1987;textual}{pcal}: \deqn{Prob(H_0|data) = \left(1 + \frac{1 - {null\_prob}}{null\_prob} \times \frac{1}{bf}\right)^{-1}}{Prob(null | data) = (1 + (1 - prior_prob) / prior_prob * (1 / bf)) ^(-1)} where \code{bf} is a Bayes factor in favor of a point null hypothesis with prior probability \code{prior_prob}. The alternative hypothesis has prior probability 1-\code{prior_prob}. \code{prior_prob} can only be of \code{length} 1, in which case it will be recycled (if necessary) and hence every element of \code{bf} will combined with the same prior probability, or of \code{length} equal to \code{length(bf)}, in which case each prior probability in \code{prior_prob} will be updated using the corresponding element of \code{bf}. The optional argument \code{prior_prob} is set to 0.5 by default, implying prior equiprobability of hypotheses.
#'
#' @return If \code{length(bf)} > 1 then \code{bfactor_to_prob} returns a numeric vector with the same \code{length} as \code{bf}, otherwise it returns a numeric vector with the same \code{length} as \code{prior_prob}.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso \code{\link[pcal]{bcal}} for the calculation of Bayes factors.
#'
#' @examples
#' # With a Bayes factor that is indifferent between the null and the alternative:
#' bfactor_to_prob(1)
#'
#' # Same as above but the null has high prior probability:
#' bfactor_to_prob(1, .99)
#'
#' # Posterior probability of the null as a function of different prior probabilities:
#' bfactor_to_prob(1, seq(.5, 1, .1))
#'
#' # With Bayes factors that favor the null hypothesis:
#' round(bfactor_to_prob(seq(2, 50, 2.5)), 3)
#'
#' # Same as above but the null has low prior probability:
#' round(bfactor_to_prob(seq(2, 50, 2.5), prior_prob = .01), 3)
#'
#' # Posterior probabilities obtained with Bayes factors that favor the alternative:
#' round(bfactor_to_prob(seq(0, 1, .05)), 3)
#'
#' # Same as above but the null has high prior probability:
#' round(bfactor_to_prob(seq(0, 1, .05), prior_prob = .99), 3)
#'
#' # Application: chi-squared goodness-of-fit test,
#' # lower bound on the posterior probability of the null:
#' x <- matrix(c(12, 41, 25, 33), ncol = 2)
#' bfactor_to_prob(bcal(chisq.test(x)[["p.value"]]), prior_prob = .9)
#'
#' @importFrom Rdpack reprompt
#' @export

bfactor_to_prob <- function(bf, prior_prob = .5) {

  if(is.null(bf)){
    stop("Invalid argument: 'bf' is NULL.", call. = FALSE)
  }
  if(length(bf) == 0){
    stop("Invalid argument: 'bf' is empty", call. = FALSE)
  }
  if(all(is.na(bf))){
    stop("Invalid argument: all elements of 'bf' are NA or NaN.", call. = FALSE)
  }
  if(any(!is.numeric(bf), !is.vector(bf),  all(is.na(bf)))){
    stop("Invalid argument: 'bf' must be a numeric vector", call. = FALSE)
  }
  if(any(bf[!is.na(bf)] < 0)){
    stop("Invalid argument: 'bf' must be non-negative.", call. = FALSE)
  }
  if(any(is.na(bf))){
    warning("There are NA or NaN values in 'bf'.", call. = FALSE)
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
  if(any(!is.numeric(prior_prob), !is.vector(prior_prob),  all(is.na(prior_prob)))){
    stop("Invalid argument: 'prior_prob' must be a numeric vector", call. = FALSE)
  }
  if(any(prior_prob[!is.na(prior_prob)] < 0, prior_prob[!is.na(prior_prob)] > 1)){
    stop("Invalid argument: all elements of 'prior_prob' must be in the [0, 1] interval.", call. = FALSE)
  }
  if(isTRUE(length(bf) > 1) && isFALSE(length(prior_prob) %in% c(1, length(bf)))){
    stop("Invalid argument: if length(bf) > 1 then length(prior_prob) can only be 1 or equal to length(bf)", call. = FALSE)
  }
  (1 + (1 - prior_prob) / prior_prob * (1 / bf)) ^(-1)

}







