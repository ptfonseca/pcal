

#' @title Posterior Probabilities of Point Null Hypotheses
#'
#' @description Update the prior probabilities of point null hypotheses to posterior probabilities using Bayes factors.
#'
#' @param bf A numeric vector of non-negative values.
#' @param prior_prob A numeric vector with values in the \[0,1\] interval. If \code{length(bf)} = 1 then \code{prior_prob} can be of any positive length, but if \code{length(bf)} > 1 then \code{length(prior_prob)} can only be 1 or equal to \code{length(bf)}.
#'
#' @details \code{bfactor_to_prob} computes posterior probabilities of point null hypotheses using the following equation from \insertCite{bergerDelampady1987;textual}{pcal}: \deqn{P(null hypothesis|data) = \left(1 + \frac{1 - {null\_prob}}{null\_prob} \times \frac{1}{bf}\right)^{-1}}{P(null hypothesis | data) = (1 + (1 - prior_prob) / prior_prob * (1 / bf)) ^(-1)} where \code{bf} is a Bayes factor in favor of the null hypothesis and \code{prior_prob} is the prior probability of the null hypothesis. The alternative hypothesis has prior probability 1 - \code{prior_prob}. The \code{prior_prob} argument is optional and is set to 0.5 by default, implying prior equiprobability of hypotheses. \code{prior_prob} can only be of \code{length} equal to \code{length(bf)}, in which case each prior probability in \code{prior_prob} will be updated using the corresponding element of \code{bf}, or of \code{length} 1, in which case it will be recycled (if \code{length(bf)} > 1) and each element of \code{bf} will update the same \code{prior_prob} value.
#'
#' @return If \code{length(bf)} > 1 then \code{bfactor_to_prob} returns a numeric vector with the same \code{length} as \code{bf}, otherwise it returns a numeric vector with the same \code{length} as \code{prior_prob}.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso
#' * \code{\link[pcal]{bcal}} for the calculation of Bayes factors.
#' * \code{\link[pcal]{bfactor_interpret}} to interpret of Bayes factors.
#' * \code{\link[pcal]{bfactor_log_interpret}} to interpret the logarithms of Bayes factors.
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
#' @export

bfactor_to_prob <- function(bf, prior_prob = .5) {

  if(is.null(bf)){
    stop("Invalid argument: 'bf' is NULL.")
  }
  if(length(bf) == 0){
    stop("Invalid argument: 'bf' is empty")
  }
  if(all(is.na(bf))){
    stop("Invalid argument: all elements of 'bf' are NA or NaN.")
  }
  if(any(!is.numeric(bf), !is.vector(bf),  all(is.na(bf)))){
    stop("Invalid argument: 'bf' must be a numeric vector")
  }
  if(any(bf[!is.na(bf)] < 0)){
    stop("Invalid argument: 'bf' must be non-negative.")
  }
  if(any(is.na(bf))){
    warning("There are NA or NaN values in 'bf'.")
  }
  if(is.null(prior_prob)){
    stop("Invalid argument: 'prior_prob' is NULL")
  }
  if(length(prior_prob) == 0){
    stop("Invalid argument: 'prior_prob' is empty")
  }
  if(any(is.na(prior_prob))){
    stop("Invalid argument: There are NA or NaN values in 'prior_prob'")
  }
  if(any(!is.numeric(prior_prob), !is.vector(prior_prob),  all(is.na(prior_prob)))){
    stop("Invalid argument: 'prior_prob' must be a numeric vector")
  }
  if(any(prior_prob[!is.na(prior_prob)] < 0, prior_prob[!is.na(prior_prob)] > 1)){
    stop("Invalid argument: all elements of 'prior_prob' must be in the [0, 1] interval.")
  }
  if(isTRUE(length(bf) > 1) && isFALSE(length(prior_prob) %in% c(1, length(bf)))){
    stop("Invalid argument: if length(bf) > 1 then length(prior_prob) can only be 1 or equal to length(bf)")
  }

  (1 + (1 - prior_prob) / prior_prob * (1 / bf)) ^(-1)

}

#' @title Interpretation of Bayes factors
#'
#' @description Quantify the strength of the evidence provided by the data to a model/hypothesis according to the Bayes factor interpretation scale from \insertCite{kass1995;textual}{pcal}.
#'
#' @param bf A numeric vector of non-negative values.
#'
#' @details Bayes factors are a summary of the evidence provided by the data in favor of a model/hypothesis. \code{bfactor_interpret} takes Bayes factors (\code{bf}) as input and returns the strength of the evidence provided by the data in favor of the  model/hypothesis in the numerator of the Bayes factors (usually the null hypothesis) according to the scale suggested by \insertCite{kass1995;textual}{pcal}. The original scale was augmented with an additional category for the case when the data provides more support to the alternative hypothesis:
#'
#' | Bayes factor      | Evidence                |
#' | ---               |    ---                  |
#' | (-Inf, 1[         | Negative                |
#' | [1, 3.2[          | Weak                    |
#' | [3.2, 10[         | Substantial             |
#' | [10, 100[         | Strong                  |
#' | [100, +Inf)       | Decisive                |
#'
#' To obtain the strength of the evidence against the null hypothesis use 1/\code{bf} instead of \code{bf}, which is equivalent to putting the null hypothesis in the denominator of the Bayes factor. This is useful for comparing results with standard likelihood ratio tests.
#'
#' @return Returns a character vector with the same \code{length} as \code{bf}.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso
#' * \code{\link[pcal]{bfactor_log_interpret}} to interpret the logarithms of Bayes factors.
#' * \code{\link[pcal]{bcal}} for the calculation of Bayes factors.
#' * \code{\link[pcal]{bfactor_to_prob}} to turn Bayes factors into posterior probabilities.
#'
#' @examples
#' # Interpret one Bayes factor
#' bfactor_interpret(1.5)
#'
#' # Interpret many Bayes factors
#' bfactor_interpret(c(0.1, 1.2, 3.5, 13.9, 150))
#'
#' # Application: chi-squared goodness-of-fit test,
#' # strength of the evidence provided by the lower
#' # bound on the Bayes factor in favor of the null:
#' x <- matrix(c(12, 15, 14, 15), ncol = 2)
#' bfactor_interpret(bcal(chisq.test(x)[["p.value"]]))
#'
#' @export
#'

bfactor_interpret <- function(bf) {

  if(is.null(bf)){
    stop("Invalid argument: 'bf' is NULL.")
  }
  if(length(bf) == 0){
    stop("Invalid argument: 'bf' is empty")
  }
  if(all(is.na(bf))){
    stop("Invalid argument: all elements of 'bf' are NA or NaN.")
  }
  if(any(!is.numeric(bf), !is.vector(bf),  all(is.na(bf)))){
    stop("Invalid argument: 'bf' must be a numeric vector")
  }
  if(any(bf[!is.na(bf)] < 0)){
    stop("Invalid argument: 'bf' must be non-negative.")
  }
  if(any(is.na(bf))){
    warning("There are NA or NaN values in 'bf'.")
  }

  ifelse(bf < 1, "Negative",
         ifelse(bf < 3.2, "Weak",
                ifelse(bf < 10, "Substantial",
                       ifelse(bf < 100, "Strong",
                              "Decisive")
                       )
                )
         )

}

#' @title Interpretation of the logarithms of Bayes factors
#'
#' @description Quantify the strength of the evidence provided by the data to a model/hypothesis according to the Bayes factor interpretation scale from \insertCite{kass1995;textual}{pcal}.
#'
#' @param bf A numeric vector.
#' @param base A numeric vector of \code{length} one. Must be a positive number.
#'
#' @details Bayes factors are a summary of the evidence provided by the data in favor of a model/hypothesis and are often presented in a logarithmic scale. \code{bfactor_log_interpret} takes (base \code{base}) logarithms of Bayes factors (\code{bf}) as input and returns the strength of the evidence provided by the data in favor of the  model/hypothesis in the numerator of the Bayes factors (usually the null hypothesis) according to the scale suggested by \insertCite{kass1995;textual}{pcal}. The original scale was augmented with an additional category for the case when the data provides more support to the alternative hypothesis:
#'
#' | Bayes factor      | Evidence                |
#' | ---               |    ---                  |
#' | (-Inf, 1[         | Negative                |
#' | [1, 3.2[          | Weak                    |
#' | [3.2, 10[         | Substantial             |
#' | [10, 100[         | Strong                  |
#' | [100, +Inf)       | Decisive                |
#'
#' To obtain the strength of the evidence against the null hypothesis use 1/\code{bf} instead of \code{bf}, which is equivalent to putting the null hypothesis in the denominator of the Bayes factor. This is useful for comparing results with standard likelihood ratio tests.
#'
#' @return Returns a character vector with the same \code{length} as \code{bf}.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso
#' * \code{\link[pcal]{bfactor_interpret}} to interpret of Bayes factors in levels.
#' * \code{\link[pcal]{bcal}} for the calculation of Bayes factors.
#' * \code{\link[pcal]{bfactor_to_prob}} to turn Bayes factors into posterior probabilities.
#'
#' @examples
#' # Interpret one Bayes factor
#' bfactor_log_interpret(1.5)
#'
#' # Interpret many Bayes factors, natural log
#' bfactor_log_interpret(c(0.1, 1.2, 3.5, 13.9, 150))
#'
#' # Interpret many Bayes factors, base 10 log
#' bfactor_log_interpret(log10(c(0.1, 1.2, 3.5, 13.9, 150)), base = 10)
#'
#' @export
#'

bfactor_log_interpret <- function(bf, base = exp(1)) {

  if(is.null(bf)){
    stop("Invalid argument: 'bf' is NULL.")
  }
  if(length(bf) == 0){
    stop("Invalid argument: 'bf' is empty")
  }
  if(all(is.na(bf))){
    stop("Invalid argument: all elements of 'bf' are NA or NaN.")
  }
  if(any(!is.numeric(bf), !is.vector(bf),  all(is.na(bf)))){
    stop("Invalid argument: 'bf' must be a numeric vector")
  }
  if(any(is.na(bf))){
    warning("There are NA or NaN values in 'bf'.")
  }
  if(any(is.null(base), is.na(base), isFALSE(is.numeric(base)), isFALSE(is.vector(base)), isFALSE(length(base) == 1))){
    stop("Invalid argument: 'base' must be a numeric vector of length 1")
  }

  bf <-  base ^ bf

  ifelse(bf < 1, "Negative",
         ifelse(bf < 3.2, "Weak",
                ifelse(bf < 10, "Substantial",
                       ifelse(bf < 100, "Strong",
                              "Decisive")
                            )
                     )
  )

}




