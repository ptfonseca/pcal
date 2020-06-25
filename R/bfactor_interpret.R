
#' @title Interpretation of Bayes factors
#'
#' @description Quantify the strength of the evidence provided by the data to a model/hypothesis according to Bayes factor interpretation scales suggested by \insertCite{jeffreys1961;textual}{pcal} and \insertCite{kass1995;textual}{pcal}.
#'
#' @param bf A numeric vector of non-negative values.
#'
#' @details Bayes factors are a summary of the evidence provided by the data to a model/hypothesis. Following Jeffreys' (1961) suggestion about the interpretation of Bayes factors in half-units on the base 10 logarithmic scale, \code{bfactor_interpret_jeffreys} takes Bayes factors (\code{bf}) as input and returns the strength of the evidence in favor of the model/hypothesis in the numerator of the Bayes factors (usually the null hypothesis) according to the following table:
#'
#' **log10(bf)**  | **bf**       | **Evidence** |
#' | ---          | ---          | ---          |
#' | [-Inf, 0[    | [0, 1[       | Negative     |
#' | [0, 0.5[     | [1, 3.2[     | Weak         |
#' | [0.5, 1[     | [3.2, 10[    | Substantial  |
#' | [1, 1.5[     | [10, 32[     | Strong       |
#' | [1.5, 2[     | [10, 100[    | Very Strong  |
#' | [2, +Inf[    | [100, +Inf[  | Decisive     |
#'
#' Because \insertCite{kass1995;textual}{pcal} noted that it can be useful to consider twice the natural logarithm of the Bayes factor, which is in the same scale as the familiar deviance and likelihood ratio test statistics, there is also \code{bfactor_interpret_kass} which instead uses this scale:
#'
#' | **2*log((bf)** | **bf**      | **Evidence** |
#' | ---            | ---         | ---          |
#' | [-Inf, 0[      | [0, 1[      | Negative     |
#' | [0, 2[         | [1, 3[      | Weak         |
#' | [2, 6[         | [3, 20[     | Positive     |
#' | [6, 10[        | [20, 150[   | Strong       |
#' | [10, +Inf[     | [150, +Inf[ | Very strong  |
#'
#' When comparing results with those from standard likelihood ratio tests, it is convenient to put the null hypothesis in the denominator of the Bayes factor so that \code{bfactor_interpret_jeffreys} returns the strength of the evidence against the null hypothesis. In that case, and if \code{bf} was obtained with the null hypothesis on the numerator, one can use 1/\code{bf} instead.
#'
#' @return Returns a character vector with the same \code{length} as \code{bf}.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso
#' * \code{\link[pcal]{bfactor_log_interpret}} for the interpretation of the logarithms of Bayes factors.
#' * \code{\link[pcal]{bfactor_to_prob}} to turn Bayes factors into posterior probabilities.
#' * \code{\link[pcal]{bcal}} for the calculation of lower bounds on Bayes factors.
#'
#' @examples
#' # Interpretation of one Bayes factor
#'
#' bfactor_interpret_jeffreys(1.5)
#' bfactor_interpret_kass(1.5)
#'
#' # Interpretation of many Bayes factors
#'
#' bfactor_interpret_jeffreys(c(0.1, 1.2, 3.5, 13.9, 150))
#' bfactor_interpret_kass(c(0.1, 1.2, 3.5, 13.9, 150))

#'
#' # Application: chi-squared goodness-of-fit test.
#' # Strength of the evidence provided by the lower
#' # bound on the Bayes factor in favor of the null
#' # hypothesis:
#'
#' x <- matrix(c(12, 15, 14, 15), ncol = 2)
#' bfactor_interpret_jeffreys(bcal(chisq.test(x)[["p.value"]]))
#' bfactor_interpret_kass(bcal(chisq.test(x)[["p.value"]]))
#'
#' @export

bfactor_interpret_jeffreys <- function(bf) {

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
                       ifelse(bf < 32, "Strong",
                              ifelse(bf < 100, "Very Strong",
                                     "Decisive")
                              )
                       )
                )
         )

}

#' @rdname bfactor_interpret_jeffreys
#' @export

bfactor_interpret_kass <- function(bf) {

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
         ifelse(bf < 3, "Weak",
                ifelse(bf < 20, "Positive",
                       ifelse(bf < 100, "Strong",
                              "Very Strong")
                )
         )
  )

}


