
#' @title Interpretation of Bayes factors
#'
#' @description Quantify the strength of the evidence provided by the data to a model/hypothesis according to the Bayes factor interpretation scale from \insertCite{jeffreys1961;textual}{pcal}.
#'
#' @param bf A numeric vector of non-negative values.
#'
#' @details Bayes factors are a summary of the evidence provided by the data to a model/hypothesis. Following the suggestion by \insertCite{jeffreys1961;textual}{pcal} of interpreting Bayes factors in half-units on the base 10 logarithmic scale, \code{bfactor_interpret} takes Bayes factors (\code{bf}) as input and returns the strength of the evidence in favor of the model/hypothesis in the numerator of the Bayes factors (usually the null hypothesis) according to the following table:
#'
#' | **bf**       | **Evidence** |
#' | ---          | ---          |
#' | [0, 1[       | Negative     |
#' | [1, 3.2[     | Weak         |
#' | [3.2, 10[    | Substantial  |
#' | [10, 100[    | Strong       |
#' | [100, +Inf)  | Decisive     |
#'
#' Two of the original categories from \insertCite{jeffreys1961;textual}{pcal} were pooled together for simplification and a new category for the case when the data provides more support to the model/hypothesis in the denominator of the Bayes factor (usually the alternative hypothesis) was added. When comparing results with standard likelihood ratio tests, it is convenient to put the null hypothesis in the denominator of the Bayes factor. To obtain the strength of the evidence against the model/hypothesis  in the numerator use 1/\code{bf} instead of \code{bf}.
#'
#' @return Returns a character vector with the same \code{length} as \code{bf}.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso
#' * \code{\link[pcal]{bfactor_interpret_2}} for an alternative Bayes factor interpretation scale.
#' * \code{\link[pcal]{bfactor_log_interpret}} to interpret the logarithms of Bayes factors.
#' * \code{\link[pcal]{bfactor_to_prob}} to turn Bayes factors into posterior probabilities.
#' * \code{\link[pcal]{bcal}} for the calculation of lower bounds on Bayes factors.
#'
#' @examples
#' # Interpret one Bayes factor
#' bfactor_interpret(1.5)
#'
#' # Interpret many Bayes factors
#' bfactor_interpret(c(0.1, 1.2, 3.5, 13.9, 150))
#'
#' # Application: chi-squared goodness-of-fit test.
#' # Strength of the evidence provided by the lower
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


#' @title Interpretation of Bayes factors
#'
#' @description Quantify the strength of the evidence provided by the data to a model/hypothesis according to the Bayes factor interpretation scales from \insertCite{kass1995;textual}{pcal}.
#'
#' @param bf A numeric vector of non-negative values.
#'
#' @details Bayes factors are a summary of the evidence provided by the data to a model/hypothesis. \code{bfactor_interpret} takes Bayes factors (\code{bf}) as input and returns the strength of the evidence provided by the data to the  model/hypothesis in the numerator of the Bayes factors (usually the null hypothesis) according to the following scale suggested by \insertCite{kass1995;textual}{pcal}:
#'
#' | bf           | log10(bf) | Evidence    |
#' | ---          | ---       | ---         |
#' | [0, 1[       | (-Inf, 0[ | Negative    |
#' | [1, 3.2[     | [0, 0.5[  | Weak        |
#' | [3.2, 10[    | [0.5, 1[  | Substantial |
#' | [10, 100[    | [1, 2[    | Strong      |
#' | [100, +Inf)  | [2, +Inf) | Decisive    |
#'
#' This scale follows the suggestion by \insertCite{jeffreys1961;textual}{pcal} of interpreting Bayes factors in half-units on the base-10 logarithmic scale. Two of the original categories from \insertCite{jeffreys1961;textual}{pcal} were pooled together for simplification and a new category for the case when the data provides more support to the alternative model/hypothesis was added.
#'
#' \insertCite{kass1995;textual}{pcal} note that sometimes it may be useful to consider twice the natural logarithm of the Bayes factor, which is in the same scale as the familiar deviance and likelihood ratio test statistics. \code{bfactor_interpret_2} is similar to \code{bfactor_interpret} but instead classifies Bayes factors using the following scale:
#'
#' | bf           | 2 log(bf)  | Evidence    |
#' | ---          | ---        | ---         |
#' | [0, 1[       | (-Inf, 0[  | Negative    |
#' | [1, 3[       | [0, 2[     | Weak        |
#' | [3, 20[      | [2, 6[     | Positive    |
#' | [20, 150[    | [6, 10[    | Strong      |
#' | [150, +Inf)  | [10, +Inf) | Very strong |
#'
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
#' * \code{\link[pcal]{bfactor_to_prob}} to turn Bayes factors into posterior probabilities.
#' * \code{\link[pcal]{bcal}} for the calculation of lower bounds on Bayes factors.
#'
#' @examples
#' # Interpret one Bayes factor
#' bfactor_interpret(1.5)
#' bfactor_interpret_2(1.5)
#'
#' # Interpret many Bayes factors
#' bfactor_interpret(c(0.1, 1.2, 3.5, 13.9, 150))
#' bfactor_interpret_2(c(0.1, 1.2, 3.5, 13.9, 150))
#'
#' # Application: chi-squared goodness-of-fit test.
#' # Strength of the evidence provided by the lower
#' # bound on the Bayes factor in favor of the null:
#' x <- matrix(c(12, 15, 14, 15), ncol = 2)
#' bfactor_interpret(bcal(chisq.test(x)[["p.value"]]))
#' bfactor_interpret_2(bcal(chisq.test(x)[["p.value"]]))
#'
#' @export
#'

bfactor_interpret_2 <- function(bf) {

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


