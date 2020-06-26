
#' @title Interpretation of the logarithms of Bayes factors \insertCite{jeffreys1961}{pcal}
#'
#' @description Quantify the strength of the evidence provided by the data to a model/hypothesis according to the Bayes factor interpretation scale suggested by \insertCite{jeffreys1961;textual}{pcal}.
#'
#' @param bf A numeric vector.
#' @param base A numeric vector of \code{length} one. Must be a positive number.
#'
#' @details Bayes factors are a summary of the evidence provided by the data to a model/hypothesis, and are often reported on a logarithmic scale. \insertCite{jeffreys1961;textual}{pcal} suggested the interpretation of Bayes factors in half-units on the base 10 logarithmic scale, as indicated in the following table:
#'
#' **log10(Bayes factor)**  | **Bayes factor** | **Evidence** |
#' | ---                    | ---              | ---          |
#' | [-Inf, 0[              | [0, 1[           | Negative     |
#' | [0, 0.5[               | [1, 3.2[         | Weak         |
#' | [0.5, 1[               | [3.2, 10[        | Substantial  |
#' | [1, 1.5[               | [10, 32[         | Strong       |
#' | [1.5, 2[               | [10, 100[        | Very Strong  |
#' | [2, +Inf[              | [100, +Inf[      | Decisive     |
#'
#' \code{bfactor_log_interpret} takes (base \code{base}) logarithms of Bayes factors (\code{bf}) as input and returns the strength of the evidence provided by the data in favor of the  model/hypothesis in the numerator of the Bayes factors (usually the null hypothesis) according to the according to the aforementioned table.
#'
#' When comparing results with those from standard likelihood ratio tests, it is convenient to put the null hypothesis in the denominator of the Bayes factor so that \code{bfactor_log_interpret} returns the strength of the evidence against the null hypothesis. If \code{bf} was obtained with the null hypothesis on the numerator, one can use \code{bfactor_log_interpret}(1/bf) to obtain the strength of the evidence against the null hypothesis.
#'
#' @return Returns a character vector with the same \code{length} as \code{bf}.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso
#' * \code{\link[pcal]{bfactor_log_interpret_kr}} for an alternative interpretation scale suggested by \insertCite{kass1995;textual}{pcal}.
#' * \code{\link[pcal]{bfactor_interpret}} and \code{\link[pcal]{bfactor_interpret_kr}} for the interpretation of Bayes factors in levels.
#' * \code{\link[pcal]{bfactor_to_prob}} to turn Bayes factors into posterior probabilities.
#' * \code{\link[pcal]{bcal}} for a p-value calibration that returns lower bounds on Bayes factors in favor of point null hypotheses.
#'
#' @examples
#' # Interpret one Bayes factor, natural log
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
                       ifelse(bf < 32, "Strong",
                              ifelse(bf < 100, "Very Strong",
                                     "Decisive")
                       )
                )
         )
  )

}

#' @title Interpretation of the logarithms of Bayes factors \insertCite{kass1995}{pcal}
#'
#' @description Quantify the strength of the evidence provided by the data to a model/hypothesis according a Bayes factor interpretation scale suggested by \insertCite{kass1995;textual}{pcal}.
#'
#' @param bf A numeric vector.
#' @param base A numeric vector of \code{length} one. Must be a positive number.
#'
#' @details Bayes factors are a summary of the evidence provided by the data to a model/hypothesis, and are often reported on a logarithmic scale. Because it can be useful to consider twice the natural logarithm of the Bayes factor, which is in the same scale as the familiar deviance and likelihood ratio test statistics, \insertCite{kass1995;textual}{pcal} suggested the following Bayes factor interpretation scale:
#'
#' | **2*log(Bayes factor)** | **Bayes factor**      | **Evidence** |
#' | ---                     | ---                   | ---          |
#' | [-Inf, 0[               | [0, 1[                | Negative     |
#' | [0, 2[                  | [1, 3[                | Weak         |
#' | [2, 6[                  | [3, 20[               | Positive     |
#' | [6, 10[                 | [20, 150[             | Strong       |
#' | [10, +Inf[              | [150, +Inf[           | Very strong  |
#'
#' \code{bfactor_log_interpret_kr} takes (base \code{base}) logarithms of Bayes factors (\code{bf}) as input and returns the strength of the evidence provided by the data in favor of the  model/hypothesis in the numerator of the Bayes factors (usually the null hypothesis) according to the scale suggested by \insertCite{kass1995;textual}{pcal}.
#'
#' When comparing results with those from standard likelihood ratio tests, it is convenient to put the null hypothesis in the denominator of the Bayes factor so that \code{bfactor_log_interpret_kr} returns the strength of the evidence against the null hypothesis. If \code{bf} was obtained with the null hypothesis on the numerator, one can use \code{bfactor_log_interpret_kr}(1/bf) to obtain the strength of the evidence against the null hypothesis.
#'
#' @return Returns a character vector with the same \code{length} as \code{bf}.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso
#' * \code{\link[pcal]{bfactor_log_interpret}} for the interpretation scale suggested by \insertCite{jeffreys1961;textual}{pcal}.
#' * \code{\link[pcal]{bfactor_interpret}} and \code{\link[pcal]{bfactor_interpret_kr}} for the interpretation of Bayes factors in levels.
#' * \code{\link[pcal]{bfactor_to_prob}} to turn Bayes factors into posterior probabilities.
#' * \code{\link[pcal]{bcal}} for a p-value calibration that returns lower bounds on Bayes factors in favor of point null hypotheses.
#'
#' @examples
#' # Interpret one Bayes factor, natural log
#' bfactor_log_interpret_kr(1.5)
#'
#' # Interpret many Bayes factors, natural log
#' bfactor_log_interpret_kr(c(0.1, 1.2, 3.5, 13.9, 150))
#'
#' # Interpret many Bayes factors, base 10 log
#' bfactor_log_interpret_kr(log10(c(0.1, 1.2, 3.5, 13.9, 150)), base = 10)
#'
#' @export
#'

bfactor_log_interpret_kr <- function(bf, base = exp(1)) {

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
         ifelse(bf < 3, "Weak",
                ifelse(bf < 20, "Positive",
                       ifelse(bf < 150, "Strong",
                              "Very Strong")
                )
         )
  )

}




