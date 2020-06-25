
#' @title Interpretation of the logarithms of Bayes factors
#'
#' @description Quantify the strength of the evidence provided by the data to a model/hypothesis according to the Bayes factor interpretation scale from \insertCite{kass1995;textual}{pcal}.
#'
#' @param bf A numeric vector.
#' @param base A numeric vector of \code{length} one. Must be a positive number.
#'
#' @details Bayes factors are a summary of the evidence provided by the data in favor of a model/hypothesis, as opposed to another, and are often presented in a logarithmic scale. \code{bfactor_log_interpret} takes (base \code{base}) logarithms of Bayes factors (\code{bf}) as input and returns the strength of the evidence provided by the data in favor of the  model/hypothesis in the numerator of the Bayes factors (usually the null hypothesis) according to the scale suggested by \insertCite{kass1995;textual}{pcal}. The original scale was augmented with an additional category for the case when the data provides more support to the alternative hypothesis:
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
#' * \code{\link[pcal]{bfactor_interpret_jeffreys}} and \code{\link[pcal]{bfactor_interpret_kass}} for the interpretation of Bayes factors in levels.
#' * \code{\link[pcal]{bcal}} for the calculation of Bayes factors.
#' * \code{\link[pcal]{bfactor_to_prob}} to turn Bayes factors into posterior probabilities.
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
#' @details Bayes factors are a summary of the evidence provided by the data in favor of a model/hypothesis, as opposed to another, and are often presented in a logarithmic scale. \code{bfactor_log_interpret} takes (base \code{base}) logarithms of Bayes factors (\code{bf}) as input and returns the strength of the evidence provided by the data in favor of the  model/hypothesis in the numerator of the Bayes factors (usually the null hypothesis) according to the scale suggested by \insertCite{kass1995;textual}{pcal}. The original scale was augmented with an additional category for the case when the data provides more support to the alternative hypothesis:
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
#' * \code{\link[pcal]{bfactor_interpret_jeffreys}} and \code{\link[pcal]{bfactor_interpret_kass}} for the interpretation of Bayes factors in levels.
#' * \code{\link[pcal]{bcal}} for the calculation of Bayes factors.
#' * \code{\link[pcal]{bfactor_to_prob}} to turn Bayes factors into posterior probabilities.
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
                       ifelse(bf < 100, "Strong",
                              "Decisive")
                )
         )
  )

}




