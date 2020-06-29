
#' @title Interpretation of the logarithms of Bayes factors
#'
#' @description Quantify the strength of the evidence provided by the data to a model/hypothesis according to the Bayes factor interpretation scale suggested by \insertCite{jeffreys1961;textual}{pcal}.
#'
#' @param bf A numeric vector.
#' @param base A numeric vector of `length` one. Must be a positive number.
#'
#' @details Bayes factors are a summary of the evidence provided by the data to a model/hypothesis, and are often reported on a logarithmic scale. \insertCite{jeffreys1961;textual}{pcal} suggested the interpretation of Bayes factors in half-units on the base 10 logarithmic scale, as indicated in the following table:
#'
#' **log10(Bayes factor)**  | **Bayes factor** | **Evidence** |
#' | ---                    | ---              | ---          |
#' | [-Inf, 0[              | [0, 1[           | Negative     |
#' | [0, 0.5[               | [1, 3.2[         | Weak         |
#' | [0.5, 1[               | [3.2, 10[        | Substantial  |
#' | [1, 1.5[               | [10, 32[         | Strong       |
#' | [1.5, 2[               | [32, 100[        | Very Strong  |
#' | [2, +Inf[              | [100, +Inf[      | Decisive     |
#'
#' `bfactor_log_interpret` takes (base `base`) logarithms of Bayes factors as input and returns the strength of the evidence provided by the data in favor of the  model/hypothesis in the numerator of the Bayes factors (usually the null hypothesis) according to the according to the aforementioned table.
#'
#' When comparing results with those from standard likelihood ratio tests, it is convenient to put the null hypothesis in the denominator of the Bayes factor so that `bfactor_log_interpret` returns the strength of the evidence against the null hypothesis. If `bf` was obtained with the null hypothesis on the numerator, one can use `bfactor_log_interpret(1/bf)` to obtain the strength of the evidence against the null hypothesis.
#'
#' @return Returns a character vector with the same `length` as `bf`.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso
#' * \code{\link[pcal]{bfactor_log_interpret_kr}}for an alternative interpretation scale.
#' * \code{\link[pcal]{bfactor_interpret}}and \code{\link[pcal]{bfactor_interpret_kr}}for the interpretation of Bayes factors in levels.
#' * \code{\link[pcal]{bfactor_to_prob}}to turn Bayes factors into posterior probabilities.
#' * \code{\link[pcal]{bcal}}for a p-value calibration that returns lower bounds on Bayes factors in favor of point null hypotheses.
#'
#' @examples
#' # Interpretation of one Bayes factor (on the natural log scale)
#' bfactor_log_interpret(log(1.5))
#'
#' # Interpretation of many Bayes factors (on the natural log scale)
#' bfactor_log_interpret(log(c(0.1, 1.2, 3.5, 13.9, 150)))
#'
#' # Interpretation of many Bayes factors (on the log10 scale)
#' bfactor_log_interpret(log10(c(0.1, 1.2, 3.5, 13.9, 150)), base = 10)
#'
#' @export

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
