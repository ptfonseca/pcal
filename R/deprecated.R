
#' @title Interpretation of Bayes factors
#'
#' @description Quantify the strength of the evidence provided by the data to a model/hypothesis according to Bayes factor interpretation scales suggested by \insertCite{jeffreys1961;textual}{pcal} and \insertCite{kass1995;textual}{pcal}.
#'
#' @param bf A numeric vector of non-negative values.
#' @param scale A string of characters specifying either "jeffreys" or "kass-raftery". Not case sensitive.
#'
#' @details Bayes factors are a summary of the evidence provided by the data to a model/hypothesis. \insertCite{jeffreys1961;textual}{pcal} suggested the interpretation of Bayes factors in half-units on the base 10 logarithmic scale, as indicated in the following table:
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
#' By default, `bfactor_interpret` takes Bayes factors as input and returns the strength of the evidence in favor of the model/hypothesis in the numerator of the Bayes factors (usually the null hypothesis) according to the aforementioned table.
#'
#' Alternatively, and because it can be useful to consider twice the natural logarithm of the Bayes factor (which is in the same scale as the familiar deviance and likelihood ratio test statistics), \insertCite{kass1995;textual}{pcal} suggested the following scale:
#'
#' | **2*log(Bayes factor)** | **Bayes factor**      | **Evidence** |
#' | ---                     | ---                   | ---          |
#' | [-Inf, 0[               | [0, 1[                | Negative     |
#' | [0, 2[                  | [1, 3[                | Weak         |
#' | [2, 6[                  | [3, 20[               | Positive     |
#' | [6, 10[                 | [20, 150[             | Strong       |
#' | [10, +Inf[              | [150, +Inf[           | Very Strong  |
#'
#' To interpret Bayes factors according to the latter table use `scale = "kass-raftery"`.
#'
#' When comparing Bayes factors with results from standard likelihood ratio tests it is convenient to put the null hypothesis in the denominator of the Bayes factor so that `bfactor_interpret` returns the strength of the evidence against the null hypothesis. If `bf` was obtained with the null hypothesis on the numerator, one can use `bfactor_interpret(1/bf)` or `bfactor_interpret(1/bf, scale = "kass-raftery")` to obtain the strength of the evidence against the null hypothesis.
#'
#' @return Returns a character vector with the same {\link[base]{length}} as `bf`.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso
#' * \code{\link[pcal]{bfactor_log_interpret}} for the interpretation of the logarithms of Bayes factors.
#' * \code{\link[pcal]{bfactor_to_prob}} to turn Bayes factors into posterior probabilities.
#' * \code{\link[pcal]{bcal}} for a p-value calibration that returns lower bounds on Bayes factors in favor of point null hypotheses.
#'
#' @examples
#' # Interpretation of one Bayes factor
#' bfactor_interpret(4)
#' bfactor_interpret(4, scale = "jeffreys")
#' bfactor_interpret(4, scale = "kass-raftery")
#'
#' # Interpretation of more than one Bayes factors
#' bfactor_interpret(c(0.1, 1.2, 3.5, 13.9, 150))
#' bfactor_interpret(c(0.1, 1.2, 3.5, 13.9, 150), scale = "jeffreys")
#' bfactor_interpret(c(0.1, 1.2, 3.5, 13.9, 150), scale = "kass-raftery")
#'
#' # Application: chi-squared goodness-of-fit test.
#' # Strength of the evidence provided by the lower
#' # bound of the Bayes factor in favor of the null hypothesis:
#' data <- matrix(c(18, 12, 10, 12, 10, 23), ncol = 2)
#' bfactor_interpret(bcal(chisq.test(data)[["p.value"]]))
#'
#' @export

bfactor_interpret <- function(bf, scale = "jeffreys") {

  if (is.null(bf)){
    stop(paste("Invalid argument: bf is NULL."))
  }
  if (any(!is.numeric(bf), !is.vector(bf))){
    stop(paste("Invalid argument: bf must be a numeric vector."))
  }
  if (length(bf) == 0){
    stop(paste("Invalid argument: bf is empty."))
  }
  if (all(is.na(bf))){
    stop(paste("Invalid argument: all elements of bf are NA or NaN."))
  }
  if (any(bf[!is.na(bf)] < 0)){
    stop(paste("Invalid argument: all elements of bf must be non-negative."))
  }
  if (any(is.na(bf))){
    warning(paste("There are NA or NaN values in bf."))
  }

  if (is.null(scale)){
    stop(paste("Invalid argument: scale is NULL."))
  }
  if (any(!is.vector(scale), !is.atomic(scale), isFALSE(length(scale) == 1))){
    stop(paste("Invalid argument: scale must be an atomic vector of length 1."))
  }
  if (is.na(scale)){
    stop(paste("Invalid argument: scale is NA or NaN."))
  }
  if (!is.character(scale)){
    stop(paste("Invalid argument: the type of scale must be character."))
  }
  if (isFALSE(tolower(scale) %in% c("jeffreys", "kass-raftery"))){
    stop(paste("Invalid argument: scale must be either 'jeffreys' or 'kass-raftery'."))
  }

  switch(
    tolower(scale),
    "jeffreys" = ifelse(bf < 1, "Negative",
                        ifelse(bf < 3.2, "Weak",
                               ifelse(bf < 10, "Substantial",
                                      ifelse(bf < 32, "Strong",
                                             ifelse(bf < 100, "Very Strong",
                                                    "Decisive")
                                      )
                               )
                        )
    ),

    "kass-raftery" = ifelse(bf < 1, "Negative",
                            ifelse(bf < 3, "Weak",
                                   ifelse(bf < 20, "Positive",
                                          ifelse(bf < 150, "Strong",
                                                 "Very Strong")
                                   )
                            )
    )
  )
}

#' @title Interpretation of the logarithms of Bayes factors
#'
#' @description Quantify the strength of the evidence provided by the data to a model/hypothesis according to Bayes factor interpretation scales suggested by \insertCite{jeffreys1961;textual}{pcal} and \insertCite{kass1995;textual}{pcal}.
#'
#' @param bf A numeric vector.
#' @param scale A string of characters specifying either "jeffreys" or "kass-raftery". Not case sensitive.
#' @param base A numeric vector of \code{\link[base]{length}} one. Must be a positive number.
#'
#'
#' @details Bayes factors are a summary of the evidence provided by the data to a model/hypothesis, and are often reported on a logarithmic scale. \insertCite{jeffreys1961;textual}{pcal} suggested the interpretation of Bayes factors in half-units on the base 10 logarithmic scale, as indicated in the following table:
#'
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
#' By default, `bfactor_log_interpret` takes (base `base`) logarithms of Bayes factors as input and returns the strength of the evidence in favor of the model/hypothesis in the numerator of the Bayes factors (usually the null hypothesis) according to the aforementioned table.
#'
#' Alternatively, and because it can be useful to consider twice the natural logarithm of the Bayes factor (which is in the same scale as the familiar deviance and likelihood ratio test statistics), \insertCite{kass1995;textual}{pcal} suggested the following scale:
#'
#' | **2*log(Bayes factor)** | **Bayes factor**      | **Evidence** |
#' | ---                     | ---                   | ---          |
#' | [-Inf, 0[               | [0, 1[                | Negative     |
#' | [0, 2[                  | [1, 3[                | Weak         |
#' | [2, 6[                  | [3, 20[               | Positive     |
#' | [6, 10[                 | [20, 150[             | Strong       |
#' | [10, +Inf[              | [150, +Inf[           | Very Strong  |
#'
#' To interpret base `base` logarithms of Bayes factors according to the latter table use `scale = "kass-raftery"`.
#'
#' When comparing Bayes factors with results from standard likelihood ratio tests it is convenient to put the null hypothesis in the denominator of the Bayes factor so that `bfactor_log_interpret` returns the strength of the evidence against the null hypothesis. If `bf` was obtained with the null hypothesis on the numerator, one can use `bfactor_log_interpret(1/bf)` or `bfactor_log_interpret(1/bf, scale = "kass-raftery")` to obtain the strength of the evidence against the null hypothesis.
#'
#' @return Returns a character vector with the same  \code{\link[base]{length}} as `bf`.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso
#' * \code{\link[pcal]{bfactor_interpret}} for the interpretation of Bayes factors in levels.
#' * \code{\link[pcal]{bfactor_to_prob}} to turn Bayes factors into posterior probabilities.
#' * \code{\link[pcal]{bcal}} for a p-value calibration that returns lower bounds on Bayes factors in favor of point null hypotheses.
#'
#' @examples
#' # Interpretation of the natural log of a Bayes factors
#' bfactor_log_interpret(log(1.5))
#' bfactor_log_interpret(log(1.5), scale = "jeffreys")
#' bfactor_log_interpret(log(1.5), scale = "kass-raftery")
#'
#' # Interpretation of the natural log of many Bayes factors
#' bfactor_log_interpret(log(c(0.1, 1.2, 3.5, 13.9, 150)))
#' bfactor_log_interpret(log(c(0.1, 1.2, 3.5, 13.9, 150)), scale = "kass-raftery")
#'
#' # Interpretation of the base-10 log of many Bayes factors
#' bfactor_log_interpret(log10(c(0.1, 1.2, 3.5, 13.9, 150)), base = 10)
#' bfactor_log_interpret(log10(c(0.1, 1.2, 3.5, 13.9, 150)), base = 10, scale = "kass-raftery")
#'
#' @export

bfactor_log_interpret <- function(bf, scale = "jeffreys", base = exp(1)) {

  if (is.null(bf)){
    stop(paste("Invalid argument: bf is NULL."))
  }
  if (any(!is.numeric(bf), !is.vector(bf))){
    stop(paste("Invalid argument: bf must be a numeric vector."))
  }
  if (length(bf) == 0){
    stop(paste("Invalid argument: bf is empty."))
  }
  if (all(is.na(bf))){
    stop(paste("Invalid argument: all elements of bf are NA or NaN."))
  }
  if (any(is.na(bf))){
    warning(paste("There are NA or NaN values in bf."))
  }

  if (is.null(scale)){
    stop(paste("Invalid argument: scale is NULL."))
  }
  if (any(!is.vector(scale), !is.atomic(scale), isFALSE(length(scale) == 1))){
    stop(paste("Invalid argument: scale must be an atomic vector of length 1."))
  }
  if (is.na(scale)){
    stop(paste("Invalid argument: scale is NA or NaN."))
  }
  if (!is.character(scale)){
    stop(paste("Invalid argument: the type of scale must be character."))
  }
  if (isFALSE(tolower(scale) %in% c("jeffreys", "kass-raftery"))){
    stop(paste("Invalid argument: scale must be either 'jeffreys' or 'kass-raftery'."))
  }

  if (any(
    is.null(base),
    isFALSE(is.vector(base)),
    isFALSE(is.atomic(base)),
    isFALSE(is.numeric(base)),
    isFALSE(length(base) == 1),
    is.na(base))
  ){
    stop(paste("Invalid argument: base must be a numeric vector of length 1."))
  }
  if (isTRUE(base <= 0)){
    stop(paste("Invalid argument: base must be positive."))
  }

  bf <-  base ^ bf

  switch(
    tolower(scale),
    "jeffreys" = ifelse(bf < 1, "Negative",
                        ifelse(
                          bf < 3.2, "Weak",
                          ifelse(bf < 10, "Substantial",
                                 ifelse(
                                   bf < 32, "Strong",
                                   ifelse(bf < 100, "Very Strong",
                                          "Decisive")
                                 ))
                        )),
    "kass-raftery" =   ifelse(bf < 1, "Negative",
                              ifelse(
                                bf < 3, "Weak",
                                ifelse(bf < 20, "Positive",
                                       ifelse(bf < 150, "Strong",
                                              "Very Strong"))
                              ))
  )

}

# nocov start

#' @title Interpretation of Bayes factors
#'
#' @description Quantify the strength of the evidence provided by the data to a model/hypothesis according a Bayes factor interpretation scale suggested by \insertCite{kass1995;textual}{pcal}.
#'
#' @param bf A numeric vector of non-negative values.
#'
#' @details Bayes factors are a summary of the evidence provided by the data to a model/hypothesis. Because it can be useful to consider twice the natural logarithm of the Bayes factor, which is in the same scale as the familiar deviance and likelihood ratio test statistics, \insertCite{kass1995;textual}{pcal} suggested the following Bayes factor interpretation scale:
#'
#' | **2*log(Bayes factor)** | **Bayes factor**      | **Evidence** |
#' | ---                     | ---                   | ---          |
#' | [-Inf, 0[               | [0, 1[                | Negative     |
#' | [0, 2[                  | [1, 3[                | Weak         |
#' | [2, 6[                  | [3, 20[               | Positive     |
#' | [6, 10[                 | [20, 150[             | Strong       |
#' | [10, +Inf[              | [150, +Inf[           | Very strong  |
#'
#' `bfactor_interpret_kr` takes Bayes factors as input and returns the strength of the evidence in favor of the model/hypothesis in the numerator of the Bayes factors (usually the null hypothesis) according to the aforementioned table.
#'
#' When comparing results with those from standard likelihood ratio tests, it is convenient to put the null hypothesis in the denominator of the Bayes factor so that `bfactor_interpret_kr` returns the strength of the evidence against the null hypothesis. If `bf` was obtained with the null hypothesis on the numerator, one can use `bfactor_interpret_kr(1/bf)` to obtain the strength of the evidence against the null hypothesis.
#'
#' @return Returns a character vector with the same \code{\link[base]{length}} as `bf`.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso
#' * \code{\link[pcal]{bfactor_interpret}} for the original interpretation scale suggested by Harold Jeffreys.
#' * \code{\link[pcal]{bfactor_log_interpret}} and \code{\link[pcal]{bfactor_log_interpret_kr}} for the interpretation of the logarithms of Bayes factors.
#' * \code{\link[pcal]{bfactor_to_prob}} to turn Bayes factors into posterior probabilities.
#' * \code{\link[pcal]{bcal}} for a p-value calibration that returns lower bounds on Bayes factors in favor of point null hypotheses.
#'
#' @examples
#' # Interpretation of one Bayes factor
#' bfactor_interpret_kr(1.5)
#'
#' # Interpretation of many Bayes factors
#' bfactor_interpret_kr(c(0.1, 1.2, 3.5, 13.9, 150))
#'
#' # Application: chi-squared goodness-of-fit test.
#' # Strength of the evidence provided by the lower
#' # bound on the Bayes factor in favor of the null hypothesis:
#' x <- matrix(c(12, 15, 14, 15), ncol = 2)
#' bfactor_interpret_kr(bcal(chisq.test(x)[["p.value"]]))
#'
#' @export

bfactor_interpret_kr <- function(bf) {

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
                       ifelse(bf < 150, "Strong",
                              "Very Strong")
                )
         )
  )

}

#' @title Interpretation of the logarithms of Bayes factors
#'
#' @description Quantify the strength of the evidence provided by the data to a model/hypothesis according a Bayes factor interpretation scale suggested by \insertCite{kass1995;textual}{pcal}.
#'
#' @param bf A numeric vector.
#' @param base A numeric vector of \code{\link[base]{length}} one. Must be a positive number.
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
#' `bfactor_log_interpret_kr` takes (base `base`) logarithms of Bayes factors as input and returns the strength of the evidence provided by the data in favor of the  model/hypothesis in the numerator of the Bayes factors (usually the null hypothesis) according to the aforementioned table.
#'
#' When comparing results with those from standard likelihood ratio tests, it is convenient to put the null hypothesis in the denominator of the Bayes factor so that `bfactor_log_interpret_kr` returns the strength of the evidence against the null hypothesis. If `bf` was obtained with the null hypothesis on the numerator, one can use `bfactor_log_interpret_kr(1/bf)` to obtain the strength of the evidence against the null hypothesis.
#'
#' @return Returns a character vector with the same {\link[base]{length}} as `bf`.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso
#' * \code{\link[pcal]{bfactor_log_interpret}} for the original interpretation scale suggested by Harold Jeffreys
#' * \code{\link[pcal]{bfactor_interpret}} and \code{\link[pcal]{bfactor_interpret_kr}} for the interpretation of Bayes factors in levels.
#' * \code{\link[pcal]{bfactor_to_prob}} to turn Bayes factors into posterior probabilities.
#' * \code{\link[pcal]{bcal}} for a p-value calibration that returns lower bounds on Bayes factors in favor of point null hypotheses.
#'
#' @examples
#' # Interpretation of one Bayes factor, natural log
#' bfactor_log_interpret_kr(1.5)
#'
#' # Interpretation of many Bayes factors, natural log
#' bfactor_log_interpret_kr(c(0.1, 1.2, 3.5, 13.9, 150))
#'
#' # Interpretation of many Bayes factors, base 10 log
#' bfactor_log_interpret_kr(log10(c(0.1, 1.2, 3.5, 13.9, 150)), base = 10)
#'
#' @export

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

# nocov end





