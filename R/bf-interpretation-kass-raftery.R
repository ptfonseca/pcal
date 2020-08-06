
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
#' * \code{\link[pcal]{check_bf}} to check if an object is a numeric vector of valid Bayes factor values.
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

  check_bf(bf)

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
#' @param lbf A numeric vector.
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
#' When comparing results with those from standard likelihood ratio tests, it is convenient to put the null hypothesis in the denominator of the Bayes factor so that `bfactor_log_interpret_kr` returns the strength of the evidence against the null hypothesis. If `lbf` was obtained with the null hypothesis on the numerator, one can use `bfactor_log_interpret_kr(1/lbf)` to obtain the strength of the evidence against the null hypothesis.
#'
#' @return Returns a character vector with the same {\link[base]{length}} as `lbf`.
#'
#' @references
#' \insertAllCited{}
#'
#' @seealso
#' * \code{\link[pcal]{check_log_bf}} to check if an object is a numeric vector of valid logarithmic Bayes factor values.
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

bfactor_log_interpret_kr <- function(lbf, base = exp(1)) {

  check_log_bf(lbf)

  check_log_base(base)

  bf <-  base ^ lbf

  ifelse(bf < 1, "Negative",
         ifelse(bf < 3, "Weak",
                ifelse(bf < 20, "Positive",
                       ifelse(bf < 150, "Strong",
                              "Very Strong")
                )
         )
  )

}
