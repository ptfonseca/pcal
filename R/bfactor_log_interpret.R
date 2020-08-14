
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

  check_log_bf(bf)
  check_scale(scale)
  check_log_base(base)

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



