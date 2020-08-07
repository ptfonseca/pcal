
#' @title Check if an object is a vector of valid probability values
#'
#' @description `check_prob` checks if an object is a numeric vector of valid probability values. This can be useful to validate inputs or intermediate calculations in user-defined functions or to validate the output of calculations/functions.
#'
#' @param p An arbitrary object.
#'
#' @details `check_prob` conducts a series of tests to check if `p` is a numeric vector of valid probability values. Namely, `check_prob` checks if:
#' * `p` is `NULL` or empty.
#' * `p` is a numeric (atomic) vector.
#' * `p` has `NA` or `NaN` values.
#' *  The values of `p` are in the \[0, 1\] interval.
#'
#' @return `check_prob` does not return any output. There are three possible scenarios:
#' * The call is silent if `p` is a numeric vector of valid probability values and there are no `NA` or `NaN` values.
#' * An informative warning message is given if `p` is a numeric vector of valid probability values and there are `NA` or `NaN` values.
#' * An informative error message is thrown if `p` is not a numeric vector of valid probability values. This will \code{\link[base]{stop}} the execution (for example, when using `check_prob` to validate the inputs of user defined functions).
#'
#' @seealso
#' * \code{\link[pcal]{check_bf}} to check if an object is a numeric vector of valid Bayes factor values.
#' * \code{\link[pcal]{check_log_bf}} to check if an object is a numeric vector of valid logarithmic Bayes factor values.
#'
#' @examples
#' # Calls that pass silently:
#' check_prob(0)
#' check_prob(0.5)
#' check_prob(1)
#' check_prob(c(0.1, 0.2, 0.3, 0.4, 0.5))
#'
#' # Call that throws an informative warning message:
#' \dontrun{check_prob(c(0.1, 0.2, NA, 0.4, 0.5))}
#'
#' # Calls that throw informative error messages:
#' \dontrun{check_prob(NULL)}
#' \dontrun{check_prob()}
#' \dontrun{check_prob(TRUE)}
#' \dontrun{check_prob("0.5")}
#' \dontrun{check_prob(factor(0.5))}
#' \dontrun{check_prob(matrix(0.5))}
#' \dontrun{check_prob(list(0.5))}
#' \dontrun{check_prob(NA)}
#' \dontrun{check_prob(NaN)}
#' \dontrun{check_prob(1.1)}
#' \dontrun{check_prob(-0.5)}
#' \dontrun{check_prob(c(-0.9, 0, 0.1, 0.2, 0.3, 0.4, 0.5))}
#'
#' @export

check_prob <- function(p){

  if(is.null(p)){
    stop("Invalid argument: 'p' is NULL")
  }
  if(length(p) == 0){
    stop("Invalid argument: 'p' is empty")
  }
  if(any(!is.numeric(p), !is.vector(p),  all(is.na(p), all(is.nan(p))))){
    stop("Invalid argument: 'p' must be a numeric vector")
  }
  if(any(p[!is.na(p)] < 0, p[!is.na(p)] > 1)){
    stop("Invalid argument: all elements of 'p' must be in the [0, 1] interval.")
  }
  if(any(is.na(p), is.nan(p))){
    warning("There are NA or NaN values in 'p'")
  }
}

#' @title Check if an object is a numeric vector of valid Bayes factor values
#'
#' @description `check_bf` checks if an object is a numeric vector of valid Bayes factor values. This can be useful to validate inputs or intermediate calculations in user-defined functions or to validate the output of calculations/functions.
#'
#' @param bf An arbitrary object.
#'
#' @details `check_bf` conducts a series of tests to check if `bf` is a numeric vector of valid Bayes factor values. Namely, `check_bf` checks if:
#' * `bf` is `NULL` or empty.
#' * `bf` is a numeric (atomic) vector.
#' * `bf` has `NA` or `NaN` values.
#' *  The values of `bf` are non-negative.
#'
#' @return `check_bf` does not return any output. There are three possible scenarios:
#' * The call is silent if `bf` is a numeric vector of valid Bayes factor values and there are no `NA` or `NaN` values.
#' * An informative warning message is given if `p` is a numeric vector of valid Bayes factor values and there are `NA` or `NaN` values.
#' * An informative error message is thrown if `p` is not a numeric vector of valid Bayes factor values. This will \code{\link[base]{stop}} the execution (for example, when using `check_bf` to validate the inputs of user defined functions).
#'
#' @seealso
#' * \code{\link[pcal]{check_log_bf}} to check if an object is a numeric vector of valid logarithmic Bayes factor values.
#' * \code{\link[pcal]{check_prob}} to check if an object is a numeric vector of valid probability values.
#'
#' @examples
#' # Calls that pass silently:
#' check_bf(0)
#' check_bf(0.5)
#' check_bf(100)
#' check_bf(c(0, 0.5, 1, 10, 50, 100))
#'
#' # Call that throws an informative warning message:
#' \dontrun{check_bf(c(0.1, 0.2, NA, 0.4, 0.5))}
#'
#' # Calls that throw informative error messages:
#' \dontrun{check_bf(NULL)}
#' \dontrun{check_bf()}
#' \dontrun{check_bf(TRUE)}
#' \dontrun{check_bf("0.5")}
#' \dontrun{check_bf(factor(0.5))}
#' \dontrun{check_bf(matrix(0.5))}
#' \dontrun{check_bf(list(0.5))}
#' \dontrun{check_bf(NA)}
#' \dontrun{check_bf(NaN)}
#' \dontrun{check_bf(-0.5)}
#' \dontrun{check_bf(c(-0.9, 0, 0.1, 0.2, 0.3, 0.4, 0.5))}
#' \dontrun{check_bf(-5)}
#'
#' @export

check_bf <- function(bf){

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
}

#' @title Check if an object is a numeric vector of valid logarithmic Bayes factor values
#'
#' @description `check_log_bf` checks if an object is a numeric vector of valid logarithmic Bayes factor values. This can be useful to validate inputs or intermediate calculations in user-defined functions or to validate the output of calculations/functions.
#'
#' @param lbf An arbitrary object.
#'
#' @details `check_log_bf` conducts a series of tests to check if `lbf` is a numeric vector of valid logarithmic Bayes factor values. Namely, `check_bf` checks if:
#' * `lbf` is `NULL` or empty.
#' * `lbf` is a numeric (atomic) vector.
#' * `lbf` has `NA` or `NaN` values.
#'
#' @return `check_log_bf` does not return any output. There are three possible scenarios:
#' * The call is silent if `lbf` is a numeric vector of valid logarithmic Bayes factor values and there are no `NA` or `NaN` values.
#' * An informative warning message is given if `lbf` is a numeric vector of valid logarithmic Bayes factor values and there are `NA` or `NaN` values.
#' * An informative error message is thrown if `lbf` is not a numeric vector of valid logarithmic Bayes factor values.
#'
#' @seealso
#' * \code{\link[pcal]{check_bf}} to check if an object is a numeric vector of valid Bayes factor values.
#' * \code{\link[pcal]{check_prob}} to check if an object is a numeric vector of valid probability values.
#'
#' @examples
#' # Calls that pass silently:
#' check_log_bf(0)
#' check_log_bf(0.5)
#' check_log_bf(100)
#' check_log_bf(c(0, 0.5, 1, 10, 50, 100))
#' check_log_bf(-0.5)
#' check_log_bf(-5)
#' check_log_bf(c(-0.9, 0, 0.1, 0.2, 0.3, 0.4, 0.5))
#'
#' # Call that throws an informative warning message:
#' \dontrun{check_log_bf(c(0.1, 2, NA, 40, 0.5))}
#'
#' # Calls that throw informative error messages:
#' \dontrun{check_log_bf(NULL)}
#' \dontrun{check_log_bf()}
#' \dontrun{check_log_bf(TRUE)}
#' \dontrun{check_log_bf("0.5")}
#' \dontrun{check_log_bf(factor(0.5))}
#' \dontrun{check_log_bf(matrix(0.5))}
#' \dontrun{check_log_bf(list(0.5))}
#' \dontrun{check_log_bf(NA)}
#' \dontrun{check_log_bf(NaN)}
#'
#' @export

check_log_bf <- function(lbf){

  if(is.null(lbf)){
    stop("Invalid argument: 'lbf' is NULL.")
  }
  if(length(lbf) == 0){
    stop("Invalid argument: 'lbf' is empty")
  }
  if(all(is.na(lbf))){
    stop("Invalid argument: all elements of 'lbf' are NA or NaN.")
  }
  if(any(!is.numeric(lbf), !is.vector(lbf),  all(is.na(lbf)))){
    stop("Invalid argument: 'lbf' must be a numeric vector")
  }
  if(any(is.na(lbf))){
    warning("There are NA or NaN values in 'lbf'.")
  }
}

