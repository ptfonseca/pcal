
#' @title Check if an object is a vector of valid probability values
#'
#' @description `check_prob` checks if an object is a numeric vector of valid probability values. This can be useful to validate inputs and intermediate calculations in user-defined functions or to validate the output of calculations/functions.
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
#' * An informative error message is thrown if `p` is not a numeric vector of valid probability values.
#'
#' @seealso
#' * \code{\link[pcal]{check_bf}} To check if an object is a numeric vector of valid Bayes factor values.
#' * \code{\link[pcal]{check_log_bf}} To check if an object is a numeric vector of valid logarithmic Bayes factor values.
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

#' @export

check_log_bf <- function(bf){

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
}

