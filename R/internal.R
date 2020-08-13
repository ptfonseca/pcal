
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
#' @keywords internal
#' @export

check_prob <- function(p){

  p_filtered <- p[!is.na(p)]

  if(is.null(p)){
    stop("Invalid argument: 'p' is NULL")
  }
  if(length(p) == 0){
    stop("Invalid argument: 'p' is empty")
  }
  if(any(!is.numeric(p), !is.vector(p))){
    stop("Invalid argument: 'p' must be a numeric vector")
  }
  if(all(is.na(p))){
    stop("Invalid argument: all 'p' are NA are NaN")
  }
  if(any(p_filtered < 0, p_filtered > 1)){
    stop("Invalid argument: all elements of 'p' must be in the [0, 1] interval.")
  }
  if(any(is.na(p))){
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
#' @keywords internal
#' @export

check_bf <- function(bf){

  if(is.null(bf)){
    stop("Invalid argument: 'bf' is NULL.")
  }
  if(length(bf) == 0){
    stop("Invalid argument: 'bf' is empty")
  }
  if(any(!is.numeric(bf), !is.vector(bf))){
    stop("Invalid argument: 'bf' must be a numeric vector")
  }
  if(all(is.na(bf))){
    stop("Invalid argument: all elements of 'bf' are NA or NaN.")
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
#' @param bf An arbitrary object.
#'
#' @details `check_log_bf` conducts a series of tests to check if `bf` is a numeric vector of valid logarithmic Bayes factor values. Namely, `check_bf` checks if:
#' * `bf` is `NULL` or empty.
#' * `bf` is a numeric (atomic) vector.
#' * `bf` has `NA` or `NaN` values.
#'
#' @return `check_log_bf` does not return any output. There are three possible scenarios:
#' * The call is silent if `bf` is a numeric vector of valid logarithmic Bayes factor values and there are no `NA` or `NaN` values.
#' * An informative warning message is given if `bf` is a numeric vector of valid logarithmic Bayes factor values and there are `NA` or `NaN` values.
#' * An informative error message is thrown if `bf` is not a numeric vector of valid logarithmic Bayes factor values.
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
#' @keywords internal
#' @export

check_log_bf <- function(bf){

  if(is.null(bf)){
    stop("Invalid argument: 'bf' is NULL.")
  }
  if(length(bf) == 0){
    stop("Invalid argument: 'bf' is empty")
  }
  if(any(!is.numeric(bf), !is.vector(bf))){
    stop("Invalid argument: 'bf' must be a numeric vector")
  }
  if(all(is.na(bf))){
    stop("Invalid argument: all elements of 'bf' are NA or NaN.")
  }
  if(any(is.na(bf))){
    warning("There are NA or NaN values in 'bf'.")
  }
}

check_prior_prob <- function(prior_prob){

  pp_filtered <- prior_prob[!is.na(prior_prob)]

  if(is.null(prior_prob)){
    stop("Invalid argument: 'prior_prob' is NULL")
  }
  if(length(prior_prob) == 0){
    stop("Invalid argument: 'prior_prob' is empty")
  }
  if(any(!is.numeric(prior_prob), !is.vector(prior_prob))){
    stop("Invalid argument: 'prior_prob' must be a numeric vector")
  }
  if(all(is.na(prior_prob))){
    stop("Invalid argument: all 'prior_prob' are NA are NaN")
  }
  if(any(is.na(prior_prob))){
    stop("Invalid argument: There are NA or NaN values in 'prior_prob'")
  }
  if(any(pp_filtered < 0, pp_filtered > 1)){
    stop("Invalid argument: all elements of 'prior_prob' must be in the [0, 1] interval.")
  }
}

check_log_base <- function(base){

  if(any(
    is.null(base),
    isFALSE(is.vector(base)),
    isFALSE(is.atomic(base)),
    isFALSE(is.numeric(base)),
    isFALSE(length(base) == 1),
    is.na(base))
    ){
    stop("Invalid argument: 'base' must be a numeric vector of length 1")
  }
  if(isTRUE(base <= 0)){
    stop("Invalid argument: 'base' must be positive")
  }
}

check_scale <- function(scale){

  if(is.null(scale)){
    stop("Invalid argument: 'scale' is NULL")
  }
  if(any(!is.vector(scale), !is.atomic(scale), isFALSE(length(scale) == 1))){
    stop("Invalid argument: 'scale' must be an atomic vector of length 1.")
  }
  if(is.na(scale)){
    stop("Invalid argument: 'scale' is NA or NaN")
  }
  if(!is.character(scale)){
    stop("Invalid argument: 'scale' of type character.")
  }
  if(isFALSE(tolower(scale) %in% c("jeffreys", "kass-raftery"))){
    stop("Invalid argument: 'scale' must be either  'jeffreys' or 'kass-raftery'")
  }
}





