
#' @title falta isto
#'
#' @description Check if an object is a numeric vector of valid probability values.
#'
#' @param p An arbitrary object.
#'
#' @details `check_prob` conducts a series of tests, namely:
#'
#' @return `check_prob` does not return any output. There are three possible scenarios:
#' * The call is silent if the `p` is a numeric vector of valid probability values without non missing values.
#' * and throws a warning in case it is a numeric vector of valid probability values with missing values
#' *It throws an informative error error otherwise.
#'
#' @seealso
#' * \code{\link[pcal]{check_bf}} FALTA ISTO.
#' * \code{\link[pcal]{check_log_bf}} FALTA ISTO.
#'
#' @examples
#' # Calls that pass silently:
#' check_prob(0)
#' check_prob(0.5)
#' check_prob(1)
#' check_prob(c(0.1, 0.2, 0.3, 0.4, 0.5))
#'
#' # Call that throws an informative warning:
#' \dontrun{check_prob(c(0.1, 0.2, NA, 0.4, 0.5))}
#'
#' # Calls that throw informative errors:
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
  if(any(!is.numeric(p), !is.vector(p),  all(is.na(p)))){
    stop("Invalid argument: 'p' must be a numeric vector")
  }
  if(any(p[!is.na(p)] < 0, p[!is.na(p)] > 1)){
    stop("Invalid argument: all elements of 'p' must be in the [0, 1] interval.")
  }
  if(any(is.na(p))){
    warning("There are NA or NaN values in 'p'")
  }
}

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

