
#' @title Check if an object is a vector of valid probability values
#'
#' @description `check_prob` checks if an object is a numeric vector of valid probability values. This can be useful to validate inputs, intermediate calculations or outputs in user-defined functions.
#'
#' @param p An arbitrary object.
#' @param allow_nas Logical value indicating either `TRUE` or `FALSE`. If `FALSE` the execution is stopped (via \code{\link[base]{stop}}) in case there are `NA` or `NaN` values in `p`.
#'
#' @details `check_prob` conducts a series of tests to check if `p` is a numeric vector of valid probability values. Namely, `check_prob` checks if:
#' * `p` is `NULL` or empty.
#' * `p` is a numeric (atomic) vector.
#' * `p` has `NA` or `NaN` values.
#' *  The values of `p` are in the \[0, 1\] interval.
#'
#' @return `check_prob` does not return any output. There are three possible scenarios:
#' * The call is silent if `p` is a numeric vector of valid probability values without `NA` or `NaN` values.
#' * An informative warning message is given if `p` is a numeric vector of valid probability values with some `NA` or `NaN` values and `allow_nas` is set to `TRUE`.
#' * An informative error message is thrown and the execution is stopped if:
#'   * `p` is not a numeric vector of valid probability values.
#'   * `p` is a numeric vector of valid probability values with some `NA` or `NaN` values and `allow_nas` is set to `FALSE`.
#'
#' @seealso
#' * \code{\link[pcal]{check_bf}} to check if an object is a numeric vector of valid Bayes factor values.
#' * \code{\link[pcal]{check_log_bf}} to check if an object is a numeric vector of valid logarithmic Bayes factor values.
#' * \code{\link[pcal]{check_log_base}} to check if an object is a numeric vector of \code{\link[base]{length}} 1 representing a valid logarithmic base.
#' * \code{\link[pcal]{check_scale}} to check if an object is a string of characters representing one of the Bayes factor interpretation scales available in the `pcal` package.
#'
#' @examples
#' # Calls that pass silently:
#' check_prob(0)
#' check_prob(0.5)
#' check_prob(1)
#' check_prob(c(0.1, 0.2, 0.3, 0.4, 0.5))
#'
#' # Calls that throw an informative warning message:
#' \dontrun{check_prob(c(0.1, 0.2, NA, 0.4, 0.5))}
#' \dontrun{check_prob(c(0.1, 0.2, NA, 0.4, 0.5), allow_nas = TRUE)}
#'
#' # Calls that throw an informative error message:
#' \dontrun{check_prob(NULL)}
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
#' \dontrun{check_prob(c(0.1, 0.2, NA, 0.4, 0.5), allow_nas = FALSE)}
#'
#' @keywords internal
#' @export

check_prob <- function(p, allow_nas = TRUE){

  output_name <- paste0("'", deparse(substitute(p)), "'")

  p_filtered <- p[!is.na(p)]

  if(is.null(p)){
    stop(paste("Invalid argument:", output_name, "is NULL."))
  }
  if(any(!is.numeric(p), !is.vector(p))){
    stop(paste("Invalid argument:", output_name, "must be a numeric vector."))
  }
  if(length(p) == 0){
    stop(paste("Invalid argument:", output_name, "is empty."))
  }
  if(all(is.na(p))){
    stop(paste("Invalid argument: all elements of", output_name,  "are NA are NaN."))
  }
  if(any(is.na(p))){
    if(isTRUE(allow_nas)){
      warning(paste("There are NA or NaN values in", paste0(output_name, ".")))
    } else {
      stop(paste("Invalid argument: There are NA or NaN values in ", paste0(output_name, ".")))
    }
  }
  if(any(p_filtered < 0, p_filtered > 1)){
    stop(paste("Invalid argument: all elements of",  output_name, "must be in the [0, 1] interval."))
  }
}

#' @title Check if an object is a numeric vector of valid Bayes factor values
#'
#' @description `check_bf` checks if an object is a numeric vector of valid Bayes factor values. This can be useful to validate inputs, intermediate calculations or outputs in user-defined functions.
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
#' * An informative warning message is given if `bf` is a numeric vector of valid Bayes factor values and there are `NA` or `NaN` values.
#' * An informative error message is thrown if `bf` is not a numeric vector of valid Bayes factor values. This will \code{\link[base]{stop}} the execution.
#'
#' @seealso
#' * \code{\link[pcal]{check_log_bf}} to check if an object is a numeric vector of valid logarithmic Bayes factor values.
#' * \code{\link[pcal]{check_prob}} to check if an object is a numeric vector of valid probability values.
#' * \code{\link[pcal]{check_log_base}} to check if an object is a numeric vector of \code{\link[base]{length}} 1 representing a valid logarithmic base.
#' * \code{\link[pcal]{check_scale}} to check if an object is a string of characters representing one of the Bayes factor interpretation scales available in the `pcal` package.
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

  output_name <- paste0("'", deparse(substitute(bf)), "'")

  if(is.null(bf)){
    stop(paste("Invalid argument:", output_name, "is NULL."))
  }
  if(any(!is.numeric(bf), !is.vector(bf))){
    stop(paste("Invalid argument:", output_name, "must be a numeric vector."))
  }
  if(length(bf) == 0){
    stop(paste("Invalid argument:", output_name, "is empty."))
  }
  if(all(is.na(bf))){
    stop(paste("Invalid argument: all elements of", output_name, "are NA or NaN."))
  }
  if(any(bf[!is.na(bf)] < 0)){
    stop(paste("Invalid argument: all elements of", output_name, "must be non-negative."))
  }
  if(any(is.na(bf))){
    warning(paste("There are NA or NaN values in", paste0(output_name, ".")))
  }
}

#' @title Check if an object is a numeric vector of valid logarithmic Bayes factor values
#'
#' @description `check_log_bf` checks if an object is a numeric vector of valid logarithmic Bayes factor values.  This can be useful to validate inputs, intermediate calculations or outputs in user-defined functions.
#'
#' @param bf An arbitrary object.
#'
#' @details `check_log_bf` conducts a series of tests to check if `bf` is a numeric vector of valid logarithmic Bayes factor values. Namely, `check_log_bf` checks if:
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
#' * \code{\link[pcal]{check_log_base}} to check if an object is a numeric vector of \code{\link[base]{length}} 1 representing a valid logarithmic base.
#' * \code{\link[pcal]{check_scale}} to check if an object is a string of characters representing one of the Bayes factor interpretation scales available in the `pcal` package.
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

  output_name <- paste0("'", deparse(substitute(bf)), "'")

  if(is.null(bf)){
    stop(paste("Invalid argument:", output_name, "is NULL."))
  }
  if(any(!is.numeric(bf), !is.vector(bf))){
    stop(paste("Invalid argument:", output_name, "must be a numeric vector."))
  }
  if(length(bf) == 0){
    stop(paste("Invalid argument:", output_name, "is empty."))
  }
  if(all(is.na(bf))){
    stop(paste("Invalid argument: all elements of ", output_name, "are NA or NaN."))
  }
  if(any(is.na(bf))){
    warning(paste("There are NA or NaN values in", paste0(output_name, ".")))
  }
}

#' @title Check if an object is a valid logarithmic base
#'
#' @description `check_log_base` checks if an object is a numeric vector of \code{\link[base]{length}} 1 that is eligible to be used as a logarithmic base. This can be useful to validate inputs in user-defined functions.
#'
#' @param bf An arbitrary object.
#'
#' @details `check_log_base` conducts a series of tests to check if `base` is a valid logarithmic base. Namely, `check_log_base` checks if:
#' * `base` is `NULL` or empty.
#' * `base` is a numeric (atomic) vector of \code{\link[base]{length}} 1.
#' * `base` is `NA` or `NaN`.
#'
#' @return `check_log_base` does not return any output. There are two possible scenarios:
#' * The call is silent if `base` is a numeric vector of \code{\link[base]{length}} 1 that is a valid logarithmic base.
#' * An informative error message is thrown otherwise.
#'
#' @seealso
#' * \code{\link[pcal]{check_prob}} to check if an object is a numeric vector of valid probability values.
#' * \code{\link[pcal]{check_bf}} to check if an object is a numeric vector of valid Bayes factor values.
#' * \code{\link[pcal]{check_log_bf}} to check if an object is a numeric vector of valid logarithmic Bayes factor values.
#' * \code{\link[pcal]{check_scale}} to check if an object is a string of characters representing one of the Bayes factor interpretation scales available in the `pcal` package.
#'
#' @examples
#' # Calls that pass silently:
#' check_log_base(10)
#' check_log_base(2)
#' check_log_base(exp(1))
#' check_log_base(0.5)
#'
#' # Calls that throw informative error messages:
#' \dontrun{check_log_base(NULL)}
#' \dontrun{check_log_base(numeric(0))}
#' \dontrun{check_log_base(-1)}
#' \dontrun{check_log_base(0)}
#' \dontrun{check_log_base(TRUE)}
#' \dontrun{check_log_base("10")}
#' \dontrun{check_log_base(factor(10))}
#' \dontrun{check_log_base(matrix(10))}
#' \dontrun{check_log_base(list(10))}
#' \dontrun{check_log_base(NA)}
#' \dontrun{check_log_base(NaN)}
#'
#' @keywords internal
#' @export

check_log_base <- function(base){

  output_name <- paste0("'", deparse(substitute(base)), "'")

  if(any(
    is.null(base),
    isFALSE(is.vector(base)),
    isFALSE(is.atomic(base)),
    isFALSE(is.numeric(base)),
    isFALSE(length(base) == 1),
    is.na(base))
    ){
    stop(paste("Invalid argument:", output_name, "must be a numeric vector of length 1."))
  }
  if(isTRUE(base <= 0)){
    stop(paste("Invalid argument:", output_name, "must be positive."))
  }
}

#' @title Check if an object is a string of characters representing an eligible Bayes factor interpretation scale
#'
#' @description `check_scale` checks if an object is a string of characters representing one of the Bayes factor interpretation scales available in the `pcal` package. This can be useful to validate inputs in user-defined functions.
#'
#' @param scale An arbitrary object.
#'
#' @details `check_scale` conducts a series of tests to check if `scale` is a string of characters representing one of the Bayes factor interpretation scales available in the `pcal` package. Namely, `check_scale` checks if:
#' * `scale` is `NULL` or empty.
#' * `scale` is a character (atomic) vector of \code{\link[base]{length}} 1 specifying either "Jeffreys" or "Kass-Raftery" (not case sensitive).
#' * `scale` is `NA` or `NaN`.
#'
#' @return `check_scale` does not return any output. There are two possible scenarios:
#' * The call is silent if `scale` is a string of characters representing one of the Bayes factor interpretation scales available in the `pcal` package.
#' * An informative error message is thrown otherwise.
#'
#' @seealso
#' * \code{\link[pcal]{check_prob}} to check if an object is a numeric vector of valid probability values.
#' * \code{\link[pcal]{check_bf}} to check if an object is a numeric vector of valid Bayes factor values.
#' * \code{\link[pcal]{check_log_bf}} to check if an object is a numeric vector of valid logarithmic Bayes factor values.
#' * \code{\link[pcal]{check_log_base}} to check if an object is a numeric vector of \code{\link[base]{length}} 1 representing a valid logarithmic base.
#'
#' @examples
#' # Calls that pass silently:
#' check_scale("Jeffreys")
#' check_scale("jeffreys")
#' check_scale("kass-raftery")
#' check_scale("Kass-Raftery")
#'
#' # Calls that throw informative error messages:
#' \dontrun{check_scale(NULL)}
#' \dontrun{check_scale(10)}
#' \dontrun{check_scale("Bayes")}
#' \dontrun{check_scale("jeff")}
#' \dontrun{check_scale("kassraftery")}
#' \dontrun{check_scale(c("jeffreys", "kass-raftery"))}
#'
#' @keywords internal
#' @export

check_scale <- function(scale){

  output_name <- paste0("'", deparse(substitute(scale)), "'")

  if(is.null(scale)){
    stop(paste("Invalid argument:", output_name, "is NULL."))
  }
  if(any(!is.vector(scale), !is.atomic(scale), isFALSE(length(scale) == 1))){
    stop(paste("Invalid argument:", output_name, "must be an atomic vector of length 1."))
  }
  if(is.na(scale)){
    stop(paste("Invalid argument:", output_name, "is NA or NaN."))
  }
  if(!is.character(scale)){
    stop(paste("Invalid argument: the type of", output_name, "must be character."))
  }
  if(isFALSE(tolower(scale) %in% c("jeffreys", "kass-raftery"))){
    stop(paste("Invalid argument:", output_name, "must be either 'jeffreys' or 'kass-raftery'."))
  }
}





