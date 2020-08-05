
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

check_prior_prob <- function(prior_prob){

  if(is.null(prior_prob)){
    stop("Invalid argument: 'prior_prob' is NULL")
  }
  if(length(prior_prob) == 0){
    stop("Invalid argument: 'prior_prob' is empty")
  }
  if(any(is.na(prior_prob))){
    stop("Invalid argument: There are NA or NaN values in 'prior_prob'")
  }
  if(any(!is.numeric(prior_prob), !is.vector(prior_prob),  all(is.na(prior_prob)))){
    stop("Invalid argument: 'prior_prob' must be a numeric vector")
  }
  if(any(prior_prob[!is.na(prior_prob)] < 0, prior_prob[!is.na(prior_prob)] > 1)){
    stop("Invalid argument: all elements of 'prior_prob' must be in the [0, 1] interval.")
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

check_log_base <- function(base){

  if(any(
    is.null(base),
    is.na(base),
    isFALSE(is.numeric(base)),
    isFALSE(is.vector(base)),
    isFALSE(length(base) == 1))
    ){
    stop("Invalid argument: 'base' must be a numeric vector of length 1")
  }
}




