
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



