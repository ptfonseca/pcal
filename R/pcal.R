
#' Calibration of P-Values for Testing Point Null Hypotheses
#'
#' \code{pcal} calibrates p-values under a robust Bayesian perspective so that they can be interpreted in either a Bayesian or a frequentist way.
#'
#' A useful way to calibrate a p-value under a robust Bayesian perspective is by using the bound that is found as the minimum posterior probability of \eqn{H_0} that is obtained by changing priors over large classes of priors under \eqn{H_{1}{H1}}.
#'
#' @param pi_null A numeric of length one value between zero and one.
#'
#' @examples
#' round(pcal(c(.1, .05, .01)), 3)
#' df <- read.csv("https://goo.gl/j6lRXD")
#' table(df$treatment, df$improvement)
#'
#' pcal(chisq.test(df$treatment, df$improvement, correct=FALSE)[["p.value"]])
#'
#' @seealso \code{\link{bcal}}
#' @importFrom Rdpack reprompt
#' @export
#'
pcal <- function(p, pi_null = 0.5){

  if(is.null(p)){
    stop("Invalid argument: 'p' NULL")
  }
  if(all(is.na(p))){
    stop("Invalid argument: all elements of 'p' are NA")
  }
  if(!is.numeric(p)){
    stop("Invalid argument: 'p' must be a numeric vector")
  }
  if(is.na(pi_null)){
    stop("Invalid argument: 'pi_null' is NA")
  }
  if(is.null(pi_null)){
    stop("Invalid argument: 'pi_null' NULL")
  }
  if(any(!is.numeric(pi_null), isFALSE(length(pi_null) == 1))){
    stop("Invalid argument: 'pi_null' must be a numeric vector of length 1")
  }

  ifelse(p == 0, 0, bfactor_to_prob(bcal(p)))

}








