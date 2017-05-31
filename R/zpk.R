#' @title Create Zero-Pole-Gain Model.
#'
#' @description
#' \code{zpk} creates the model for a system represented in zero-pole form
#'
#' @details
#' \code{zpk} creates a model object for zero-pole systems.
#'
#'
#' @param zero      A vector
#' @param pole      A vector
#' @param gain      A vector
#' @param Ts        Sample time for discrete time systems
#'
#' @return Returns a list object of 'zpk' class.
#'
#' @examples
#' sys <- zpk(NULL, c(-1,-1), 1)
#' sys <- zpk(c(1,2), c(3,4), 5)
#' ## Access individual sys elements as
#' sys$z
#' sys$p
#' sys$k
#'
#' @export


zpk <- function(zero, pole, gain, Ts=NULL){
  if((nargs() < 3) || (nargs() > 4)){
    stop("zpk: Incorrect number of inputs")
  }
  if(is.vector(zero) && is.vector(pole) && is.vector(gain)){
    z <- as.matrix(zero)
    p <- as.matrix(pole)
    k <- as.matrix(gain)
  } else if((is.null(zero) || length(zero)==0) && is.vector(pole) && is.vector(gain)){
    z <- NULL;
    p <- as.matrix(pole)
    k <- as.matrix(gain)
  } else {
    stop("zpk: zero, pole, gain must be vectors.")
  }

  if(is.null(Ts)){
    cat("\n Zero-Pole-Gain: Continuous time model", "\n")
  } else {
    cat("\n Zero-Pole-Gain: Discrete time model", "\n")
  }
  sys <- list(z = z, p = p, k = k, Ts = Ts)
  class(sys) <- "zpk"
  return(sys)
}

