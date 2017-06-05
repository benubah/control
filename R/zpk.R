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
#' @seealso \code{\link{ss}} \code{\link{tf}}
#'
#' @examples
#' sys <- zpk(NULL, c(-1,-1), 1)
#' sys <- zpk(c(1,2), c(3,4), 5)
#' sys <- zpk(c(1,2), c(3+1i,4+2i), 5)
#' ## Access individual sys elements as
#' sys$z
#' sys$p
#' sys$k
#'
#' @export

zpk <- function(zero, pole, gain, Ts=NULL) {

  if ( is.vector(zero) && is.vector(pole) && is.vector(gain) ) {
    z <- as.matrix(zero)
    p <- as.matrix(pole)
    k <- as.matrix(gain)
  } else if ((is.null(zero) || length(zero)==0) && is.vector(pole) && is.vector(gain)) {
    z <- NULL;
    p <- as.matrix(pole)
    k <- as.matrix(gain)
  } else {
    stop("zpk: zero, pole, gain must be vectors.")
  }
  sys <- list(z = z, p = p, k = k, Ts = Ts)
  class(sys) <- "zpk"
  return(sys)
}

#' @export
print.zpk <- function(sys) {
  if(class(sys) != 'zpk'){
    stop("print.zpk: sys must be a zpk object!")
  }
  if (is.null(sys$z) || length(sys$z) == 0) {
    numstr <- " "
  } else {
    if (!is.complex(sys$z)) {
      numstr <- paste("(s", sprintf("%+g",sys$z), ")", sep="")
    } else if (is.complex(sys$z)) {
      numstr <- paste("(s", sprintf("%+g%+g%s",Re(sys$z), Im(sys$z), "j"), ")", sep="")
    }
  }

  if (!is.complex(sys$p)) {
    denstr <- paste("(s", sprintf("%+g",sys$p), ")", sep="")
  } else if (is.complex(sys$p)) {
    denstr <- paste("(s", sprintf("%+2g%+2g%s", Re(sys$p), Im(sys$p), "j"),  ")", sep="")
  }

  numstr <- paste(c(sys$k,numstr), collapse = " ")
  numlen <- nchar(numstr)
  denlen <- nchar(denstr)
  numlen <- sum(numlen)
  denlen <- sum(denlen)
  len <- max(numlen, denlen)
  cat(sprintf("\n"))
  if(numlen < len){
    center <- round((len - numlen)/2)
    cat(rep(" ", center), numstr,"\n")
  } else {
    cat("   ", numstr,"\n")
  }
  cat(" " ,rep("-",round((len/2))+3))
  cat(sprintf("\n"))
  if(denlen < len){
    center <- round((len - denlen)/2)
    cat(rep(" ", center), denstr, "\n")
  } else {
    cat("   ", denstr, "\n")
  }
  cat(sprintf("\n\n"))

  if( is.null(sys$Ts) || sys$Ts <= 0 || !exists("sys$Ts")) {
    cat(" Zero-Pole-Gain: Continuous time model", "\n")
  } else {
    cat(" Zero-Pole-Gain: Discrete time model", "\n")
  }
}


