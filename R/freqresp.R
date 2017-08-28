#' @title Low level frequency response function
#'
#' @description
#' This function obtains the low level frequency response of a system.
#'
#' @param sys An LTI system of \code{tf}, \code{ss} and \code{zpk} class
#' @param w   a vector of frequency points
#' @param iu For calls to \code{freqresp}, \code{iu} is a number specifying an input for a MIMO state-space system. If the system has
#'        3 inputs, then \code{iu} would be set to 1, set to 2 and then to 3 to obtain the step
#'        response from input 1, 2, and 3 to the outputs
#'
#' @return  \code{freqresp(sys, w)} returns a vector of frequencies for \code{sys} in complex form
#'
#' @seealso \code{\link{bode}} \code{\link{nyquist}}
#'
#' @examples
#' H <- freqresp(ssdata(tf(c(1,1), c(1,2,1))), (seq(0, 100, length = 10000)))
#' H <- freqresp(tf(c(1,1), c(1,2,1)), seq(0, 100, length = 10000))
#'
#' @export
freqresp <- function(sys, w = seq(0, 100, length = 10000), iu = 1) {
  if (class(sys) == 'tf') {
    H <- signal::freqs(c(sys$num), c(sys$den), w)
      return(H$H)
  }
 if (class(sys) != 'tf') {
   sys_ss <- ssdata(sys)
    # sys_ss is in state space mode.
    ny <- nrow(sys_ss$D)
    nu <- ncol(sys_ss$D)
    nx <- nrow(sys_ss$A)
    na <- ncol(sys_ss$A)
    if(is.complex(w)) {
      w <- as.matrix(w)
    }
    if(!is.complex(w)) {
      j <- sqrt(as.complex(-1))
      w <- w*j
      w <- as.matrix(w)
    }
    nw <- max(dim(w))

    # Balance A matrix
    tmpmat <- expm::balance(sys_ss$A)
    t <- diag(tmpmat$scale)
    a <- tmpmat$z
    sys_ss$B <- solve(t, sys_ss$B)
    sys_ss$C <- sys_ss$C %*% t

    # Reduce a to Hesenburg form
    tmp <- pracma::hessenberg(a)
    p <- tmp$P
    a <- tmp$H

    # Do similarity transformations from Hessenberg reduction to b and c
    if (nx > 0) {
      sys_ss$B <- t(p) %*% sys_ss$B[, iu]
      sys_ss$C <- sys_ss$C %*% p
      d <- sys_ss$D[ , iu, drop = FALSE]
      g <- ltifr(a, sys_ss$B, w)
      g <- t(sys_ss$C %*% g + diag(d) * pracma::ones(ny, nw))
    } else {
      d <- sys_ss$D[ , iu, drop = FALSE]
      g <- t(diag(d) * pracma::ones(ny, nw))
    }
  }
  return(g)
}

#' @title LTI frequency response kernel
#'
#' @description
#' This function computes the frequency
#' response of the following system:
#'
#'    g(w) = (wI-A) \ B
#'
#' for the complex frequencies contained in the vector W. The column
#' vector B must have as many rows as the matrix A.
#' @param A State-space matrix, A
#' @param B State-space matrix, B. B must have as many rows as the matrix A.
#'
#' @return Returns the frequency response in vector. \code{\link{freqresp}} utilizes this function for state-space systems.
#'
#'@seealso \code{\link{freqresp}}
#'
#' @examples
#' ## use \code{\link{freqresp}}
#'
#' @export
ltifr <- function(A, B, w){
  num_w <- max(dim(w))
  num_a <- max(dim(A))
  dA <- diag(num_a)
  fr <- sqrt(as.complex(-1)) * pracma::ones(num_a, num_w)
  for (i in 1:num_w) {
    tmp_mat <- (w[i] * dA - A)
    fr[ , i] <- solve(tmp_mat, B)
  }
  return(fr)
}
