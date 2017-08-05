#----------------------------------------------------------------------
# freqresp.R
#
# Low level frequency response function.
#
#	G <- freqresp(sys, w, iu)
#
# w <- seq(0, 4, length = 128)
# H=freqresp(as.matrix(c(1,1)), as.matrix(c(1,2,1)), as.matrix(w))
# j <- sqrt(as.complex(-1))
# A <- rbind(c(-2, -1), c(1,0)); B <- rbind(1,0);
# C <- cbind(0,1); D <- as.matrix(0);
# H <- freqresp(ss(A,B,C,D), w =  as.matrix(seq(0, 100, length=10000))*j)

#' @export
freqresp <- function(sys, w = seq(0, 100, length=10000), iu = 1) {
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
    w <- as.matrix(w)
    nw <- max(dim(w))

    # Balance A
    tmpmat <- expm::balance(sys_ss$A)
    t <- diag(tmpmat$scale)
    a <- tmpmat$z
    sys_ss$B <- solve(t, sys_ss$B)
    sys_ss$C <- sys_ss$C %*% t

    # Reduce a to Hesenburg form then directly evaluate frequency response.
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


#
# ltifr.R
#
# Usage: g <- ltifr(A,B,W)
#
# This function computes the Linear time-invariant frequency response
# kernel. Calling the routine as ltifr(A,B,W) computes the frequency
# response of the following system:
#
#    g(w) = (wI-A)\b
#
# for the complex frequencies contained in the vector W. The column
# vector B must have as many rows as the matrix A.

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
