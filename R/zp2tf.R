#' @title Zero-pole-gain model conversion to Transfer function model
#'
#' @description
#' \code{zp2tf} converts the model for a zero-pole-gain system to transfer function representation
#'
#' @usage zp2tf(z, p, k)
#' zp2tf(sys)
#'
#' @details
#' \code{zp2tf} converts a model object for a zero-pole-gain system to a transfer function model
#'
#' @param sys   An object of zero-pole-gain class
#' @param z   A numeric vector containing zero locations
#' @param p   A numeric vector containing pole locations
#' @param k   A numeric vector for gain
#'
#' @return Returns a list object of 'tf' class.
#'
#' @seealso \code{\link{zp2ss}} \code{\link{tf2zp}}
#'
#' @examples
#' systf <- zp2tf(zpk(NULL, c(-1,-1), 1))
#' zp2tf(tf2zp(c(2,2,1), c(1,2,1)))
#'
#' @export
#'
zp2tf <- function (z, p, k) {

  if (nargs() == 1)  {
    sys_tmp <- z
    if( class(sys_tmp) == 'zpk') {
      sys <- unclass(sys_tmp)
      z <- sys$z
      p <- sys$p
      k <- sys$k
    } else {
      stop("ZP2TF: sys should be a Zero-Pole-Gain model")
    }
  } else if (nargs() == 3){
    sys <- zpk(z, p, k)
    z <- sys$z
    p <- sys$p
    k <- sys$k
  }

  num <- c()
  den <- Re( pracma::Poly( c(p) ) )
  den <- t(as.matrix(den))

  m <- nrow(z)
  n <- ncol(z)
  den_rows <- nrow(den)
  den_cols <- ncol(den)
  gain_rows <- nrow(k)
  gain_cols <- ncol(k)

  if (is.null(z)) {
    num <- cbind(matrix(0, gain_rows, (den_cols-1) ), k)
    sys1 <- tf(num,den)
    return(sys1)
  }
  if (gain_rows != n) {
    if (m == 1) {
      stop("z and p should be column vectors.")
    }
    stop("k must have as many elements as the columns of z.")
  }
  for (i in 1:n) {
    zj <- z[, i]
    pj <- Re( pracma::Poly(zj) * k[i] )
    pj <- t( as.matrix(pj) )
    if (den_cols - length(pj) == 0) {
      num <- pj
    } else {
      num[i, ] <- rbind( rep(0, den_cols - length(pj) ), pj)
    }
  }
  sys1 <- tf(num, den)
  return(sys1)
}
