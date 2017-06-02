#' @title State-space model conversion to Transfer function model.
#'
#' @description
#' \code{ss2tf} converts the model for a state-space system to transfer function representation
#'
#' @usage ss2tf(sys)
#' ss2tf(sys, iu) for systems with more than one input
#' ss2tf(a, b, c, d)
#' ss2tf(a, b, c, d, iu) for systems with more than one input
#'
#' @details
#' \code{ss2tf} converts a model object in state-space form to transfer function model by calculating the transfer function of the system:
#' .
#' x = Ax + Bu
#' y = Cx + Du
#'
#' @param sys   An object of state-space class
#' @param a An n x n matrix
#' @param b An n x m matrix
#' @param c An p x n matrix
#' @param d An p x m matrix
#' @param iu A numeric vector denoting number of inputs. default value is 1.
#'
#' @return Returns an object of 'tf' class containing \code{num} and \code{den}. The numerator coefficients
#' are returned in matrix num with as many rows as outputs y.
#'
#' @seealso \code{\link{tf2ss}} \code{\link{ss2zp}}
#'
#' @examples
#' sys2 <- tf2ss(tf(1, c(1,2,1)))
#' ss2tf(sys2)
#' ## OR
#' ss2tf(sys2$A,sys2$B,sys2$C,sys2$D)
#'
#' @export

ss2tf <- function(a, b, c, d, iu) {
  if (nargs() == 1 || nargs() == 2)  {
    sys_tmp <- a

    if (nargs() == 2){
      iu <- b
    }
      if( class(sys_tmp) == 'ss') {
         sys <- unclass(sys_tmp)
         a <- sys$A
         b <- sys$B
         c <- sys$C
         d <- sys$D

    } else {
      stop("SS2TF: sys should be a state-space model")
    }
  }

  d_rows <- nrow(d)
  d_cols<- ncol(d)
  errmsg <- abcdchk(a, b, c, d)
  if (errmsg != "") {
    stop(errmsg)
  }

  if ( nargs() == 4) {
    if (d_cols <= 1) {
      iu <- 1
    } else {
      stop("Specify iu for systems with more than one input.");
    }
  }
  den <- pracma::Poly(a);
  den <- t(as.matrix(den))
  if (!is.null(b)) {
    b <- b[ , iu, drop = FALSE]
  }
  if (!is.null(d)) {
    d <- d[ , iu, drop = FALSE]
  }

  if (is.null(b) && is.null(c))  {
    num <- d
    if (is.null(d) && is.null(a)) {
      den = c()
    }
    return(list(den = den, num = num))
  }

  maxdimA <- max(dim(a))
  num <- matrix(1, d_rows, maxdimA + 1)

  for (i in 1:d_rows) {
    num[i,] <- pracma::Poly(a-b %*% c[i, , drop = FALSE]) + (d[i] - 1) * den
  }
  sys1 <- list(num = num, den = den)
  class(sys1) <- 'tf'
  return(sys1)
}
