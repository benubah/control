#' @title State-space model conversion to Transfer function model.
#'
#' @description
#' \code{ss2tf} converts the model for a state-space system to transfer function representation
#'
#' @usage ss2tf(a, b, c, d, iu)
#'
#' @details
#' \code{ss2tf} converts a model object in state-space form to transfer function model by calculating the transfer function of the system:
#' .
#' x = Ax + Bu
#' y = Cx + Du
#'
#' #' Other possible usages for \code{ss2tf} are:
#' \code{ss2tf(a,b,c,d)}
#' \code{ss2tf(sys)}
#' \code{ss2tf(sys, iu)}
#'
#' where \code{sys} is an object of state-space class
#'
#' @param a An n x n matrix
#' @param b An n x m matrix
#' @param c An p x n matrix
#' @param d An p x m matrix
#' @param iu A numeric value denoting number of inputs. default value is 1.For example, if the system
#' has three inputs (u1, u2, u3), then iu must be either 1, 2, or 3, where 1 implies u1, 2
#' implies u2, and 3 implies u3.
#'
#' @return Returns an object of 'tf' class containing \code{num} and \code{den}. The numerator coefficients
#' are returned in matrix \code{num} with as many rows as outputs \code{y}.
#'
#' @seealso \code{\link{tf2ss}} \code{\link{ss2zp}}
#'
#' @examples
#' sys2 <- tf2ss(tf(1, c(1,2,1)))
#' ss2tf(sys2)
#' ## OR
#' ss2tf(sys2$A,sys2$B,sys2$C,sys2$D)
#'
#' # a single input multiple output system
#' A <- rbind(c(0,1), c(-10000,-4)); B <- rbind(0,1); C <- rbind(c(1,0), c(0,1));
#' D <- rbind(0,0)
#' ss2tf(A, B, C, D)
#'
#' # a MIMO system
#' A = rbind(c(0,1), c(-25,-4)); B = rbind(c(1,1), c(0,1));
#' C = rbind(c(1,0), c(0,1)); D = rbind(c(0,0), c(0,0))
#' ss2tf(A,B,C,D,1) # to obtain output for input 1
#' ss2tf(A,B,C,D,2) # to obtain output for input 2
#'
#' ## OR
#'
#' systems <- vector("list", ncol(D))
#' for(i in 1:ncol(D)){ systems[[i]] <- ss2tf(A,B,C,D,i) }
#' systems
#' systems[[1]]
#' systems[[2]]
#'
#' @export

ss2tf <- function(a, b, c, d, iu = 1) {
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

  if (d_cols <= 1 && iu > 1){
    stop("The system does not have more than one input. iu should be 1.")
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
