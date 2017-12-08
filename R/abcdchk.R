#' @title State-space matrices check.
#'
#' @description
#' \code{abcdchk} verifies the dimensions of A,B,C,D matrices in its arguments, to ascertain that they are correctly defined.
#'
#' @details
#' This is a utility function that is always invoked by other functions to
#' ascertain the dimensions of the arguments \code{a,b,c,d} and returns a message
#' if there is an ill-defined entry.
#'
#'
#' @param a An n x n matrix
#' @param b An n x m matrix
#' @param c An p x n matrix
#' @param d An p x m matrix
#'
#' @return Returns an empty string if matrix dimensions are consistent. Otherwise it returns the associated error message
#'
#' @examples
#' A <- rbind(c(0,1), c(-10000,-4))
#' B <- rbind(0,1)
#' C <- rbind(c(1,0), c(0,1))
#' D <- rbind(0,0)
#' message <-  abcdchk(A,B,C,D)
#'
#' @export

abcdchk <- function (a, b, c, d) {

  if (nargs() == 1){
    sys1 <- a
    if (class(sys1) == 'ss') {
      a <- sys1[[1]]
      b <- sys1[[2]]
      c <- sys1[[3]]
      d <- sys1[[4]]
    } else {
      stop("ABCDCHK: sys should be state-space object")
    }
  }

  errmsg <- ""
  if (nrow(a) != ncol(a)) {
    errmsg <- "The A matrix must be square"
  }

  if (nargs() > 1) {
    if (ncol(b)) {
      if (nrow(a) != nrow(b)) {
        errmsg <- "A and B must have the same number of rows."
      }
    }
  }

  if (nargs() > 2) {
    if (nrow(c)) {
      if (ncol(c) != ncol(a)) {
        errmsg <- "A and C must have the same number of columns."
      }
    }
  }

  if (nargs() > 3) {
       if ( ( nrow(a) + nrow(b) + nrow(c) ) == 0) {
      return(errmsg);
    }
    if( !is.matrix(d)){
      stop("ABCDCHK:  D must be a matrix! ")
    }
    if (ncol(d) || ncol(b)) {
      if (nrow(d) != nrow(c)) {
        errmsg <- "C and D must have the same number of rows."
      }
    }

    if (nrow(d) || nrow(c)) {
      if (ncol(d) != ncol(b)) {
        errmsg <- "B and D must have the same number of columns."
      }
    }
  }
  return(errmsg)
}
