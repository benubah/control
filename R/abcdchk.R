#' @title State-space matrices check.
#'
#' @description
#' \code{abcdchk} verifies the dimensions of A,B,C,D matrices in its arguments, to be ascertain they correctly defined.
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

abcdchk = function(a,b,c,d){

  msg="";
  # print(a); print(ncol(a))
  if (nrow(a) != ncol(a)) {
    msg="The A matrix must be square";
  }

  if (nargs() > 1) {
    if (ncol(b)) {
      if (nrow(a) != nrow(b)) {
        msg="The A and B matrices must have the same number of rows.";
      }
    }
  }

  if (nargs() > 2) {
    if (nrow(c)) {
      if (ncol(c) != ncol(a)) {
        msg="The A and C matrices must have the same number of columns.";
      }
    }
  }

  if (nargs() > 3) {
    # Check if a,b,c matrices have zero dimensions. If so, just return.
    if ((nrow(a)+nrow(b)+nrow(c)) == 0) {
      return(msg);
    }
    # Check C and D matrix compatibilities
    if (ncol(d) || ncol(b)) {
      if (nrow(d) != nrow(c)) {
        msg="The C and D matrices must have the same number of rows.";
      }
    }
    # Check B and D matrix compatibilities
    if (nrow(d) || nrow(c)) {
      if (ncol(d) != ncol(b)) {
        msg="The B and D matrices must have the same number of columns.";
      }
    }
  }

  return(msg)


}
