#' @title Discrete Lyapunov Equation Solution
#'
#' @description
#' dlyap(A,C) solves the discrete Lyapunov equation:
#'
#'	A*X*A' + C = X
#'
#' @details
#' The solution X is symmetric when C is symmetric.
#'
#' @param a A square matrix
#' @param c A square matrix
#'
#' @return The functions returns a matrix
#'
#' @seealso \code{\link{lyap}}
#'
#' @examples
#' D = rbind(c(1,2), c(2,1))
#' B = rbind(c(4,3), c(4,3))
#' dlyap(B,D)
#'
#' @export

dlyap <- function(a, c) {
  m <- nrow(a)
  n <- ncol(a)
  if(ncol(c) != n || nrow(c) != m || is.complex(a) || is.complex(c)) {
    stop("dlyap: A and C must be real square matrices!!")
  }
  a <- solve((a + pracma::eye(m, m)), (a - pracma::eye(m, m)))
  c <- (pracma::eye(m, m) - a) %*% c %*% (pracma::eye(m, m) - t(a)) / 2
x <- lyap(a, c)
return(x)
}
