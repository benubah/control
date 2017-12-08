#' @title Time response of a Linear Time-Invariant system
#'
#' @description
#' \code{ltitr} Computes the time response of a Linear Time-Invariant system
#'
#' @usage
#' ltitr(a, b, u, x0)
#'
#'
#' @details
#' \code{ltitr} computes the time response of a Linear Time-Invariant system in state-space representation of the form:
#'  x[n+1] = Ax[n] + Bu[n] to an input, \code{U}
#'
#' \code{ltitr(a, b, u)} computes the time response with zero-initial conditions since x0 is not supplied.
#'
#' @param a      An n x n matrix of the state-space system
#' @param b      An n x m matrix of the state-space system
#' @param u      A row vector for single input systems. The input U must have as many rows as there are inputs
#' in the system. Each column of U corresponds to a new time point. \code{u} could be generated using a signal generator
#' like \code{gensig}
#' @param x0     a vector of initial conditions with as many rows as the rows of \code{a}
#'
#' @return Returns a matrix X which has as
#' many rows as there are outputs y (and with \code{max(dim(U))} columns).
#'
#' @seealso \code{\link{lsim}} \code{\link{gensig}}
#'
#' @examples
#'
#' A <- diag(1, 2)
#' B <- rbind(1, 1)
#' x0 <- rbind(-1, -2)
#' u <- cbind(1, 2, 3, 4, 5)
#' X <- ltitr(A, B, u)
#' X <- ltitr(A, B, u, x0)
#'
#' A <- replicate(6, abs(rnorm(6)))
#' B <- replicate(3, abs(rnorm(6)))
#' U <- replicate(100, rnorm(3))
#' x0 <-  rnorm(6)
#' X <- ltitr(A, B, U)
#' X <- ltitr(A, B, U, x0)
#'
#' @export

ltitr <- function (a, b, u, x0 = NULL) {

  N <- dim(u)[2]
  X <- pracma::zeros(dim(a)[1], N)

  if (nrow(u)  !=  ncol(b)) {
    stop("LTITR:  The input U must have as many rows as there are inputs to the system.")
  }
  if (!is.null(x0)) {
    if (is.vector(x0)) {
      x0 <- as.matrix(x0)
    }
     if (ncol(x0) > 1 || length(x0) != nrow(a)) {
       stop("LTITR: X0 should be a column vector with as many rows as the rows of A")
      }
  }
  if (nargs() == 3) {
    X <- pracma::zeros(dim(a)[1], N)
  } else {
    X[,1] <- x0
  }
  for (i in 2:N) {
    X[, i] = a %*% X[, i-1] + b %*% u[, i-1]
  }
  return(X)
}

