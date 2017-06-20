#---------------------------------------------------------------------------
#
# ltitr
#
# Syntax: X=ltitr(A,B,U,X0)
#
# This routine computes the time response of the following system:
#
#  x[n+1] = Ax[n] + Bu[n]
#
# to the input U. The result is returned in a matrix X which has as
# many columns as there are outputs y (and with length(U) rows).
#
# This routine can also be used with initial conditions as:
#
#  X=ltitr(A,B,U,X0)
#
# Note: The input U must have as many columns as there are inputs u
# in the system. Each row of U corresponds to a new time point.
#

# A <- diag(1,2)
# B <- rbind(1,1)
# x0 <- rbind(-1, -2)
# u <- cbind(1,2,3,4,5)
# ltitr(A,B,u,x0)


#---------------------------------------------------------------------------

ltitr <- function (a, b, u, x0) {

  N <- dim(u)[2]
  X <- pracma::zeros(dim(a)[1], N)

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

