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
# many rows as there are outputs y (and with max(dim(U)) columns).
#
# This routine can also be used with initial conditions as:
#
#  X=ltitr(A,B,U,X0)

#  where X0 is a column vector with as many rows as the rows of A,
# Note: The input U must have as many rows as there are inputs u
# in the system. Each column of U corresponds to a new time point.
#

# A <- diag(1,2)
# B <- rbind(1,1)
# x0 <- rbind(-1, -2)
# u <- cbind(1,2,3,4,5)
# ltitr(A,B,u,x0)

# A <- matrix(c(
#0.0948,    0.3140,   -0.0264,    0.3122,    0.0508,   -0.1067,
#0.1942,   -0.0640,    0.3212,   -0.3044,   -0.1294,    0.1517,
#0.1020,    0.0272,   -0.2001,    0.0469,   -0.0988,   -0.3590,
#0.2762,   -0.3470,    0.1809,    0.1321,   -0.0954,    0.2293,
#-0.0388,   -0.1235,    0.1149,   -0.0617,    0.2265,   -0.1043,
#-0.2912,    0.1957,    0.0466,    0.3083,   -0.0804,   -0.4048
#), nrow = 6)


#B <- matrix(c(
#  -0.9738,    0.5583,    0.7618,
#  -1.6347,    0.7752,    1.2038,
#  0,   -0.7823,    1.5441,
#  3.2967,   -0.5119,   -1.0941,
#  -0.4837,    0.3074,    1.4009,
#  0.3281,   -0.7417,   -0.7114
#), nrow=6)

#U <- replicate(100, rnorm(3))
#X <- ltitr(A,B,U)


# TESTS
# A <- diag(1,2)
# B <- rbind(1,1)
# x0 <- rbind(-1, -2)
# u <- cbind(1,2,3,4,5)
# X1 <- ltitr(A,B,u)
# X2 <- ltitr(A,B,u,x0)
#
# expected1 <- rbind(c(0, 1,  3, 6, 10), c(0, 1,  3, 6, 10))
# expected2 <- rbind(c(-1, 0,  2, 5, 9), c(-2, -1,  1, 4, 8))
# identical(X1, expected1)
# identical(X2, expected2)
#---------------------------------------------------------------------------

ltitr <- function (a, b, u, x0 = NULL) {

  N <- dim(u)[2]
  X <- pracma::zeros(dim(a)[1], N)

  if (nrow(u)  !=  ncol(b)) {
    stop("LTITR:  The input U must have as many rows as there are inputs to the system.")
  }
  if (!is.null(x0)) {
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

