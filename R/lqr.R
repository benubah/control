#----------------------------------------------------------------------
#
# lqr.R
#
# syntax: lqr2(a,b,q,r,s)
#
# Linear-quadratic regulator design for continuous-time systems.
#  lqr(A,B,Q,R)  calculates the optimal feedback gain matrix
# K such that the feedback law  u <- -Kx  minimizes the cost function
#
#      J <- Integral {x'Qx + u'Ru} dt
#	                                      .
# subject to the constraint equation:   x <- Ax + Bu
#
# Also returned is P, the steady-state solution to the associated
# algebraic Riccati equation:
#
#        0 <- PA + A'P - PBR^-1  B'P + Q
#
# lqr(A,B,Q,R,N) includes the cross-term 2x'Nu that
# relates u to x in the cost functional.
#
#Example
#A2 = rbind(c(0,1), c(0,0))
#B2 = rbind(0,1)
#R = as.matrix(1)
#Q = rbind(c(1,0), c(0,2))


lqr <- function(a,b,q,r,s){
  # Convert data for linear-quadratic regulator problem to data for
  # the algebraic Riccati equation.
  #	F <- A - B*inv(R)*S'
  #	G <- B*inv(R)*B'
  #	H <- Q - S*inv(R)*S'
  # R must be symmetric positive definite.

  errmsg <- abcdchk(a,b)
  if (errmsg != "") {
    stop("LQR:" + errmsg)
  }

  n  <- nrow(b)
  m  <- ncol(b)
  ri <- solve(r)
  rb <- ri %*% t(b)
  g  <- b %*% rb
  if (nargs() > 4) {
    rs <- ri * t(s)
    f <- a - b*rs
    h <- q - s*rs
  } else {
    f <- a
    h <- q
    rs <- pracma::zeros(m,n)
  }
  p <- care(f, g, h)
  # Find gains:
    k <- rs + rb %*% p$X

  return(list(k = k, p = p$X))
}

