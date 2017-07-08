
#
# care
#
# X <- care(A,B,Q)
#
# Continuous-time Algebraic Riccati Equation solution.
# X <- care(A, B, Q, R) returns the stablizing solution (if it
# exists) to the continuous-time Riccati equation:
#
#          A'*X + X*A − X*B*R^−1*B'*X + Q'*Q = 0
#
# G = R^-1 B'X*E
# L = eig(a-b*g)

#Example
#a = matrix(c(-3, 2,1, 1), byrow = TRUE, ncol = 2)
#b = matrix(c(0, 1), nrow = 2)
#c = matrix(c(1, -1), ncol=2)
#q=t(c)%*%c
#r=3
#care(a,b,q,r)
#' @export
care <- function(A, B, Q, R = 1) {
  eps <- .Machine$double.eps
  nr <- nrow(A)
  nc <- ncol(A)
  n  <- nr;
  if (nr != nc) {
    stop("A should be a square matrix")
  }
  nr <- nrow(B)
  nc <- ncol(B)
  if (nr!=n) {
    stop("CARE: A and B must have equal rows")
  }
  nr <- nrow(Q)
  nc <- ncol(Q)

  if (nr!=n || nc!=n) {
    stop("CARE: Q must have same dimensions as A")
  }
  if(!is.matrix(R)) {
  R <- R * diag(1,nrow(A),ncol(A))
  }
  G <- solve(R) %*% B %*% t(B)

  var1 <- rbind(cbind(A, -G), cbind(-Q, t(-A)))
  val <- var1 * (1.0 + (eps * eps) * sqrt(as.complex(-1)))
  tmp <- Matrix::Schur(val) # coerces imaginary parts
  #tmp <- qz.zgees(val) # schur decomposition from QZ package retaining imaginary parts
  q <- as.matrix(tmp$Q)
  t <- as.matrix(tmp$T)
  tol <- 10.0 * eps * max(abs(diag(t)))
  ns <- 0
  idx <- c()
  for (i in 1:(2 * n)) {
    if ( Re(t[i, i]) < -tol ) {
      idx <- cbind(idx, -1)
      ns <- ns + 1
    } else if (Re(t[i, i]) > tol) {
      idx <- cbind(idx, 1)
    } else {
      idx <- cbind(idx, 0)
    }
  }
  if (ns != n) {
    stop("CARE: A, B may be uncontrollable or no solution exists");
  }
  res <- ordschur(q, t, idx)
  U <- res$U
  X <- Re(U[(n+1):(n+n), 1:n]) %*% solve(Re(U[1:n, 1:n]))
  E <- diag(1, nrow(A))
  gain <- t(B) %*% solve(R) %*% X %*% E
  L <- as.matrix(pracma::eig(( A - B %*% gain )))

  return(list(X = X, L = L, G = gain))
}
