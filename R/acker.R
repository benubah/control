#' @title Pole placement gain selection using Ackermann's formula
#'
#'
#' @description  Computes the Pole placement gain selection using Ackermann's formula.
#'
#' @details  K <- ACKER(A,B,P)  calculates the feedback gain matrix K such that
#'	the single input system
#'	        .
#'	        x <- Ax + Bu
#'
#'	with a feedback law of  u <- -Kx  has closed loop poles at the
#'	values specified in vector P, i.e.,  P <- eigen(A - B * K).
#'
#'  This method is NOT numerically stable and a warning message is printed if the nonzero closed loop
#'	poles are greater than 10% from the desired locations specified
#'	in P.
#'
#'	@param a State-matrix of a state-space system
#'	@param b Input-matrix of a state-space system
#'	@param p closed loop poles
#'
#' @examples
#' F <- rbind(c(0,1),c(0,0))
#' G <- rbind(0,1)
#' H <- cbind(1,0);
#' J <- 0
#' t <- 1
#' sys  <-  ss(F,G, H,J)
#' A  <-  c2d(sys,t);
#' j <- sqrt(as.complex(-1));
#' pc  <- rbind(0.78+0.18*j, 0.78-0.18*j)
#' K  <-  acker(A$A, A$B, pc)
#' @export
#'
acker <- function (a, b, p) {

  errmsg <- abcdchk(a, b)
  if (errmsg != "") {
    stop(errmsg)
    }
  b_cols <- ncol(b)
  if (b_cols != 1) {
    stop("System must be a single input system")
  }
  if (is.vector(p)) {
    p <- as.matrix(p) # Make sure roots are in a column vector
  }
  p_rows <- nrow(p)
  a_rows <- nrow(a)
  if (a_rows != p_rows) {
    stop("Vector p must have elements as the size of a")
  }
  Cmat <- ctrb(a, b)
  Amat <-  polyvalm(Re(pracma::Poly(c(p))), a)
  k <- solve(Cmat, Amat)
  k <- k[b_cols, ]
  p <- sort(p)
  i <- which(p != 0)
  p <- p[i]
  pc <- sort(eigen(a - b %*% k)$values)
  pc <- pc[i]
  if (max(abs(p - pc) / abs(p)) > .1) {
    warning("Pole locations are more than 10% in error.")
  }
  return(as.matrix(k))
}

