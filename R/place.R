#' @title Pole placement gain selection
#'
#'
#' @description  Computes the Pole placement gain selection using Ackermann's formula.
#'
#' @details  K <- place(A,B,P)  calculates the feedback gain matrix K such that
#'	the single input system
#'	        .
#'	        x <- Ax + Bu
#'
#'	with a feedback law of  u <- -Kx  has closed loop poles at the
#'	values specified in vector P, i.e.,  P <- eigen(A - B * K). This function
#'	is just a wrapper for the \code{acker} function.
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
#' K  <-  place(A$A, A$B, pc)
#' @export
#'
place <- function (a, b, p) {

  return(acker(a, b, p))

}

