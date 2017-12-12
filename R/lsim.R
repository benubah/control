#' @title Time response of a Linear system
#'
#' @description
#' \code{lsim} Computes the time response of a Linear system described by:
#'  \deqn{x = Ax + Bu}
#'  \deqn{ y = Cx + Du}
#'  to the input time history \code{u}.
#'
#' @usage lsim(sys, u, t, x0)
#'
#'
#' @details
#' \code{lsim(sys, u, t)} provides the time history of the linear system with zero-initial conditions.
#'
#'  \code{lsim(sys, u, t, x0)} provides the time history of the linear system with initial conditions.
#'  If the linear system is represented as a model of \code{tf} or \code{zpk}
#'  it is first converted to state-space before linear simulation is performed. This function depends on \code{c2d} and \code{ltitr}
#'
#' @param sys    An LTI system of \code{tf}, \code{ss} and \code{zpk} class
#' @param u      A row vector for single input systems. The input \code{u} must have as many rows as there are inputs
#' in the system. Each column of U corresponds to a new time point. \code{u} could be generated using a signal generator
#' like \code{gensig}
#' @param t     time vector which must be regularly spaced. e.g. \code{seq(0,4,0.1)}
#' @param x0    a vector of initial conditions with as many rows as the rows of \code{a}
#'
#' @return Returns a list of two matrices, \code{x} and \code{y}. The \code{x} values are returned from \code{ltitr}
#'         call.
#'
#' @seealso \code{\link{ltitr}}   \code{\link{lsimplot}}
#'
#' @examples
#' signal <- gensig('square',4,10,0.1)
#' H <- tf(c(2, 5, 1),c(1, 2, 3))
#' response <- lsim(H, signal$u, signal$t)
#' plot(signal$t, response$y, type = "l", main = "Linear Simulation Response", col = "blue")
#' lines(signal$t, signal$u, type = "l", col = "grey")
#' grid(5,5, col = "lightgray")
#' \dontrun{ based on example at: https://www.mathworks.com/help/ident/ref/lsim.html }
#'
#' \dontrun{ MIMO system response }
#' A <- rbind(c(0,1), c(-25,-4)); B <- rbind(c(1,1), c(0,1))
#' C <- rbind(c(1,0), c(0,1)); D <- rbind(c(0,0), c(0,0))
#' response <- lsim(ss(A,B,C,D), cbind(signal$u, signal$u), signal$t)
#' plot(signal$t, response$y[1,], type = "l",
#'  main = "Linear Simulation Response", col = "blue"); grid(7,7)
#' plot(signal$t, response$y[2,], type = "l",
#' main = "Linear Simulation Response", col = "blue"); grid(7,7)
#'
#' @export

lsim <- function (sys, u, t, x0 = NULL) {

  if (nargs() < 3) {
    stop("LSIM: Incorrect number of input arguments.")
  }

  if ( class(sys) == 'tf'){
    tfsys <- tfchk(sys$num, sys$den)
    num <- tfsys$numc
    den <- tfsys$denc

    sssys <- tf2ss(num, den)
    A <- sssys$A
    B <- sssys$B
    C <- sssys$C
    D <- sssys$D

  }
  if ( class(sys) == 'ss'){
    sssys <- sys
    A <- sssys[[1]]
    B <- sssys[[2]]
    C <- sssys[[3]]
    D <- sssys[[4]]
  }

  if ( class(sys) == 'zpk') {
    sssys <- zp2ss(sys)
    A <- sssys$A
    B <- sssys$B
    C <- sssys$C
    D <- sssys$D
  }

  errmsg <- ""
  errmsg <- abcdchk(A, B, C, D)
  if (errmsg != "") {
    estring <- paste("lsim: ", errmsg)
    stop(estring)
  }
  if ( is.null(x0) ) {
    X0 <- matrix(0, nrow(A), 1)
  } else {
    X0 <- x0
  }
  if(is.vector(u)){
    u <- as.matrix(u)
  }
  if (ncol(u) != ncol(D)) {
    stop("LSIM: Input vector U must have same number of columns as system inputs")
  }
  if (nrow(u) != length(t)) {
    stop("LSIM: Input vector U must have same number of rows as length of T")
  }
  dt <- t[2] - t[1]
  dsys <- c2d(sssys, dt)
  Ad <- dsys$A
  Bd <- dsys$B
  u <- t(u)
  x <- ltitr(Ad,Bd,u,X0)
  y <- C %*% x  + D %*% u
  return( list(t = t, y = y, x = x) )
}

