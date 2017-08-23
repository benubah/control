#' @title Initial Condition Response for Linear Systems
#'
#' @aliases initialplot
#'
#' @usage initial(sys)
#' initial(sys, x0)
#' initial(sys, x0, t)
#' initialplot(sys)
#' initialplot(sys, x0)
#' initialplot(sys, x0, t)
#'
#' @description \code{initial} obtains the time response of the linear system:
#'
#'\deqn{dx/dt =  Ax + Bu}
#'
#'\deqn{y = Cx + Du}
#'
#'to an initial condition.
#'
#' @details \code{initial} produces the time response of linear systems to initial conditions using \code{lsim}
#'
#' \code{initialplot} produces the time response to initial conditions as a plot againts time.
#'
#' The functions can handle both SISO and MIMO (state-space) models.
#'
#' @param sys LTI system of transfer-function, state-space and zero-pole classes
#' @param  t   regularly spaced time vector. If not provided, it is automatically set.
#' @param x0  initial conditions as a column vector. Should have as many rows as the rows of A.
#'            where x0 is not specified, random values are assigned
#'
#'        For calls to \code{initialplot}, the same arguments are allowed
#'
#' @return A list is returned by calling \code{initial} containing:
#'
#' \code{x} Individual response of each x variable
#'
#' \code{y} Response of the system
#'
#' \code{t} Time vector
#'
#' The matrix \code{y} has as many rows as there are outputs, and columns of the same size of \code{length(t)}.
#' The matrix X has as many rows as there are states.  If the time
#' vector is not specified, then the automatically set time
#' vector is returned as \code{t}
#'
#'   A plot of \code{y} vs \code{t} is returned by calling \code{initialplot}
#'
#' @examples
#' res <- initial(tf(1, c(1,2,1)))
#' res$y
#' res$t
#' A <- rbind(c(-2, -1), c(1,0)); B <- rbind(1,0);
#' C <- cbind(0,1); D <- as.matrix(0);
#' x0 <- matrix(c( 0.51297, 0.98127))
#' initialplot(ss(A,B,C,D), x0)
#' initialplot(tf(1, c(1,2,1)), t = seq(0, 10, 0.1))
#'
#' ## State-space MIMO systems
#' A <- rbind(c(0,1), c(-25,-4)); B <- rbind(c(1,1), c(0,1));
#' C <- rbind(c(1,0), c(0,1)); D <- rbind(c(0,0), c(0,0))
#' res <- initial(ss(A,B,C,D))
#' res$y # has two rows, i.e. for two outputs
#' initialplot(ss(A,B,C,D))
#'
#' @seealso \code{\link{step}} \code{\link{impulse}} \code{\link{ramp}}
#'
#' @export

initial <- function (sys, x0 = NULL, t = NULL) {

  sys_ss <- ssdata(sys)
  errmsg <- abcdchk(sys_ss)
  if (errmsg != "") {
    report <- "INITIAL: " + errmsg
    stop(report)
  }
  if(is.null(t)) {
    t <- seq(0, 5, 0.01)
  }
  if (is.null(x0)) {
    x0 <- pracma::rand(nrow(sys_ss[[1]]), 1)
  }
  n <- length(t)
  res <- lsim(sys_ss, pracma::zeros(n, ncol(sys_ss[[2]])), t, x0)
  x <- res$x
  y <- res$y

  return(list(t = t, y = y, x = x ))

}
