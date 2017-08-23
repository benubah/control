#' @title Impulse Response for Linear Systems
#'
#' @aliases impulseplot
#'
#' @usage impulse(sys)
#' impulse(sys, t)
#' impulse(sys, t, input)
#' impulseplot(sys)
#' impulseplot(sys, t)
#' impulseplot(sys, t, input)
#'
#' @description \code{impulse} obtains the time response of the linear system:
#'
#'\deqn{dx/dt =  Ax + Bu}
#'
#'\deqn{y = Cx + Du}
#'
#' @details \code{impulse} produces the impulse response of linear systems using \code{lsim}
#'
#' \code{impulseplot} produces the impulse response as a plot against time.
#'
#' These functions can handle both SISO and MIMO (state-space) models.
#'
#' @param sys LTI system of transfer-function, state-space and zero-pole classes
#' @param  t   Time vector. If not provided, it is automatically set.
#' @param input  For calls to \code{impulse}, \code{input} is a number specifying an input for a MIMO state-space system. If the system has
#'        3 inputs, then \code{input} would be set to 1, set to 2 and then to 3 to obtain the impulse
#'        response from input 1, 2, and 3 to the outputs. For single input systems, \code{input} is always
#'        set to 1.
#'
#'        For calls to \code{impulseplot}, \code{input} is a vector or range for a MIMO state-space system. For example, \code{input <- 1:3} for a system with 3-inputs
#'
#' @return A list is returned by calling \code{impulse} containing:
#'
#'  \code{t} Time vector
#'
#' \code{x} Individual response of each x variable
#'
#' \code{y} Response of the system
#'
#' The matrix \code{y} has as many rows as there are outputs, and columns of the same size of \code{length(t)}.
#' The matrix \code{x} has as many rows as there are states.  If the time
#' vector is not specified, then the automatically set time
#' vector is returned as \code{t}
#'
#'   A plot of \code{y} vs \code{t} is returned by calling \code{impulseplot}
#'
#' @examples
#' res <- impulse(tf(1, c(1,2,1)))
#' res$y
#' res$t
#' impulse(tf(1, c(1,2,1)), seq(0, 10, 0.1))
#' impulseplot(tf(1, c(1,2,1)))
#' impulseplot(tf(1, c(1,2,1)), seq(0, 10, 0.1))
#'
#' ## State-space MIMO systems
#' A <- rbind(c(0,1), c(-25,-4)); B <- rbind(c(1,1), c(0,1));
#' C <- rbind(c(1,0), c(0,1)); D <- rbind(c(0,0), c(0,0))
#' res1 <- impulse(ss(A,B,C,D), input = 1)
#' res2 <- impulse(ss(A,B,C,D), input = 2)
#' res1$y # has two rows, i.e. for two outputs
#' res2$y # has two rows, i.e. for two outputs
#' impulseplot(ss(A,B,C,D), input = 1:2) # OR
#' impulseplot(ss(A,B,C,D), input = 1:ncol(D))
#' impulseplot(ss(A,B,C,D), seq(0,3,0.01), 1:2)
#'
#' @seealso \code{\link{initial}} \code{\link{step}} \code{\link{ramp}}
#'
#' @export

impulse <- function(sys,  t = NULL, input = 1) {
  if(is.null(t)) {
    t <- seq(0,5,0.01)
  }
  sys_ss <- ssdata(sys)

  num_y <- nrow(sys_ss[[4]])
  num_u <- ncol(sys_ss[[4]])
  if (num_u*num_y == 0) {
    return(list(t=c(), x=c(), y=c()))
  }
  if (!is.null(sys_ss$B)) {
    sys_ss$B <- sys_ss$B[ , input, drop = FALSE]
  }
  sys_ss$D <- sys_ss$D[ ,input, drop = FALSE]
  x0 <- sys_ss$B
  dims <- dim(as.matrix(t))
  iu <- pracma::zeros(dims[1],dims[2])
  res <- lsim(sys_ss, iu, t, x0)
  x <- res$x
  y <- res$y
  return(list(t = t, x = x, y = y))
}
