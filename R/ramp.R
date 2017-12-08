#' @title Ramp Response for Linear Time-Invariant Systems
#'
#' @aliases rampplot
#'
#' @usage
#' ramp(sys, t, input)
#' rampplot(sys, t, input)
#'
#' @description \code{ramp} obtains the ramp response of the linear system:
#'
#'\deqn{dx/dt =  Ax + Bu}
#'
#'\deqn{y = Cx + Du}
#'
#'
#' @details \code{ramp} produces the ramp response of linear systems using \code{lsim}
#'
#' \code{rampplot} produces the ramp response as a plot against time.
#'
#' These functions can handle both SISO and MIMO (state-space) models.
#'
#' #' Other possible calls using \code{ramp} and \code{rampplot} are:
#'
#' \code{ramp(sys)}
#'
#' \code{ranp(sys, t)}
#'
#' \code{rampplot(sys)}
#'
#' \code{rampplot(sys, t)}
#'
#' @param sys LTI system of transfer-function, state-space and zero-pole classes
#' @param  t   Time vector. If not provided, it is automatically set.
#' @param input  For calls to \code{ramp}, \code{input} is a number specifying an input for a MIMO state-space system. If the system has
#'        3 inputs, then \code{input} would be set to 1, set to 2 and then to 3 to obtain the ramp
#'        response from input 1, 2, and 3 to the outputs. For single input systems, \code{input} is always
#'        set to 1.
#'
#'        For calls to \code{rampplot}, \code{input} is a vector or range for a MIMO state-space system. For example, \code{input <- 1:3} for a system with 3-inputs
#'
#' @return A list is returned by calling \code{ramp} containing:
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
#'   A plot of \code{y} vs \code{t} is returned by calling \code{rampplot}
#'
#' @examples
#' res <- ramp(tf(1, c(1,2,1)))
#' res$y
#' res$t
#' ramp(tf(1, c(1,2,1)), seq(0, 6, 0.1))
#' rampplot(tf(1, c(1,2,1)))
#' rampplot(tf(1, c(1,2,1)), seq(0, 6, 0.1))
#'
#' ## State-space MIMO systems
#' A <- rbind(c(0,1), c(-25,-4)); B <- rbind(c(1,1), c(0,1));
#' C <- rbind(c(1,0), c(0,1)); D <- rbind(c(0,0), c(0,0))
#' res1 <- ramp(ss(A,B,C,D), input = 1)
#' res2 <- ramp(ss(A,B,C,D), input = 2)
#' res1$y # has two rows, i.e. for two outputs
#' res2$y # has two rows, i.e. for two outputs
#' rampplot(ss(A,B,C,D), input = 1:2) # OR
#' rampplot(ss(A,B,C,D), input = 1:ncol(D))
#' rampplot(ss(A,B,C,D), seq(0,3,0.01), 1:2)
#'
#' @seealso \code{\link{initial}} \code{\link{step}} \code{\link{impulse}}
#'
#' @export

ramp <- function (sys, t = NULL, input = 1) {
  sys_ss <- ssdata(sys)
  errmsg <- abcdchk(sys_ss)
  if (errmsg != "") {
    report <- "RAMP: " + errmsg
    stop(report)
  }
  if(is.null(t)) {
    t <- seq(0, 5, 0.01)
  }
    # only iu-th input related items needed
    if (!is.null(sys_ss$B)) {
      sys_ss$B <- sys_ss$B[ , input, drop = FALSE]
    }
    sys_ss$D <- sys_ss$D[ , input, drop = FALSE]
  r <- as.matrix(t)
  res <- lsim(sys_ss, r, t)
  x <- res$x
  y <- res$y
  return(list(t = t, y = y, x = x ))
}
