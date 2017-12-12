#' @title Plot time response of an LTI system
#'
#' @description
#' \code{lsimplot} Plots the time response of a Linear system described by:
#'  \deqn{x = Ax + Bu}
#'  \deqn{ y = Cx + Du}
#'  to the input time history \code{u}.
#'
#' @usage
#' lsimplot(sys, u, t, x0)
#'
#'
#' @details
#' \code{lsimplot(sys, u, t)} plots the time history of the linear system with zero-initial conditions.
#'
#' \code{lsimplot(sys, u, t, x0)} plots the time history of the linear system with given initial conditions.
#'
#'  If the linear system is represented as a model of \code{tf} or \code{zpk}
#'  it is first converted to state-space before linear simulation is performed. This function depends on \code{c2d} and \code{ltitr}
#'
#' @param sys    An LTI system of \code{tf}, \code{ss} and \code{zpk} class
#' @param u      A row vector for single input systems. The input \code{u} must have as many rows as there are inputs
#' in the system. Each column of \code{u} corresponds to a new time point. \code{u} could be generated using a signal generator
#' such as \code{gensig}
#' @param t     time vector which must be regularly spaced. e.g. \code{seq(0,4,0.1)}
#' @param x0    a vector of initial conditions with as many rows as the rows of \code{sys$A}
#'
#'
#' @return Returns a plot for the response of the system
#'
#' @seealso \code{\link{lsim}} \code{\link{stepplot}}   \code{\link{rampplot}}
#'
#' @examples
#' signal <- gensig('square',4,10,0.1)
#' H <- tf(c(2, 5, 1),c(1, 2, 3))
#' lsimplot(H, signal$u, signal$t)
#'
#' \dontrun{ MIMO system response }
#' A <- rbind(c(0,1), c(-25,-4)); B <- rbind(c(1,1), c(0,1))
#' C <- rbind(c(1,0), c(0,1)); D <- rbind(c(0,0), c(0,0))
#' lsimplot(ss(A,B,C,D), cbind(signal$u, signal$u), signal$t)
#'
#' @export

lsimplot <- function (sys, u, t, x0 = NULL) {
  resp <- lsim(sys, u, t, x0)
  if (nrow(resp$y) > 1) {
    graphics::par(mfrow = c(nrow(resp$y), 1))
    for (i in 1:nrow(resp$y)) {
      graphics::plot(t, resp$y[i,], type = "l", xlab = "Time, sec", ylab = paste("y", i), main = "Linear Simulation Response", col = "blue");
      graphics::grid(7,7)
    }
  } else {
    graphics::plot(t, resp$y, type = "l", xlab = "Time, sec", ylab = paste("y", 1), main = "Linear Simulation Response", col = "blue")
    graphics::grid(7,7)
  }
  graphics::par(mfrow = c(1,1))
}
