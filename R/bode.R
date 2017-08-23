#' @title Bode Frequency Response for continuous-time Linear Systems.
#'
#' @aliases bodeplot
#'
#' @usage bode(sys)
#' bode(sys, w)
#' bode(sys, w, iu)
#' bode(sys, w = seq(0, 100, length = 10000), iu = 1)
#' bodeplot(sys)
#' bodeplot(sys, w)
#' bodeplot(sys, w, subtitle)
#'
#' @description \code{bode} computes the magnitude and phase of the frequency response of system \code{sys}
#' at given frequencies \code{w}
#'
#' @details \code{bode} Compute the magnitude and phase of the frequency response of system \code{sys}
#' at given frequencies \code{w}. When \code{sys} is a transfer function, \code{bode}
#' computes the frequency response of the system using the \code{signal} package.
#'
#' \code{bodeplot} plots the frequency response computed by \code{bode}. For a MIMO state-space system,
#' \code{bodeplot} uses \code{selectsys} to obtain the bode response for each input-to-output pair and plot
#' them individually. This means that for a 2-input, 2-output system, \code{bodeplot} obtains the response
#' for input 1 to output 1, input 1 to output 2, input 2 to output 1 and input 2 to output 2.
#' \code{bodeplot} uses the \code{subtitle} argument to allow a user assign the plot a sub-title
#'
#' @param sys LTI system of transfer-function, state-space and zero-pole classes
#' @param  w   vector of range of frequencies at the response is computed in rad/sec
#' @param iu  number to specify an input for a MIMO state-space system. If the system has
#'        3 inputs, then \code{iu} would be set to 1, set to 2 and then to 3 to obtain the bode
#'        response from input 1, 2, and 3 to the outputs. For single input systems, \code{iu} is always
#'        set to 1. \code{iu} is not needed/allowed for calls to \code{bodeplot}
#'
#' @return A list is returned by calling \code{bode} containing:
#' \code{w} - frequencies
#'
#' \code{mag} - magnitude of the response
#'
#' \code{phase} - phase of the response
#'
#' A plot is returned by calling \code{bodeplot}
#'
#' @examples
#' bode(tf(100, c(1,6,100)))
#' bode(ssdata(tf(100, c(1,6,100))))
#'
#' bode(tf(4, c(1,1)))
#' A <- rbind(c(-2, -1), c(1,0)); B <- rbind(1,0);
#' C <- cbind(0,1); D <- as.matrix(0);
#' bode(ss(A,B,C,D))
#'
#' ## MIMO plot
#' A1 <- rbind(c(0,1), c(-25,-4)); B1 <- rbind(c(1,1), c(0,1))
#' C1 <- rbind(c(1,0), c(0,1)); D1 <- rbind(c(0,0), c(0,0))
#' sys1 <- ss(A1,B1,C1,D1)
#' bodeplot(sys1)
#' # Use:  par(mfrow = c(2,1)); bodeplot(selectsys(sys1,1,2)) to obtain the response for a subsystem
#' # of sys1 for input 1 and output 2 only
#' # RESET your plot layout using par(mfrow = c(1,1))
#'
#' @seealso \code{\link{nyquist}}
#'
#' @export

bode <- function(sys, w = seq(0, 100, length=10000), iu = 1) {
  j <- sqrt(as.complex(-1))

  if (class(sys) == 'tf') {
    H <- freqresp(sys, w)
  }

  if (class(sys) != 'tf') {
    sys_ss <- ssdata(sys)
    H <- freqresp(sys_ss, as.matrix(w)*j, iu)
  }
  mag = 20*log10(abs(H))
  phase <- atan2(Im(H), Re(H)) * 180/pi
  return(list(w = w, mag = mag, phase = phase))
}
