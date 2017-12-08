#' @title Nyquist Frequency Response for continuous-time Linear Systems.
#'
#' @aliases nyquistplot
#'
#' @usage
#' nyquist(sys, w, iu)
#'
#' @description \code{nyquist} computes the real and imaginary parts of the frequency response of system \code{sys}
#' at given frequencies \code{w}
#'
#' @details \code{nyquist} Compute the real and imaginary parts of the frequency response of system \code{sys}
#' at given frequencies \code{w}. When \code{sys} is a transfer function, \code{nyquist}
#' computes the frequency response of the system using the \code{signal} package.
#'
#' \code{nyquistplot} plots the frequency response computed by \code{nyquist}. For a MIMO state-space system,
#' \code{nyquistplot} uses \code{selectsys} to obtain the nyquist response for each input-to-output pair and plot
#' them individually. This means that for a 2-input, 2-output system, \code{nyquistplot} obtains the response
#' for input 1 to output 1, input 1 to output 2, input 2 to output 1 and input 2 to output 2.
#' \code{nyquistplot} uses the \code{subtitle} argument to allow a user assign the plot a sub-title
#'
#' Other possible calls using \code{nyquist} and \code{nyquistplot} are:
#'
#' \code{nyquist(sys)}
#' \code{nyquist(sys, w)}
#' \code{nyquist(sys, w = seq(0, 100, length = 10000), iu = 1)}
#' \code{nyquistplot(sys)}
#' \code{nyquistplot(sys, w)}
#' \code{nyquistplot(sys, w, subtitle)}
#'
#' @param sys LTI system of transfer-function, state-space and zero-pole classes
#' @param  w   vector of range of frequencies at the response is computed in rad/sec
#' @param iu  number to specify an input for a MIMO state-space system. If the system has
#'        3 inputs, then \code{iu} would be set to 1, set to 2 and then to 3 to obtain the nyquist
#'        response from input 1, 2, and 3 to the outputs. For single input systems, \code{iu} is always
#'        set to 1. \code{iu} is not needed/allowed for calls to \code{nyquistplot}
#'
#' @return A list is returned by calling \code{nyquist} containing:
#' \code{h.real} - real part of the frequency response
#'
#' \code{h.imag} - imaginary part of the frequency response
#'
#' A plot is returned by calling \code{nyquistplot}
#'
#' @examples
#' nyquist(tf(100, c(1,6,100)))
#' nyquist(ssdata(tf(100, c(1,6,100))))
#'
#' ## MIMO plot
#' A1 <- rbind(c(0,1), c(-25,-4)); B1 <- rbind(c(1,1), c(0,1))
#' C1 <- rbind(c(1,0), c(0,1)); D1 <- rbind(c(0,0), c(0,0))
#' sys1 <- ss(A1,B1,C1,D1)
#' nyquistplot(sys1)
#' # Use nyquistplot(selectsys(sys1,1,2)) to obtain the response for a subsystem
#' # of sys1 for input 1 and output 2 only
#'
#' @seealso \code{\link{bode}}
#'
#' @export
nyquist <- function(sys, w = seq(0, 100, length=10000), iu = 1){
  j <- sqrt(as.complex(-1))

  if (class(sys) == 'tf') {
    H <- freqresp(sys, w)
  }

  if (class(sys) != 'tf') {
    sys_ss <- ssdata(sys)
    H <- freqresp(sys_ss, as.matrix(w)*j, iu)
  }
  return(list(h.real = Re(H),  h.imag = Im(H)))
}
