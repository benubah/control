#' @title Generate periodic signal
#'
#' @description
#' \code{gensig} generates a periodic signal. More useful when used in combination with \code{lsim}
#'
#' @usage  gensig(signal, tau, tfinal, tsam)
#'
#' @details
#' \code{gensig} generates a periodic signal of the following types: \code{square, sin, cos, pulse}
#'
#' Possible usage: \code{gensig(signal)}
#'
#' @param signal      A string input containing either values of: \code{square, sin, cos, pulse} in the following format:
#'
#'                    'sq' or 'square' - Square wave
#'
#'                    'si' or 'sine'  -  Sine wave
#'
#'                    'co' or 'cos' -  Cosine wave
#'
#'                    'pu' or 'pulse' -  Periodic pulse
#'
#' @param tau         Duration of one period in seconds. Default is 5
#' @param tfinal     Duration of the signal in seconds. Default is 30
#' @param tsam       sampling time in seconds. Default is 0.01
#'
#' @return Returns a list of two single column matrices, \code{u} and \code{t}
#'
#'         \code{u} is the vector of signal values
#'
#'         \code{t} is the time vector of the signal
#'
#' @seealso \code{\link{lsim}}
#'
#' @examples
#'
#' \dontrun{ A square wave signal }
#' sig <-  gensig('square', 4, 10, 0.1)
#' plot(sig$t, sig$u, type = "l", col = "blue")
#' grid(5,5, col = "lightgray")
#'
#' \dontrun{ A sine wave signal }
#'
#' sig <-  gensig('sin')
#' plot(sig$t, sig$u, type = "l", col = "blue")
#' grid(5,5, col = "lightgray")
#'
#' @export

gensig <- function(signal, tau=5, tfinal=30, tsam=0.01){

  if (!is.character(signal)) {
    stop("gensig: first argument, signal should be a string")
  }

  stopifnot(is.numeric(tau) || is.numeric(tfinal))

  t <- pracma::Reshape ( seq(0, tfinal, tsam), length(seq(0, tfinal, tsam) ), 1)

  if(signal == "square" || signal == "sq"){
    u <- 1 * (pracma::rem(t, tau) >= tau/2)
  } else if(signal == "sin" || signal == "si"){
    u <- sin (2 * pi/tau * t);
  } else if(signal == "cos" || signal == "co"){
    u <- cos (2 * pi/tau * t);
  } else if(signal == "pulse" || signal == "pu"){
    u <- 1 * (pracma::rem (t, tau) < (1 - 1000 * .Machine$double.eps) * tsam)
  }
  return(list(u = u, t = t))
}
