
# use with lsim in this format:
# H <- tf(c(2, 5, 1),c(1, 2, 3))
# response <-  gensig('square',4,10,0.1)
# response <- lsim(H, signal$u, signal$t)
# plot(signal$t, response$y, type = "l", main = "Linear Simulation Response", col = "blue"); lines(signal$t, signal$u, type = "l", col = "grey"); grid(5,5, col = "lightgray")
#  Above Example obtained from: https://www.mathworks.com/help/control/ref/lsim.html
#' @export
#'
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
