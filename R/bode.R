# Bode program
#use freqresp from signal package in R (or elsewhere) to obtain bode plot
# the freqresp here returns two values - NOTE
# bodeplot(tf(4, c(1,1)))
# A <- rbind(c(-2, -1), c(1,0)); B <- rbind(1,0);
# C <- cbind(0,1); D <- as.matrix(0);
# bodeplot(ss(A,B,C,D))

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
  phase <- atan2(Imag(H), Re(H)) * 180/pi
  return(list(w = w, mag = mag, phase = phase))
}
