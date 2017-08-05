

# nyquist(tf(100, c(1,6,100)))
# nyquist(ssdata(tf(100, c(1,6,100))))
#require(signal)
nyquist <- function(sys, w = seq(0, 100, length=10000), iu = 1){
  j <- sqrt(as.complex(-1))

  if (class(sys) == 'tf') {
    H <- freqresp(sys, w)
  }

  if (class(sys) != 'tf') {
    sys_ss <- ssdata(sys)
    H <- freqresp(sys_ss, as.matrix(w)*j, iu)
  }
  return(list(h.real = Re(H),  h.imag = Imag(H)))
}
