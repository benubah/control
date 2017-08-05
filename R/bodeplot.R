# Bode program
#use freqresp from signal package in R (or elsewhere) to obtain bode plot
# the freqresp here returns two values - NOTE
# bodeplot(tf(4, c(1,1)))
# A <- rbind(c(-2, -1), c(1,0)); B <- rbind(1,0);
# C <- cbind(0,1); D <- as.matrix(0);
# bodeplot(ss(A,B,C,D))
# A1 <- rbind(c(0,1), c(-25,-4)); B1 <- rbind(c(1,1), c(0,1))
# C1 <- rbind(c(1,0), c(0,1)); D1 <- rbind(c(0,0), c(0,0))
# sys1 <- ss(A1,B1,C1,D1)
# bodeplot(sys1)
# Use par(mfrow = c(2,1)); bodeplot(selectsys(sys1,1,2)) to obtain the response for a subsystem
# of sys1 for input 1 and output 2 only
# RESET your plot layout using par(mfrow = c(1,1))

bodeplot <- function(sys,  w = seq(0, 100, length=10000), iu = 1, subtitle = "In(1) Out(1)") {
 if (issiso(sys)) {
  resp <- bode(sys, w, iu)
  w <- resp$w
  mag <- resp$mag
  phase <- resp$phase
  #magnitude plot
  pracma::semilogx(w, mag, main = "Bode Magnitude", sub = subtitle, col="blue", xlab="Frequency [rad/sec]", ylab="Magnitude [dB]", type = 'l')

  #phase plot
  pracma::semilogx(w, phase, main = "Bode Phase", sub = subtitle, col="blue", xlab="Frequency [rad/sec]", ylab="Phase [deg]", type = 'l')
 }

  if(ismimo(sys)) {
    par(mfrow = c( 2*nrow(sys[[4]]), ncol(sys[[4]]) ) )
    for (i in 1:nrow(sys[[4]])) {
      for (j in 1:ncol(sys[[4]])) {
        bodeplot(selectsys(sys,i,j), subtitle = paste("In -", i, "Out -", j))
      }
    }
  }
}
