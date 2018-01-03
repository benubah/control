# Bode Plot

#' @export
bodeplot <- function(sys,  w = seq(0, 100, length=10000), iu = 1, subtitle = "In(1) Out(1)") {
 if (class(sys) != 'ss' || issiso(sys)) {
  resp <- bode(sys, w, iu)
  w <- resp$w
  mag <- resp$mag
  phase <- resp$phase
  graphics::par(mfrow = c(2,1))
  #magnitude plot
  pracma::semilogx(w, mag, main = "Bode Magnitude", sub = subtitle, col="blue", xlab="Frequency [rad/sec]", ylab="Magnitude [dB]", type = 'l')

  #phase plot
  pracma::semilogx(w, phase, main = "Bode Phase", sub = subtitle, col="blue", xlab="Frequency [rad/sec]", ylab="Phase [deg]", type = 'l')
 }

  if(class(sys) == 'ss' && ismimo(sys)) {
    graphics::par(mfrow = c( 2*nrow(sys[[4]]), ncol(sys[[4]]) ) )
    for (i in 1:nrow(sys[[4]])) {
      for (j in 1:ncol(sys[[4]])) {
        bodeplot(selectsys(sys,i,j), subtitle = paste("In -", i, "Out -", j))
      }
    }
  }
}
