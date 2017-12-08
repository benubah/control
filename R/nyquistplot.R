

# nyquist(tf(100, c(1,6,100)))
# nyquist(ssdata(tf(100, c(1,6,100))))
# A <- rbind(c(-2, -1), c(1,0)); B <- rbind(1,0);
# C <- cbind(0,1); D <- as.matrix(0);
# nyquistplot(ss(A,B,C,D))
# A1 <- rbind(c(0,1), c(-25,-4)); B1 <- rbind(c(1,1), c(0,1))
# C1 <- rbind(c(1,0), c(0,1)); D1 <- rbind(c(0,0), c(0,0))
# sys1 <- ss(A1,B1,C1,D1)
# nyquistplot(sys1)
# Use nyquistplot(selectsys(sys1,1,2)) to obtain the response for a subsystem
# of sys1 for input 1 and output 2 only
# RESET your plot layout using par(mfrow = c(1,1))

#' @export
nyquistplot <- function(sys, w = seq(0, 100, length=10000), subtitle = "In(1) Out(1)"){
  if (issiso(sys)) {
     H <- nyquist(sys, w)
     Real_Axis <- H$h.real
     Imaginary_Axis <- H$h.imag
     graphics::plot.default(Real_Axis, Imaginary_Axis, main="Nyquist Plot for the System", sub = subtitle, col="blue", type = "l", ylim=range( c(Imaginary_Axis, -Imaginary_Axis) ))
     graphics::lines.default(Real_Axis, -Imaginary_Axis, col="blue", type = "l", lty =2)
     graphics::grid(10,10)
  }

  if (ismimo(sys)) {
    graphics::par(mfrow = c( nrow(sys[[4]]), ncol(sys[[4]]) ) )
      for (i in 1:nrow(sys[[4]])) {
        for (j in 1:ncol(sys[[4]])) {
          nyquistplot(selectsys(sys,i,j), subtitle = paste("In -", i, "Out -", j))
        }
      }
  }
}
