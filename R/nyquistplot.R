#' @export
nyquistplot <- function(sys, w = seq(0, 100, length=10000), subtitle = "In(1) Out(1)"){
  if (class(sys) == 'tf' || class(sys) == 'zpk' || issiso(sys)) {
     H <- nyquist(sys, w)
     Real_Axis <- H$h.real
     Imaginary_Axis <- H$h.imag
     graphics::plot.default(Real_Axis, Imaginary_Axis, main="Nyquist Plot for the System", sub = subtitle, col="blue", type = "l", ylim=range( c(Imaginary_Axis, -Imaginary_Axis) ))
     graphics::lines.default(Real_Axis, -Imaginary_Axis, col="blue", type = "l", lty =2)
     graphics::grid(10,10)
  }

  if(class(sys) == 'ss' && ismimo(sys)) {
    graphics::par(mfrow = c( nrow(sys[[4]]), ncol(sys[[4]]) ) )
      for (i in 1:nrow(sys[[4]])) {
        for (j in 1:ncol(sys[[4]])) {
          nyquistplot(selectsys(sys,i,j), subtitle = paste("In -", i, "Out -", j))
        }
      }
  }
}
