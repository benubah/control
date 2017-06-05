
###Zero-pole-gain representation to state-space representation
#    Parameters
#    ----------
#    z, p :    Zeros and poles.
#    k :       System gain.
#    Returns
#    -------
#    A, B, C, D : ndarray
#        State space representation of the system, in controller canonical
#        form.
###

#INSPIRED FROM SCIPY - https://github.com/scipy/scipy/blob/master/scipy/signal/lti_conversion.py

#NOT FULLY TESTED YET
# sys4$z <- NULL
# sys4$p <- as.matrix(c(-1,-1))
# sys4$k <- as.matrix(1)
#But test with zp2ss(sys4$z, sys4$p, sys4$k)
# zp2ss(tf2zp(c(1,1,1), c(1,2,1)))
zp2ss <- function(z, p, k) {

  if (nargs() == 1)  {
    sys <- z
    if( class(sys) == 'zpk') {
      #sys <- unclass(sys_tmp)
      z <- sys$z
      p <- sys$p
      k <- sys$k
    } else {
      stop("ZP2TF: sys should be a Zero-Pole-Gain model")
    }
  } else if (nargs() == 3){
    sys <- zpk(z, p, k)
    z <- sys$z
    p <- sys$p
    k <- sys$k
  }


  sys_tf <- zp2tf(z, p, k)
  sys_ss<- tf2ss(sys_tf)

  return(sys_ss)

}
