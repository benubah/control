#State-space representation to zero-pole-gain representation.

#INSPIRED FROM SCIPY - https://github.com/scipy/scipy/blob/master/scipy/signal/lti_conversion.py

#NOT FULLY TESTED YET
#         A <- rbind(c(-2, -1), c(1,0)); B <- rbind(1,0);
#          C <- cbind(0,1); D <- 0;
#          sys2 <- ss(A,B,C,D)
# ss2zp(sys2$A,sys2$B,sys2$C,sys2$D)
# ss2zp(zp2ss(tf2zp(c(1,1,1), c(1,2,1))))

ss2zp <- function (a, b, c, d, iu) {
  if (nargs() == 1 || nargs() == 2)  {
    sys_tmp <- a

    if (nargs() == 2){
      iu <- b
    }
    if( class(sys_tmp) == 'ss') {
      sys <- unclass(sys_tmp)
      a <- sys$A
      b <- sys$B
      c <- sys$C
      d <- sys$D

    } else {
      stop("SS2TF: sys should be a state-space model")
    }
  }

  if ( nargs() == 4) {
    if (ncols(d) <= 1) {
      iu <- 1
    } else {
      stop("Specify iu for systems with more than one input.");
    }
  }
  sys_tf <- ss2tf(a, b, c, d, iu)
  sys_zp <- tf2zp(sys_tf)

  return(sys_zp)
}
