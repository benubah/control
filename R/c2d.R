# EXAMPLES
#
# # for TF
# c2d(tf(c(1,-1), c(1,4,5)), 0.1)
# # for ZPK
# sys <- zpkdata( tf(c(1,-1), c(1,4,5)) )
# c2d(sys, 0.1)
# c2d(zpkdata( tf(c(1,-1), c(1,4,5)) ), 0.1)

c2d <- function (sys, t) {

  if ( class(sys) == 'tf') {
    sssys <- tf2ss(sys)
    a <- sssys$A
    b <- sssys$B
    c <- sssys$C
    d <- sssys$D

  }
  if ( class(sys) == 'ss' ) {
    a <- sys[[1]]
    b <- sys[[2]]
    c <- sys[[3]]
    d <- sys[[4]]
  }
  if ( class(sys) == 'zpk') {
    zpsys <- zp2ss(sys)
    a <- zpsys$A
    b <- zpsys$B
    c <- zpsys$C
    d <- zpsys$D

  }

  errmsg <- abcdchk(a, b)
  if (errmsg != "") {
    print(errmsg)
  }
  n <- ncol(a)
  nb <- ncol(b)
  m <- rbind( (cbind(a, b) * t ), matrix(0, nb, n + nb) )
  s <- expm::expm(m)
  phi <- s[1:n, 1:n, drop = FALSE]
  gamma <- s[1:n, (n + 1):(n + nb), drop = FALSE]

  ad <- phi
  bd <- gamma

  if ( class(sys) == 'tf') {
    tfsys <- ss2tf(ad, bd , c, d)
    return( tf(tfsys$num, tfsys$den, t) )
  }
  if ( class(sys) == 'ss') {
     return( ss(ad, bd, c, d, t) )
  }
  if ( class(sys) == 'zpk') {
    zpsys <- ss2zp(ad, bd , c, d)
    return( zpk(zpsys$z, zpsys$p, zpsys$k, t) )
  }
}
