#' @title Continuous Time model conversion to Discrete Time model.
#'
#' @description
#' \code{c2d} converts a system in continuous-time model to a discrete time model
#'
#' @usage c2d(sys, t)
#'
#'
#' @details
#' \code{c2d} converts the continuous-time system: x = Ax + Bu
#' to the discrete-time state-space system: x[n+1] = Phi * x[n] + Gamma * u[n] based on the method
#' of assuming a zero-order hold on the inputs and sample time
#' Transfer function and zero-pole systems are converted to state-space representation before
#' conversion to discrete-time.
#'
#' @param sys   An object of transfer function, state-space or zero-pole class
#' @param t     Sample time; a numeric value greater than 0
#'
#' @return Returns the provided system (transfer function, state-space or zero-pole) in an equivalent discrete-time.
#'
#' @seealso  \code{\link{ltitr}}
#'
#' @examples
#'
#' ## for TF
#' c2d(tf(c(1,-1), c(1,4,5)), 0.1)
#' ## for ZPK
#' sys <- zpkdata( tf(c(1,-1), c(1,4,5)) )
#' c2d(sys, 0.1)
#' c2d(zpkdata( tf(c(1,-1), c(1,4,5)) ), 0.1)
#'
#' @export
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
  a_cols <- ncol(a)
  b_cols <- ncol(b)
  mat1 <- rbind( (cbind(a, b) * t ), matrix(0, b_cols, a_cols + b_cols) )
  smat <- expm::expm(mat1)
  phi <- smat[1:a_cols, 1:a_cols, drop = FALSE]
  gamma <- smat[1:a_cols, (a_cols + 1):(a_cols + b_cols), drop = FALSE]
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
