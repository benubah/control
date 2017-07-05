#----------------------------------------------------------------------
#
# givens_rot
#
# Usage: g <- givens_rot(a, b)
#      Givens rotation matrix.
#	G <- givens_rot(a, b) returns the complex Givens rotation matrix
#
#----------------------------------------------------------------------
givens_rot <- function (a, b) {
  absa <- abs(a)
  if (absa == 0){
    realVar <- 0
    cplxVar <- 1
  } else {
    normVariable <- pracma::Norm(cbind(a, b), p = 2)
    realVar <- absa / normVariable
    cplxVar <- a / absa * (Conj(b) / normVariable)
  }
  res <- rbind( cbind(realVar, cplxVar), cbind(-Conj(cplxVar), realVar) )
  return(res)
}
