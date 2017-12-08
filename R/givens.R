#' @title Complex Givens Rotation
#'
#' @description
#' \code{givens_rot} Forms the Givens rotation matrix
#'
#'
#' @details
#'  \code{givens_rot(a, b)} returns the complex Givens rotation matrix
#'  This function is called by \code{\link{ordschur}}
#'
#' @param a Complex Square-matrix
#' @param b complex Input-matrix
#'
#' @return Returns the complex Givens rotation matrix.
#'
#' @seealso \code{\link{ordschur}}
#'
#' @export
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
