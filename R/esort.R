#' @title Sort Complex Continuous Eigenvalues in Descending
#' Order
#'
#' @description
#' \code{esort} sorts the complex continuous eigenvalues in descending
#' order
#'
#' @details
#'  \code{esort} sorts the complex eigenvalues based on their real part. The unstable eigenvalues (positive
#' real part) are first shown.
#'
#' This function is used to sort eigenvalues and system poles in \code{\link{damp}}
#'
#' @param p A vector containing the poles of a transfer-function, zero-pole model or the eigenvalues of a state-matrix
#'
#' @return Returns the sorted eigenvalues and the cooresponding
#' indices in a list:
#'
#'    s   = sorted eigenvalues
#'    idx = index
#'
#' @seealso \code{\link{damp}}
#'
#' @export
esort <- function(p) {
  t <- sort(-Re(p), index.return = TRUE)
  return(list(s = p[t$ix], idx = t$ix))
}

