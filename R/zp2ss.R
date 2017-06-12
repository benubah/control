#' @title Convert Zero-Pole-Gain Model to State-Space Model
#'
#' @description
#' \code{zp2ss} converts a system represented in zero-pole form to state-space
#'
#' @usage zp2ss(z,p,k)
#' zp2ss(sys)
#'
#' @details
#' \code{zp2ss} converts a system represented in zero-pole form to state-space by converting from zero-pole to transfer function and from transfer functon to state-space
#'
#'
#' @param zero      A vector or single row matrix
#' @param pole      A vector or single row matrix
#' @param gain      A vector
#'
#' @return Returns a list object of 'ss' class.
#'
#' @seealso \code{\link{ss2zp}} \code{\link{zp2tf}}
#'
#' @examples
#' zp2ss(NULL, c(-1,-1), 1)
#' zp2ss(tf2zp(c(1,1,1), c(1,2,1)))
#'
#' @export

zp2ss <- function(z, p, k) {

  if (nargs() == 1)  {
    sys <- z
    if( class(sys) == 'zpk') {
      sys_tf <- zp2tf(sys)
      sys_ss<- tf2ss(sys_tf)
    } else {
      stop("ZP2TF: sys should be a Zero-Pole-Gain model")
    }
  }
  if (nargs() == 3){
    sys <- zpk(z, p, k)
    sys_tf <- zp2tf(sys)
    sys_ss<- tf2ss(sys_tf)
  }
  return(sys_ss)
}
