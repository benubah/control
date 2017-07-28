#' @title Form Controllability Matrix
#'
#' @description
#' \code{ctrb} forms the controllability matrix.
#'
#'
#' @details
#' \code{ctrb}  \code{ctrb(a, b)} returns the controllability matrix,  [B AB A^2B ... A^(n-1)B]. If the Controllability
#' matrix has full row rank, the system is controllable.
#'
#' @param A   State matrix, A
#' @param B   State matrix, B
#'
#' @return Returns the controllability matrix.
#'
#' @seealso \code{\link{obsv}}
#'
#' @examples
#' a1 <- rbind(c(0,0),c(1,-3))
#' b1 <- rbind(-2,0)
#' ctrb(a1, b1)
#'
#' @export

ctrb <- function(A, B) {
  cmat <- B
  D <- diag(1, nrow(A), ncol(A))
  for (i in 1:(ncol(A) - 1)) {
    D <- D %*% A
    cmat <- cbind(cmat, D %*% B)
  }
  return(cmat)
}

