#' @title Ordered schur decomposition
#'
#' @usage ordschur(Ui, Si, idx)
#'
#' @description
#' \code{ordschur} Orders a schur decomposition
#'
#'
#' @details
#' \code{ordschur} finds an orthogonal matrix, \code{U} so that the eigenvalues
#'	appearing on the diagonal of \code{Si} are ordered according to the
#'	increasing values of the array index where the i-th element
#'	of index corresponds to the eigenvalue appearing as the
#'	element  \code{Si[i,i]}.
#'
#'	\code{ordschur} could also be used in this syntax:  \code{ordschur(Si, idx)}
#'
#' @param Ui   Square upper-triangular matrix matrix from schur decomposition. If Ui is not given it is set to the identity matrix.
#' @param Si   Orthogonal matrix from schur decomposition
#' @param idx  array index
#'
#' @return Returns a list of ordered (U, S)
#'
#' @export
ordschur <- function(Ui, Si,  idx) {
  n <- nrow(Si)
  if (nrow(Si) != ncol(Si)) {
    stop("ORDSCHUR: Arg 2 is not square")
    }
  if (nargs() == 2) {
     idx <- Si
    So <- Ui
  } else {
    So <- Si
  }
  if (nargs() > 2) {
    Uo <- Ui
  }else {
    Uo <- diag(1, n, n)
  }
  for (i in 1:(n-1)) {
    rot <- 0
    k <- i
    for (j in (i+1):n) {
      if ( idx[j] <  idx[k]) {
        k <- j
        rot <- 1
      }
    }
    if (rot) {
      for (ii in seq(k, (i+1), -1)) {
        lvar1 <- ii - 1
        lvar2 <- ii
        tmat <- givens_rot(So[lvar1, lvar1] - So[lvar2, lvar2], So[lvar1, lvar2])
        tmat <- rbind(tmat[2, ], tmat[1, ])
        So[ , lvar1:lvar2] <- So[ , lvar1:lvar2] %*% tmat
        So[lvar1:lvar2, ] <- t(tmat) %*% So[lvar1:lvar2, ]
        Uo[ , lvar1:lvar2] <- Uo[ , lvar1:lvar2] %*% tmat
        ix_val <-  idx[lvar1]
        idx[lvar1] <-  idx[lvar2]
        idx[lvar2] <- ix_val
      }
    }
  }
  return(list( U = Uo, S = So ))
}
