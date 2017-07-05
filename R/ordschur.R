#----------------------------------------------------------------------
#
# ordschur
#
#	Ordered schur decomposition.
# ordschur(Ui, Si, index)
# Si is a square upper-triangular matrix
# Ui is an orthogonal matrix Ui
#	where Si and Ui are output from a SCHUR function call
# !!! WARNING: ORDSCHUR does not reorder REAL Schur forms. !!!
#	ordschur finds an orthogonal matrix U such that the eigenvalues
#	appearing on the diagonal of Si are ordered according to the
#	increasing values of the array index where the i-th element
#	of index corresponds to the eigenvalue appearing as the
#	element  Si[i,i].
#
#	Usage: ordschur(Ui, Si, idx)
#        ordschur(Si, idx)
#	Return:
#       list of (U, S)
#
#	The orthogonal matrix U is accumulated in Uo as  Uo <- Ui*U.
#     	If Ui is not given it is set to the identity matrix.
#
#
#
#----------------------------------------------------------------------

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
