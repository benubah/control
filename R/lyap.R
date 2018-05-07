    #' @title Continuous Lyapunov equation solution.
    #'
    #' @description
    #' \code{lyap(A, B)} solves the particular Lyapunov equation:
    #'
    #'	A*X+X*A' = -B
    #'
    #'  \code{lyap(A, B, C)} solves the Sylvester equation A*X + X*B = -C
    #'
    #' @details
    #' This function solves the lyapunov equation using an eigenvalue
    #' decomposition.
    #' The solution X is symmetric when B is symmetric.
    #'
    #' @param A An appropriately dimensioned matrix
    #' @param B An appropriately dimensioned matrix
    #' @param C An appropriately dimensioned matrix
    #'
    #' @return The functions returns a matrix
    #'
    #' @seealso \code{\link{dlyap}}
    #'
    #' @examples
    #' A = rbind(c(1,2), c(-3, -4))
    #' Q = rbind(c(3,1), c(1,1))
    #' lyap(A, Q)
    #'
    #' @note
    #' WARNING: This function assumes that there are no repeated eigenvalues.
    #'       If repeated eigenvalues exist, then the solution may be
    #'       unreliable.
    #'
    #' @export
    lyap <- function(A, B, C = NULL) {

    if (nrow(A) != ncol(A)) {
      stop("lyap: Dimensions of A and B do not agree")
      }
    if (nrow(B) != ncol(B)) {
      stop("lyap: Dimensions of A and B do not agree")
      }
    if (nrow(A) == 0) {
    X = c()
    return(X)
    }

    # Compute eigenvalues and the left and right eigenvectors of A
    E <- eigen(A)
    LamA <- diag(E$val, nrow = length(E$val))
    DA <- LamA
    PhiA <- E$vec
    PsiA <- solve(PhiA)

# Switch on Lyapunov solution or Sylvester solution.
if ( !is.null(C)) {
  if (nrow(C) != nrow(A)) {
    stop("lyap: Dimensions of C do not agree");
    }
  if (ncol(C) != nrow(B)) {
    stop("lyap: Dimensions of C do not agree");
    }

  # Computes eigenvalues and left and right eigenvectors of B
  E <- eigen(B)
  LamB <- diag(E$val, nrow = length(E$val))
  DB <- LamB
  LamB <- pracma::ones(nrow(A), nrow(B)) %*% LamB
  PhiB <- E$vec
  PsiB <- solve(PhiB)
  LamA <- LamA %*% pracma::ones(nrow(A), nrow(B))

  X <- -PhiA %*%  (solve(PhiA, C%*%PhiB/(LamA + LamB))) %*% solve(PhiB)

 } else {
  LamA <- LamA %*% pracma::ones(nrow(A), nrow(A))
  X= -PhiA %*% (PsiA%*%B%*%t(PsiA) /  (LamA + t(LamA) )) %*% t(PhiA)
 }
 X <- Re(X)
 return(X)
}

