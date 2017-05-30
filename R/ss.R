#' @title Create State-space Model.
#'
#' @description
#' \code{ss} creates the model for a system represented in state-space form
#'
#' @details
#' \code{ss} creates a model object for state-space systems.
#'
#'
#' @param a         An n x n matrix
#' @param b         An n x m matrix
#' @param c         An p x n matrix
#' @param d         An p x m matrix
#' @param Ts        Sample time for discrete time systems
#'
#' @return Returns a list object of 'ss' class.
#'
#' @examples
#' A <- rbind(c(-2, -1), c(1,0))
#' B <- rbind(1,0)
#' C <- cbind(0,1)
#' D <- 0;
#' sys <- ss(A,B,C,D)
#' ## Or
#' sys <- ss(c(-2,-1,1,0), c(1,0), c(0,1), 0)
#' ## Access individual state-space sys elements as
#' sys$A
#' sys$B
#' sys$C
#' sys$D
#'
#' @export

ss <- function(A,B,C,D,Ts=NULL){

      if((nargs() < 4) || (nargs() > 5)){
        stop("ss: Incorrect number of inputs")
      }
        if(is.vector(A)){
          A <- matrix(A, nrow = sqrt(length(A)), byrow = TRUE)
         }
            if(is.vector(B)){
              B <- matrix(B, nrow = nrow(A))
             }
               if(is.vector(C)){
                   C <- matrix(C, ncol = ncol(A))
               }
                   if(is.vector(D)){
                        D <- matrix(D, ncol = ncol(B))
                   }

  response <- abcdchk(A,B,C,D);
  if (response != "") {
    err_msg <- paste("ss: ",response);
    stop(err_msg);
  }

  if(is.null(Ts)){
    cat("\n State-Space system: Continuous time model", "\n")
  } else {
    cat("\n State-Space system: Discrete time model", "\n")
  }
  sys = list(A = A, B = B, C = C, D = D, Ts = Ts)
  class(sys) <- "ss"
  return(sys)
}

#' @export
print.ss <- function(sys){
  A <- sys$A;
  B <- sys$B;
  C <- sys$C;
  D <- sys$D;
  Ts <- sys$Ts

  colnames(A) <- paste("x", 1:ncol(A), sep='')
  rownames(A) <- paste("x", 1:nrow(A), sep='')
  colnames(B) <- paste("u", 1:ncol(B), sep='')
  rownames(B) <- paste("x", 1:nrow(B), sep='')
  colnames(C) <- paste("x", 1:ncol(C), sep='')
  rownames(C) <- paste("y", 1:nrow(C), sep='')
  colnames(D) <- paste("u", 1:ncol(D), sep='')
  rownames(D) <- paste("y", 1:nrow(D), sep='')

  cat("\n", 'sys.A =', '\n\n')

  print(A, print.gap = 3, right = TRUE)
  cat("   \n")
  cat('sys.B =', '\n\n')
  print(B, print.gap = 3, right = TRUE)
  cat("   \n")
  cat('sys.C =', '\n\n')
  print(C, print.gap = 3, right = TRUE)
  cat("   \n")
  cat('sys.D =', '\n\n')
  print(D,print.gap = 3, right = TRUE)
  cat("   \n")
}
