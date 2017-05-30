#' @title Create Transfer function Model.
#'
#' @description
#' \code{tf} creates the model for a transfer function
#'
#' @details
#' \code{tf} creates a model object for transfer functions, Where \code{num} is the numerator and \code{den} is the denominator
#' of the transfer function. If the transfer function is not proper, it returns a list with length(num) = length(den).
#'
#'
#' @param num   A numeric vector
#' @param den   A numeric vector
#'
#' @return Returns an object of 'tf' class list with a proper transfer function or with warnings when not proper.
#'
#' @examples
#' tf(1, c(1,2,1))
#' sys1 <- tf(1, c(1,2,1))
#' sys1$num
#' sys1$den
#'
#' @export

tf <- function(num, den, Ts=NULL){

  if((nargs() < 2) || (nargs() > 3)){
    stop("tf: Incorrect number of inputs")
  }
    Dum <- tfchk(matrix(num,nrow = 1),matrix(den,nrow = 1));
    num1 <- Dum$numc;
    den1 <- Dum$denc;

      if(is.null(Ts)){
           cat("\nTransfer Function: Continuous time model", "\n\n")
      } else {
           cat("\nTransfer function: Discrete time model", "\n\n")
      }

    sys = list(num=num1, den=den1, Ts = Ts)
    class(sys) <- "tf"
    return(sys)
}


print.tf <- function(sys){
  argnum <- c(sys$num)
  argden <- c(sys$den)
  numstr <- poly2str(argnum, svar = "s", smul = " ")
  denstr <- poly2str(argden, svar = "s", smul = " ")
  numlen <- nchar(numstr)
  denlen <- nchar(denstr)
  len <- max(numlen, denlen)
  if(numlen < len){
    center <- round((len - numlen)/2)
    cat(rep(" ", center), numstr,"\n")
  } else {
    cat("   ", numstr,"\n")
  }
  cat(" " ,rep("-",round((len/2))+3))
  cat(sprintf("\n"))
  if(denlen < len){
    center <- round((len - denlen)/2)
    cat(rep(" ", center), denstr, "\n")
  } else {
    cat("   ", denstr, "\n")
  }
  cat(sprintf("\n\n"))
}
