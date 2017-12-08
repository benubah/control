#' @title Create Transfer function Model.
#'
#' @description
#' \code{tf} creates the model for a transfer function
#'
#' @details
#' \code{tf} creates a model object for a transfer function, Where \code{num} is the numerator and \code{den} is the denominator
#' of the transfer function.
#'
#'
#' @param num   A numeric vector or matrix (for multivariable systems)
#' @param den   A numeric vector or matrix (for multivariable systems)
#' @param Ts Sample time for discrete time systems
#'
#' @return Returns an object of 'tf' class list with a proper transfer function or with warnings when not proper.
#'
#' @seealso \code{\link{ss}} \code{\link{zpk}} \code{\link{TF}} \code{\link{tf2ss}} \code{\link{tf2zp}}
#'
#' @examples
#' tf(1, c(1,2,1))
#' sys1 <- tf(1, c(1,2,1))
#' sys1$num
#' sys1$den
#'
#' # for single-input multi-output systems (SIMO) each numerator row for one output.
#' num = rbind(c(0,1,1), c(1,0,1))
#' den = rbind(c(1,3,2))
#' tf(num, den)
#'
#' @rdname tf
#' @export

tf <- function (num, den, Ts=NULL) {
    # single variable systems
    if (is.vector(num) && is.vector(den)) {
      Dum <- tfchk(matrix(num, nrow = 1), matrix(den, nrow = 1))
      num1 <- Dum$numc
      den1 <- Dum$denc
    }

    if (is.matrix(num) && nrow(num) == 1) {
    Dum <- tfchk(matrix(num, nrow = 1), matrix(den, nrow = 1))
    num1 <- Dum$numc
    den1 <- Dum$denc
    #multiple output systems
    } else if (is.matrix(num) && nrow(num) > 1){
      num1 <- num
      den1 <- den
    }
    sys <- list(num = num1, den = den1, Ts = Ts)
    class(sys) <- "tf"
    return(sys)
}

#' @export
print.tf <- function ( x, ... ) {
# arg x is the same sys
  cat(sprintf("\n"))
  for (i in 1:nrow(x$num)){
    cat(paste("y",i,":", sep = ''))
    argnum <- c(x$num[i,])

  if (nrow(x$den) == 1 ) {
    argden <- c(x$den)
  } else if(nrow(x$den) > 1) {
    argden <- c(x$den[i,])
  }

  if ( is.null(x$Ts) || x$Ts <= 0 ) {
  numstr <- poly2str(argnum, svar = "s", smul = " ")
  denstr <- poly2str(argden, svar = "s", smul = " ")
  } else {
    numstr <- poly2str(argnum, svar = "z", smul = " ")
    denstr <- poly2str(argden, svar = "z", smul = " ")
  }
  numlen <- nchar(numstr)
  denlen <- nchar(denstr)
  len <- max(numlen, denlen)
  cat( sprintf("\n") )
  if (numlen < len) {
    center <- round( (len - numlen) / 2)
    cat(rep(" ", center), numstr, "\n")
  } else {
    cat("   ", numstr, "\n")
  }

  cat(" " , rep("-", round( (len/2) ) + 3 ))
  cat(sprintf("\n"))

  if (denlen < len) {
    center <- round((len - denlen) / 2)
    cat(rep(" ", center), denstr, "\n")
  } else {
    cat("   ", denstr, "\n\n")
  }

  }
  cat( sprintf("\n") )

  if ( is.null(x$Ts) || x$Ts <= 0) {
    cat("Transfer Function: Continuous time model", "\n\n")
  } else {
    cat("Sample Time =", x$Ts, "\n")
    cat("Transfer function: Discrete time model", "\n\n")
    }
}
