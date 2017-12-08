#' @title DC Gain
#'
#' @description
#' \code{dcgain} Forms the Givens rotation matrix
#'
#'
#' @details
#'  \code{dcgain(sys)}  Computes the steady-state gain (or low frequency gain) of a continuous
#'  system.
#'
#' @param sys A transfer function or state-space model
#'
#' @return Returns the gain.
#'
#' @export
dcgain <- function(sys) {
  if(class(sys) == 'tf'){
    systf <- tfchk(sys$num, sys$den)
    if ( (length(systf$den) == 0) || (length(systf$num) == 0) ) {
      gain <- c()
    } else {
      gain <- systf$num[, length(systf$den)] / systf$den[length(systf$den)]
    }
  } else if (class(sys) == 'ss') {
    errmsg <- ""
    errmsg <- abcdchk(sys)
    if (errmsg != "") {
      stop("DCGAIN: "+ errmsg)
    }
    gain <- sys$D - (sys$C %*% solve(sys$A)) %*% sys$B
  } else {
    stop("DCGAIN: System must be transfer-function or State-space model");
  }
  return(gain)
}

