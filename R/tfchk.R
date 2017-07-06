#' @title Transfer function check.
#'
#' @description
#' \code{tfchk} verifies the structure of a transfer function
#'
#' @details
#' This is a utility function that is always invoked by other functions to
#' verify the structure of \code{num, den}. Where \code{num} is the numerator and \code{den} is the denominator
#' of the transfer function. If the transfer function is not proper, it returns a list with length(num) = length(den).
#'
#'
#' @param num A numeric vector
#' @param den A numeric vector
#'
#' @return Returns a list with a proper transfer function or with warnings when not proper.
#'
#' @examples
#' tf1 <- tfchk(1, c(1,2,1))
#'
#' @export

tfchk <- function (num, den) {

  if ( is.vector(num) ) {
    num <- matrix(num, nrow = 1)
  }
  if ( is.vector(den) ) {
    den <- matrix(den, nrow = 1)
  }
  if ( is.null(num) ) {
    print("TFCHK: Warning: Transfer function numerator should not be empty.\n")
  }
  if ( is.null(den) ) {
    print("TFCHK: Warning: Transfer function denominator should not be empty.\n")
  }
  if ( !( (nrow(den) == 1) || (ncol(den) == 1)) ) {
    stop("TFCHK: Denominator must be a row vector.")
  }
  if ( (nrow(den) != 1) && (ncol(den) == 1)) {
    stop("TFCHK: Denominator must be a row vector.")
  }
  if (ncol(num) > ncol(den)) {
    print("TFCHK: Transfer function may not be proper and may lead to errors. Num > Den")
  }
  if ( ncol(num) <= ncol(den) ) {
    numc <- cbind( matrix(0, nrow(num), ncol(den) - ncol(num) ), num)
  } else {
    numc <- num
  }
  denc <- den;
  return (list(numc = numc, denc = denc))
}

