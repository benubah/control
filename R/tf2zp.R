#' @title Transfer function model conversion to Zero-Pole-Gain model.
#'
#' @description
#' \code{tf2zp} converts the model for a transfer function to zero-pole-gain representation
#'
#' @usage tf2zp(num, den)
#' tf2zp(sys)
#'
#' @details
#' \code{tf2zp} converts a model object for a transfer function to a zero-pole model, Where \code{num} is the numerator and \code{den} is the denominator
#' of the transfer function and \code{sys} is a transfer function object
#'
#' @param sys   An object of transfer function class
#' @param num   A numeric vector containing the coefficients of the
# numerator in descending powers of s
#' @param den   A numeric vector containing the coefficients of the
# denominator in descending powers of s
#'
#' @return Returns a list object of 'zpk' class.
#'
#' @seealso \code{\link{tf2ss}} \code{\link{zp2tf}}
#'
#' @examples
#' syszp1 <- tf2zp(c(1,1), c(1,2,1))
#' syszp1
#' syszp2 <- tf2zp(c(2,2,1), c(1,2,1))
#' syszp2
#' unclass(syszp2) # to see list of the zeros,poles and gain as vectors
#' tf2zp(zp2tf(c(-1,-1), c(-1,-2), 5))
#' @export

tf2zp <- function (num, den) {
  if (nargs() == 1)  {
    sys_tmp <- num
    if( class(sys_tmp) == 'tf') {
      sys <- unclass(sys_tmp)
      num <- sys$num
      den <- sys$den
    } else {
      stop("TF2ZP: sys should be a transfer function model")
    }
  }
  dumsys <- tfchk(matrix(num,nrow = 1),matrix(den,nrow = 1))
  num <- dumsys$numc
  den <- dumsys$denc

  if ( length(den) ) {
    lead_coeff <- den[1]
  } else {
    lead_coeff <- 1
  }
  if (abs(lead_coeff) < .Machine$double.eps) {
    stop("The Leading coefficient of the denominator must be non-zero!")
  }
  den <- den / lead_coeff
  num <- num / lead_coeff

  if ( length(num) ) {
    while( all(num[, 1] == 0) ) {
      num <- num[, -1, drop = FALSE]
    }
  }
  num_rows <- nrow(num)
  num_cols <- ncol(num)
  p  <- pracma::roots(c(den))
  p <- as.matrix(p)

  z <- Inf * matrix(1, num_cols-1, num_rows)
  k <- matrix(0, num_rows, 1)
  for (i in 1:num_rows) {
    zz <- pracma::roots(num[i, ])
    if (length(zz)) {
      z[1:length(zz), i] <- zz
    }
    idx <- which(num[i, ] != 0)
    if (length(idx)) {
      k[i, 1] <- num[i, idx[1]]
    }
  }
  sys1 <- list(z = z, p = p, k = k)
  class(sys1) <- 'zpk'
  return(sys1)
}


