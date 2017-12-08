#' @title Damping and Natural Frequencies for Continuous Systems
#'
#' @description
#' \code{damp} computes the natural frequency and damping for
#' continuous systems.
#'
#' @details
#' A table of the eigenvalues of
#' the matrix a, the associated damping factors, the associated natural
#' frequency (rad/s and Hz.) is displayed by calling the function.
#'
#' When the continuous system is a state-space model, the eigenvalues of the state matrix
#' are obtained and sorted.
#' If the system is a transfer-function, the poles of the systems are obtained and sorted.
#' For zero-pole systems, the poles are just extracted and sorted.
#' The sorted eigenvalues are printed to output and used to obtain the natural frequencies
#' and damping factors.
#'
#' @param sys A Continuous-time system of state-space, transfer-function or zero-pole-gain model.
#' @param doPrint If TRUE prints out the results. Default is TRUE.
#'
#' @return Returns the natrural frequencies and damping
#' factors in a list:
#'
#'  omegan = Natural Frequencies (rad/s)
#'  zeta   = Damping Factors
#'
#' @seealso \code{\link{esort}}
#'
#' @examples
#' sys1 <- tf(1, c(1,2,5))
#' damp(sys1)
#'
#' @export
damp <- function (sys, doPrint = TRUE) {
  # Compute the eigenvalues of the matrix and sort them
  if (class(sys) == 'ss') {
    eigvlsort <- esort(eigen(sys[[1]])$values)$s
  } else  if (class(sys) == 'tf') {
    eigvlsort <- esort(pole(sys))$s
  } else  if (class(sys) == 'zpk') {
    eigvlsort <- esort(sys$p)$s
  }  else{
      stop("DAMP: sys must be of tf, ss or zpk class")
  }
  omegan <- abs(eigvlsort)
  omegan <- as.matrix(omegan)
  zeta <- -cos(atan2(Im(eigvlsort),Re(eigvlsort)))
  omegahz <- sqrt(eigvlsort)/(2.0*pi)
 if (doPrint) {
  cat(sprintf("\n"))
  cat(sprintf("     Eigenvalue            Damping     Freq. (rad/s)    Freq. (Hz). \n"))
  cat(sprintf("     ----------            -------     -------------    -----------\n"))
  for (i in 1:nrow(omegan)) {
    if (Im(eigvlsort[i]) < 0.0) {
      cat(sprintf("%-10.2f -%10.2fj", Re (eigvlsort[i]), abs(Im(eigvlsort[i]))))
    }  else {
      cat(sprintf("%-10.2f +%10.2fj", Re (eigvlsort[i]), abs(Im(eigvlsort[i]))))
    }
    cat(sprintf("   "))
    cat(sprintf("%-10.2f", zeta[i]))
    cat(sprintf("     "))
    cat(sprintf("%-10.2f", omegan[i]))
    cat(sprintf("      "))
    cat(sprintf("%-10.2f", omegahz[i]))
    cat(sprintf("\n"))
  }
  cat(sprintf("\n"))
 }
  return(list(omega = omegan, zeta = zeta))
}

