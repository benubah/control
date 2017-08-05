# esort.R
#
# usage: a <- esort(p)
#
# This function sorts the complex continuous eigenvalues in descending
# order. It sorts based on the real part. The unstable eigenvalues (positive
# real part) will appear first.
#
# The function passes back the sorted eigenvalues and the cooresponding
# indices in a list:
#
#    a$s   = sorted eigenvalues
#    a$idx = index
#
#------------------------------------------------------------------------------
#' @export
esort <- function(p) {
  t <- sort(-Re(p), index.return = TRUE)
  return(list(s = p[t$ix], idx = t$ix))
}

