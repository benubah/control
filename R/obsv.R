# obsv.R
#
# Usage: obsv(A,C)
#
# This function creates the observability matrix.
# obsm <- obsv(A,C) returns the observability matrix.
# where
###     | C        |
##      | CA       |
# Obsm= | CA^2     |
##      | ...      |
##      | CA^(n-1) |

#' @export
obsv <- function(A, C) {
  obsm <- t(ctrb (t(A), t(C)))
  return(obsm)
}

