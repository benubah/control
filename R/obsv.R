#' @title Observability Matrix
#'
#' @description
#' This function creates the observability matrix.
#'
#' @param A State matrix, A
#' @param C State matrix, C
#'
#' @return  \code{obsv(A, C)} returns the observability matrix.
#' where
#'      | C        |
#'      | CA       |
#'obsm =| CA^2     |
#'      | ...      |
#'      | CA^(n-1) |
#'
#'@seealso \code{\link{ctrb}}
#'
#' @examples
#' A <- rbind(c(0,1), c(-25,-4))
#' C <- rbind(c(1,0), c(0,1))
#'  obsv(A, C)
#'
#' @export

obsv <- function(A, C) {
  obsm <- t(ctrb (t(A), t(C)))
  return(obsm)
}

