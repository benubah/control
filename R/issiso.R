#' @title SISO / MIMO Check
#'
#' @aliases ismimo
#'
#' @description
#' \code{issiso} checks if state-space system is a single-input single-output system
#' \code{ismimo} checks if state-space system is a multiple-input multiple-output system
#'
#'
#' @param sys Dynamic system of state-space model
#'
#' @return Returns TRUE or FALSE
#'
#'
#' @export
issiso <- function (sys) {

  if (class(sys) == 'ss'){
    if (nrow(sys[[4]]) * ncol(sys[[4]]) == 1){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}


#' @export
ismimo <- function (sys) {

  if (class(sys) == 'ss'){
    if (nrow(sys[[4]]) * ncol(sys[[4]]) > 1){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}
