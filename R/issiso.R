# checks if state-space system is a single-input single-output system

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

# checks if state-space system is a multiple-input multiple-output system
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
