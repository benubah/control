#' @title Retrieve zero-pole data from LTI system object
#'
#' @description
#' \code{zpkdata} retrieves the model for a zero-pole-gain system from a \code{sys} object
#'
#' @details
#' \code{zpkdata} retrieves a model object for a zero-pole-gain system, from a \code{sys} object of \code{tf}, \code{ss} and \code{zpk} classes
#'
#' @param sys an LTI system object of \code{tf}, \code{ss} or \code{zpk} classes
#'
#' @return Returns a list object of \code{zpk} class containing zero, pole and gain matrices. For multivariable systems,
#' the zeros of each system is listed as a column in the zeros matrix, the poles are listed as a column-vector as well as the
#' gain
#'
#'
#'
#' @seealso \code{\link{zpk}} \code{\link{tfdata}} \code{\link{ssdata}}
#'
#' @examples
#'
#' sys1 <- zpk(NULL, c(-1,-1), 1)
#' zpkdata(sys1)
#' sys3 <- tf(c(1), c(1,2,1))
#' zpkdata(sys3)
#'
#' ## MIMO system of 2-inputs and 2-outputs
#' A = rbind(c(0,1), c(-25,-4)); B = rbind(c(1,1), c(0,1));
#' C = rbind(c(1,0), c(0,1)); D = rbind(c(0,0), c(0,0))
#' zpkdata(ss(A,B,C,D))
#'
#' ## OR
#' syszp <- zpkdata(ss(A,B,C,D))
#' syszp[[1]]
#' syszp[[2]]
#' syszp[[1]]$z # retrieve zeros of system 1 - Input 1 to Outputs 1 and 2
#' syszp[[2]]$z # retrieve zeros of system 2 - Input 2 to Outputs 1 and 2
#'
#' @export

zpkdata <- function (sys1) {

  if (class(sys1) == "tf") {
    zpsys <- tf2zp(sys1)
    return(zpsys)

  } else if (class(sys1) == 'ss') {
    # MIMO systems
    if( ncol(sys1[[4]]) > 1) {
      systems <- vector("list", ncol(sys1[[4]]))
      for (i in 1:ncol(D)) { systems[[i]] <- ss2zp(sys1, i) }
      return(systems)
    } else {
      zpsys <- ss2zp(sys1)
      return(zpsys)
    }

  } else if (class(sys1) == "zpk") {
    return(sys1)

  } else {
    stop("zpkdata: sys must be class of tf, ss or zpk")
  }


}

