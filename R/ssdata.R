#' @title Retrieve State-space data
#'
#' @description
#' \code{ssdata} retrieves the model for a state-space system from a \code{sys} object
#'
#' @details
#' \code{ssdata} retrieves a model object for a state-space system, from a \code{sys} object of tf, ss and zpk classes
#'
#' @param sys1 an LTI system object of tf, ss or zpk classes
#'
#' @return Returns a list object of \code{ss} class containing A, B, C and D matrices
#'
#'
#' @seealso \code{\link{ss}} \code{\link{tfdata}} \code{\link{zpkdata}}
#'
#' @examples
#' sys1 <- tf(c(1), c(1,2,1))
#'  ssdata(sys1)
#'  A <- rbind(c(-2, -1), c(1,0)); B <- rbind(1,0);
#'  C <- cbind(0,1); D <- 0;
#'  sys2 <- ss(A,B,C,D)
#'  ssdata(sys2)
#'  ss2zp(ssdata(zpk(NULL, c(-1,-1), 1)))
#'
#' @export

ssdata <- function (sys1) {

  if (class(sys1) == "ss") {

    return(sys1)

  } else if(class(sys1) == "tf"){

    sssys <- tf2ss(sys1)
    return(sssys)

  } else if (class(sys1) == "zpk") {

    zpsys <- zp2ss(sys1)
    return(zpsys)

  } else {
    stop("ssdata: sys must be class of tf, ss or zpk")
  }
}


