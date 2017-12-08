#' @title Retrieve Transfer function data
#'
#' @description
#' \code{tfdata} retrieves the model for a transfer function from a \code{sys} object
#'
#' @details
#' \code{tfdata} retrieves a model object for a transfer function, from a \code{sys} object of tf, ss and zpk classes
#'
#' @param sys1 an LTI system object of tf, ss or zpk classes
#'
#' @return Returns a list object of \code{tf} class containing numerator and denominator coefficients in desecending values of s.
#'  For multiple-input multiple-output systems (MIMO) a list containing tf sys objects for as many outputs is returned
#'
#' @seealso \code{\link{tf}} \code{\link{ssdata}} \code{\link{zpkdata}}
#'
#' @examples
#' sys1 <- zpk(NULL, c(-1,-1), 1)
#' tfdata(sys1)
#' A <- rbind(c(-2, -1), c(1,0)); B <- rbind(1,0);
#' C <- cbind(0,1); D <- 0
#' tfdata( ss(A, B, C, D) )
#' tfdata(ss2zp( A,B,C,D))
#' tfdata(tf(c(1), c(1,2,1)))
#'
#'  ## MIMO system
#' A = rbind(c(0,1), c(-25,-4)); B = rbind(c(1,1), c(0,1));
#' C = rbind(c(1,0), c(0,1)); D = rbind(c(0,0), c(0,0))
#' tfdata(ss(A,B,C,D))
#'
#' @export

tfdata <- function ( sys1 ) {

  if (class(sys1) == "ss") {
    # MIMO systems
    if( ncol(sys1[[4]]) > 1) {
      systems <- vector("list", ncol(sys1[[4]]))
      for (i in 1:ncol(sys1[[4]])) { systems[[i]] <- ss2tf(sys1, i) }
      return(systems)
    } else {
    tfsys <- ss2tf(sys1)
    return(tfsys)
    }

  } else if (class(sys1) == "tf") {

    return(sys1)

  } else if (class(sys1) == "zpk") {
    tfsys <- zp2tf(sys1)
    return(tfsys)

  } else {
    stop("tfdata: sys must be class of tf, ss or zpk")
  }


}

