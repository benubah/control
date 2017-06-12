# Function to return transfer function from system object of classes 'ss', 'tf' and 'zpk'
# Usage: tfcn <- tfdata(sys)
#        tfcn$num
#        tfcn$den
# Example: sys1 <- zpk(NULL, c(-1,-1), 1)
#          tfdata(sys1)
#          A <- rbind(c(-2, -1), c(1,0)); B <- rbind(1,0);
#          C <- cbind(0,1); D <- 0
#          tfdata( ss(A, B, C, D) )
#          tfdata(ss2zp( A,B,C,D))
#          tfdata(tf(c(1), c(1,2,1)))
#

tfdata <- function ( sys1 ) {

  if (class(sys1) == "ss") {

    tfsys <- ss2tf(sys1)
    return(tfsys)

  } else if (class(sys1) == "tf") {

    return(sys1)

  } else if (class(sys1) == "zpk") {
    tfsys <- zp2tf(sys1)
    return(tfsys)

  } else {
    stop("tfdata: sys must be class of tf, ss or zpk")
  }


}

