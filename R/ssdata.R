# Function to return state-space data from system object  of classes 'ss' and 'tf'
# Usage: states <- ssdata(sys)
#        states$A
#        states$B
#        states$C
#        states$D
# Example: sys1 <- tf(c(1), c(1,2,1))
#          ssdata(sys1)
#          A <- rbind(c(-2, -1), c(1,0)); B <- rbind(1,0);
#          C <- cbind(0,1); D <- 0;
#          sys2 <- ss(A,B,C,D)
#          ssdata(sys2)
#          ss2zp(ssdata(zpk(NULL, c(-1,-1), 1)))

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


