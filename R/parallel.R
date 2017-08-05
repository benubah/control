#' @title Parallel Connection of two systems
#'
#' @usage parallel(sys1, sys2)
#' parallel(sys1, sys2, IN1, IN2, OUT1, OUT2)
#'
#' @description \code{parallel} connects two systems in the parallel block form below
#'
#'               |-->[System1]--|
#            u-->+              0--->y
#                |<--[System2]--|
#
#' @details  \code{psys <-  parallel(sys1, sys2)} produces a state-
#'	space system consisting of the parallel connection of sys1
#'	and sys2 that connects all the inputs together and sums all the
#'	outputs of the two systems.
#'
#'  The parallel connection
#'	is performed by appending the two systems, summing the specified
#'	inputs and outputs, and removing the, now redundant, inputs and
#'	outputs of system 2.
#'
#'	If sys1 and sys2 are transfer functions, then parallel(sys1, sys2) produces a parallel
#'	connection of the two transfer function systems.
#'
#'	\code{parallel(sys1, sys2,IN1,IN2,OUT1,OUT2)}
#'	connects the two systems in parallel by connecting the inputs
#'	specified by IN1 and IN2 and by summing the outputs specified
#'	by OUT1 and OUT2. The vector IN1 contains
#'	indexes into the input vectors of sys1 while, IN2 contains indexes for sys2,
#'	. Vectors OUT1 and OUT2 contain	indexes for the outputs of the sys1 and sys2 respectively.
#'
#' @param sys1 LTI system object of tf, ss or zpk class
#' @param sys2 LTI system object of tf, ss or zpk class
#'
#' @return The function returns a state-space model of the parallel-connected system with A, B, C, D matrices
#'
#' @seealso \code{\link{series}} \code{\link{feedback}} \code{\link{connect}}
#'
#' @examples
#' sys2 = ss(1,2,3,4)
#' sys3 = ss(6,7,8,9)
#' parallel(sys2, sys3)
#' parallel(tf(1, c(1,2,3)), ss(1,2,3,4))
#' parallel(tf(1, c(1,2,3)),tf(2, c(3,2,3)))
#' @export

parallel <- function (sys1, sys2, in1, in2, out1, out2) {

  if (class(sys1) == 'tf' && class(sys2) == 'tf') {
    csys1 <- tfchk(sys1$num, sys1$den)
    csys2 <- tfchk(sys2$num, sys2$den)
    nn <- nrow(csys1$numc)
    mn <- ncol(csys1$numc)
    #initialize num
    num <- matrix(pracma::polymul(c(csys1$numc[1, ]), c(csys2$denc)), nrow = nn)
    for (k in 1:nn) {
      num[k, ] <- pracma::polymul(c(csys1$numc[k, ]), c(csys2$denc) + pracma::polymul(c(csys2$numc[k, ]), c(csys1$denc)))
      den <- pracma::polymul(c(csys1$denc), c(csys2$denc))
    }
    return(tf(num, den))

  } else {
    sys1 <- ssdata(sys1)
    sys2 <- ssdata(sys2)

    errmsg <- abcdchk(sys1)
    if(errmsg != "") {
      stop("Parallel: System 1: " + errmsg)
    }
    errmsg <- abcdchk(sys2)
    if(errmsg != "") {
      stop("Parallel: System 2 " + errmsg)
    }
    num_y1 <- nrow(sys1$D)
    num_u1 <- ncol(sys1$D)
    num_y2 <- nrow(sys2$D)
    num_u2 <- ncol(sys2$D)

    if (nargs() == 2) {
      inputs1 <- 1:num_u1
      outputs1 <- 1:num_y1
      inputs2 <- (1:num_u2) + num_u1
      outputs2 <- (1:num_y2) + num_y1
    }
    if (nargs() == 6) {
      inputs1 <- in1
      outputs1 <- out1
      inputs2 <- in2 + num_u1
      outputs2 <- out2 + num_y1
    }

    inputs1 <- as.matrix(inputs1)
    outputs1 <- as.matrix(outputs1)
    inputs2 <- as.matrix(inputs2)
    outputs2 <- as.matrix(outputs2)

    # Check that I/O sizes match
    if (max(dim(inputs1)) != max(dim(inputs2))) {
      stop("parallel: Input Sizes don't match.")
    }
    if (max(dim(outputs1)) != max(dim(outputs2))) {
      stop("parallel: Output sizes don't match.")
    }
    # Connecting systems in Parallel
    appsys <- append(sys1, sys2)
    a <- appsys[[1]]
    b <- appsys[[2]]
    c <- appsys[[3]]
    d <- appsys[[4]]
    # Connecting inputs
    if (!is.null(b)) {
      b[ , inputs1] <- b[ , inputs1] + b[ , inputs2]
    }
    if (!is.null(d)) {
      d[ , inputs1] <- d[ , inputs1] + d[ , inputs2]
    }
    # Connecting outputs
    if (!is.null(c)) {
      c[outputs1, ] <- c[outputs1, ] + c[outputs2, ]
    }
    if (!is.null(d)) {
      d[outputs1, ] <- d[outputs1, ] + d[outputs2, ]
    }
    newsys <- removesys(ss(a, b, c, d), inputs2, outputs2)
    return(newsys)
  }
}




