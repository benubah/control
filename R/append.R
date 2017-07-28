#' @title Append the dynamics of a set of systems
#'
#' @usage append(sys1, sys2, sys3, ..., sysN)
#'
#' @description \code{append} appends the dynamics of a set of n-state-space systems together
#'
#' @details  \code{append(sys1, sys2, sys3,...sysN)} first combines the the first two systems and then
#' goes on to combine  the resulting state-space system to the next system and so forth.
#' This is achieved by calling the \code{sysgroup(sys1, sys2)} at each iteration to group
#' the systems in consecutive pairs until all systems are
#' completely appended to form one system.
#' If a system is not in state-space representation, the function
#' tries to form a state-space representation for such system.
#'
#' @return The function returns a state-space model of the formed appended system with A, B, C, D matrices
#'
#' @examples
#' sys1 <- ss(1,2,3,4)
#' sys2 <- ss(2,3,4,5)
#' sys3 <- ss(6,7,8,9)
#' append(sys1, sys2, sys3)
#' sys4 <- tf(1, c(1,2,5))
#' append(sys1, sys2, sys4)
#'
#' @export

append <- function (...) {
  args1 <- list(...)
  sys <- args1[[1]]
  if (length(args1) > 1) {
    for (k in 2 : length(args1)) {
      sys <- sysgroup(sys, args1[[k]]);
    }
  }
  return(sys)
}

#' @export
sysgroup <- function (sys1, sys2) {

  if (nargs() < 2) {
    stop("SYSGROUP: You must group at least two systems.")
  }
  if (class(sys1) != 'ss') {
    sys1 <- ssdata(sys1)
  }
  if (class(sys2) != 'ss') {
    sys2 <- ssdata(sys2)
  }
  errmsg <- abcdchk(sys1)
  if (errmsg != "") {
    dim_report <- "System 1: " + errmsg
    stop(dim_report)
  }
  errmsg <- abcdchk(sys2)
  if (errmsg != "") {
    dim_report <- "System 2: " + errmsg
    stop(dim_report)
  }
  a0  <- cbind(sys1[[1]], matrix(0, nrow(sys1[[1]]), ncol(sys2[[1]])))
  a02 <- cbind(matrix(0, nrow(sys2[[1]]), ncol(sys1[[1]])), sys2[[1]])
  aa  <- rbind(a0, a02)

  b0  <- cbind(sys1[[2]], matrix(0, nrow(sys1[[1]]), ncol(sys2[[4]])))
  b02 <- cbind(matrix(0, nrow(sys2[[1]]), ncol(sys1[[4]])), sys2[[2]])
  ba  <- rbind(b0,b02)

  c0  <- cbind(sys1[[3]], matrix(0, nrow(sys1[[4]]), ncol(sys2[[1]])))
  c02 <- cbind(matrix(0, nrow(sys2[[4]]), ncol(sys1[[1]])), sys2[[3]])
  ca  <- rbind(c0,c02)

  d0  <- cbind(sys1[[4]], matrix(0, nrow(sys1[[4]]), ncol(sys2[[4]])))
  d02 <- cbind(matrix(0, nrow(sys2[[4]]), ncol(sys1[[4]])), sys2[[4]])
  da  <- rbind(d0,d02)

  return(ss(aa, ba, ca, da))
}
