
#  removesys.R
#
# Syntax: sys <-  removesys(statesys,input,outputs)
#         sys <- removesys(statesys,input,outputs, states)
#
# This function removes inputs, outputs, and state from a state-space system.
#
# sys <- removesys(statesys,input,outputs) will remove the
# specified inputs and outputs from the system.
#
# sys <- removesys(statesys,inputs,outputs,states) will
# also return a state-space model with the specified inputs, outputs, and
# states removed from the system.
# Example:
# A <- rbind(c(33,2,5), c(23,200,2), c(9,2,45))
# B <- rbind(c(4,5), c(12,5), c(82,1))
# C <- rbind(c(34,56,2), c(6,2,112))
# D <- rbind(c(2,0), c(0,19))
# removesys(sys1, 1,2) # removes input 1 and output 2

#' @export
removesys <- function (statesys, inputs, outputs, states) {
  errmsg <- abcdchk(statesys)
  if (errmsg != "") {
    stop("selectsys: " + errmsg)
  }
  a <- statesys[[1]]
  b <- statesys[[2]]
  c <- statesys[[3]]
  d <- statesys[[4]]

  if (nargs() == 3) {
     states <- NULL  # CHECK
  }
   a_rows <- nrow(a)
   a_cols <- ncol(a)
   d_rows <- nrow(d)
   d_cols <- ncol(d)

   a_rmv <- a
   b_rmv <- b
   c_rmv <- c
   d_rmv<- d

  if (length(states) !=  a_rows) {
    if (!is.null(a)) {
      if(is.null(states)){
        a_rmv[ , states] <- NULL
        a_rmv[states, ] <- NULL
      } else {
        a_rmv <- a_rmv[ , -states, drop = FALSE]
        a_rmv <- a_rmv[-states, , drop = FALSE]
      }

    } else {
      a_rmv <- c()
    }
    if (!is.null(b)) {
      if(is.null(states)){
        b_rmv[states, ] <- NULL
      } else {
        b_rmv <-  b_rmv[-states, , drop = FALSE]
      }
      b_rmv <-  b_rmv[, -inputs, drop = FALSE]
    }  else {
      b_rmv <- c()
    }
    if (!is.null(c)) {
      if(is.null(states)){
        c_rmv[ , states] <- NULL
      } else {
        c_rmv  <-  c_rmv[ , -states, drop = FALSE]
      }
      c_rmv <-  c_rmv[-outputs, , drop = FALSE]

    }  else {
      c_rmv <- c()
    }
  } else {
    a_rmv <- c()
    b_rmv <- c()
    c_rmv <- c()
  }
  inputs <- as.matrix(inputs)
  if ( (max(dim(inputs)) != d_cols) && (!is.null(d)) ) {
    d_rmv <- d_rmv[, -inputs, drop = FALSE]
    d_rmv <- d_rmv[-outputs, , drop = FALSE]
  }  else {
    d_rmv<- c()
  }
  return(ss(a_rmv, b_rmv, c_rmv, d_rmv))
}

