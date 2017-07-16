# selectsys.R
#
# Usage: A <- selectsys(statesys,inputs,outputs,states)
#
# This  function extracts a subsystem from a larger state-space system.
# subsys <- selectsys(statesys,inputs,outputs) will extract a state
# space subsystem with the specified inputs and outputs.
#
# subsys <- selectsys(a,b,c,d,inputs,outputs,states) will return
# the state space subsystem with the specified inputs, outputs, and states.
#
# Return: The matrices  a_extracted,  b_extracted, c_extracted, and  d_extracted are returned in a list.
#
# Example:
# A <- rbind(c(33,2,5), c(23,200,2), c(9,2,45))
# B <- rbind(c(4,5), c(12,5), c(82,1))
# C <- rbind(c(34,56,2), c(6,2,112))
# D <- rbind(c(2,0), c(0,19))
#  selectsys(sys1, 1, 1) # extract subsystem for only input 1 and output 1
#  selectsys(sys1, 2,2) # extract subsystem for only input 2 and output 2
#  selectsys(sys1, 2, 1:2) # extract subsystem for only input 1 and output 1 to 2
#  selectsys(sys1, 1:2, 2) # extract subsystem for only input 1 to 2 and output 2 to 2
#  selectsys(sys1, 2, 2, 1:2) # extract subsystem for only input 2 and output 2 but states 1 to 2

selectsys <- function(statesys, inputs, outputs, states) {
  errmsg <- abcdchk(statesys)
  if (errmsg != "") {
    stop("selectsys: " + errmsg)
  }
  a <- statesys[[1]]
  b <- statesys[[2]]
  c <- statesys[[3]]
  d <- statesys[[4]]

  if (nargs() == 3) {
    stateselect <- 1:nrow(a)
  }
  if (nargs() == 4) {
       stateselect <- states
  }
  if (!is.null(a)) {
     a_extracted <- a[stateselect, stateselect]
  } else {
     a_extracted <- c()
    }
  if (!is.null(b)) {
     b_extracted <- b[stateselect, inputs, drop = FALSE]
  }  else {
     b_extracted <- c()
  }
  if (!is.null(c)) {
     c_extracted <- c[outputs, stateselect, drop = FALSE]
  }  else {
     c_extracted <- c()
  }
  if (!is.null(d)) {
     d_extracted <- d[outputs, inputs, drop = FALSE]
  }  else {
     d_extracted <- c()
  }

  return(ss(a_extracted, b_extracted, c_extracted, d_extracted))
}

