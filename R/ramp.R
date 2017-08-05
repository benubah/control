# Ramp response
# ramp(tf(c(2,1), c(1,1,1)))

#MIMO
#A = rbind(c(0,1), c(-25,-4)); B = rbind(c(1,1), c(0,1));C = rbind(c(1,0), c(0,1)); D = rbind(c(0,0), c(0,0))
#ramp(ss(A1,B1,C1,D1))
#ramp(ss(A1,B1,C1,D1), input = 1)
#ramp(ss(A1,B1,C1,D1), input = 2)

#' @export

ramp <- function (sys, t = NULL, input = 1) {

  sys_ss <- ssdata(sys)
  errmsg <- abcdchk(sys_ss)
  if (errmsg != "") {
    report <- "RAMP: " + errmsg
    stop(report)
  }
  if(is.null(t)) {
    t <- seq(0, 5, 0.01)
  }
    # only iu-th input related items needed
    if (!is.null(sys_ss$B)) {
      sys_ss$B <- sys_ss$B[ , input, drop = FALSE]
    }
    sys_ss$D <- sys_ss$D[ ,input, drop = FALSE]

  r <- as.matrix(t)
  res <- lsim(sys_ss, r, t)
  x <- res$x
  y <- res$y

  return(list(t = t, y = y, x = x ))

}
