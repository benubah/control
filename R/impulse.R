# plot utility for impulse
# Example:
# A <- rbind(c(-2, -1), c(1,0)); B <- rbind(1,0);
#          C <- cbind(0,1); D <- as.matrix(0);
# t <- seq(0,3,0.01)
# iu <- 1
# res <- impulse(A,B,C,D);
# res <- impulse(A,B,C,D,iu,t);

#' @export
impulse <- function(sys, input = 1, t = NULL) {
  if(is.null(t)) {
    t <- seq(0,5,0.01)
  }
  sys_ss <- ssdata(sys)

  num_y <- nrow(sys_ss[[4]])
  num_u <- ncol(sys_ss[[4]])
  if (num_u*num_y == 0) {
    return(list(t=c(), x=c(), y=c()))
  }
  if (!is.null(sys_ss$B)) {
    sys_ss$B <- sys_ss$B[ , input, drop = FALSE]
  }
  sys_ss$D <- sys_ss$D[ ,input, drop = FALSE]
  x0 <- sys_ss$B
  dims <- dim(as.matrix(t))
  iu <- pracma::zeros(dims[1],dims[2])
  res <- lsim(sys_ss, iu, t, x0)
  x <- res$x
  y <- res$y
  return(list(t = t, x = x, y = y))
}
