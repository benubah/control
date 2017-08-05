
# Step response


#' @export
step <- function (sys, t = NULL, input = 1) {

  if(is.null(t)) {
    t <- seq(0,5,0.01)
  }
    sys_ss <- ssdata(sys)

  num_y <- nrow(sys_ss[[4]])
  num_u <- ncol(sys_ss[[4]])
  if (num_u*num_y == 0) {
    return(list(t=c(), x=c(), y=c()))
  }

  if (input != 1) {
    # only iu-th input related items needed
    if (!is.null(sys_ss$B)) {
      sys_ss$B <- sys_ss$B[ , input, drop = FALSE]
    }
    sys_ss$D <- sys_ss$D[ ,input, drop = FALSE]
  }

  dims <- dim(as.matrix(t))
  u <- matrix(rep(1,dims[1]), dims[1], dims[2])
  if (ncol(sys_ss[[4]]) > 1){
    u <- pracma::ones(dims[1], ncol(sys_ss[[4]]))
  }
  resp <- lsim(sys_ss, u, t)
  return(list(t = t, x = resp$x, y = resp$y))
}
