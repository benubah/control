
#' @export

initial <- function (sys, x0 = NULL, t = NULL) {

  sys_ss <- ssdata(sys)
  errmsg <- abcdchk(sys_ss)
  if (errmsg != "") {
    report <- "INITIAL: " + errmsg
    stop(report)
  }
  if(is.null(t)) {
    t <- seq(0, 5, 0.01)
  }
  if (is.null(x0)) {
    x0 <- pracma::rand(nrow(sys_ss[[1]]), 1)
  }
  n <- length(t)
  res <- lsim(sys_ss, pracma::zeros(n, ncol(sys_ss[[2]])), t, x0)
  x <- res$x
  y <- res$y

  return(list(t = t, y = y, x = x ))

}
