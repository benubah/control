#  This function converts an nth order ordinary differential equation:
# a(n) d^nx/dt^n + a(n-1) d^(n-1)x/dt^(n-1) + . . + a(1)dx/dt + a(0)x  =  k
# to the state-space phase variable form.
# The function returns the A, B, C matrices, while D is usually zero.

# EXAMPLE
# 2 d^3x/dt^3 + 4 d^2x/dt^2 + 6 dx/dt + 8 = 10 u(t)
# coef_i <- c(2,4,6,8)
# k <- 10
# ode2ss(coef_i, k)

#' @export
ode2ss <- function (coef_i, k) {
  n <- length(coef_i) - 1
  I  <- pracma::eye(n - 1)
  z <- pracma::zeros(n - 1, 1)
  zi <- cbind(z, I)
  coef_n <- matrix(0, nrow = 1, ncol = n)
  for (m in 1:n) {
    coef_n[m] <- coef_n[m] - coef_i[n+2-m]/coef_i[1]
  }
  A <- rbind(zi, coef_n)
  B <- rbind(z, k/coef_i[1])
  C <- cbind(1, t(z))

  return(list(A = A, B = B, C = C))
}
