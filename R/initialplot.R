
# Example:
# A <- rbind(c(-2, -1), c(1,0)); B <- rbind(1,0);
#          C <- cbind(0,1); D <- as.matrix(0);
# x0 <- matrix(c( 0.51297, 0.98127))
# initialplot(ss(A,B,C,D), x0)

#' @export
initialplot <- function (sys, x0 = NULL, t = NULL) {

  res <- initial(sys, x0, t)
  y <- res$y
  t <- res$t
  # Plot the results for all rows of y against t
  if (nrow(y) == 1) {
    plot(t, y, type = "l", lwd = 2, col = "blue", xlab = "Time, sec", ylab = paste("y"), main = "Initial response")
    grid(5, 5)
  } else {
    par(mfrow = c(nrow(y), 1))
    for (i in 1:nrow(y)) {
      plot(t, y[i, ], type = "l", lwd = 2, col = "blue", xlab = "Time, sec", ylab = paste("y", i), main = "Initial response")
      grid(5, 5)
    }
    par(mfrow = c(1,1))
  }
}
