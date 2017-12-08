
#' @export
initialplot <- function (sys, x0 = NULL, t = NULL) {

  res <- initial(sys, x0, t)
  y <- res$y
  t <- res$t
  # Plot the results for all rows of y against t
  if (nrow(y) == 1) {
    graphics::plot(t, y, type = "l", lwd = 2, col = "blue", xlab = "Time, sec", ylab = paste("y"), main = "Initial response")
    graphics::grid(5, 5)
  } else {
    graphics::par(mfrow = c(nrow(y), 1))
    for (i in 1:nrow(y)) {
      graphics::plot(t, y[i, ], type = "l", lwd = 2, col = "blue", xlab = "Time, sec", ylab = paste("y", i), main = "Initial response")
      graphics::grid(5, 5)
    }
    graphics::par(mfrow = c(1,1))
  }
}
