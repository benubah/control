
# Plot linear simulation
# signal <- gensig('square',4,10,0.1)
# H <- tf(c(2, 5, 1),c(1, 2, 3))
# lsimplot(H, signal$u, signal$t)

#' @export

lsimplot <- function (sys, u, t, x0 = NULL) {

  resp <- lsim(sys, u, t, x0)
  if (nrow(resp$y) > 1) {
    for (i in 1:nrow(resp$y)) {
      plot(t, resp$y[1,], type = "l", xlab = "Time, sec", ylab = paste("y", i), main = "Linear Simulation Response", col = "blue"); grid(7,7)
    }
  } else {
     plot(t, resp$y, type = "l", xlab = "Time, sec", ylab = paste("y", 1), main = "Linear Simulation Response", col = "blue"); grid(7,7)
  }
}
