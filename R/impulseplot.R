# Example:
# A <- rbind(c(-2, -1), c(1,0)); B <- rbind(1,0);
#          C <- cbind(0,1); D <- as.matrix(0);
# impulseplot(ss(A,B,C,D))
# MIMO
# A1 <- rbind(c(0,1), c(-25,-4)); B1 <- rbind(c(1,1), c(0,1))
# C1 <- rbind(c(1,0), c(0,1)); D1 <- rbind(c(0,0), c(0,0))
# impulseplot(ss(A1,B1,C1,D1), 1:2)

#par(mfrow = c(2,2)) #run first to see all plots for 2-input, 2-output MIMO
#' @export
impulseplot <- function(sys, input = 1, t = NULL) {
  res <- impulse(sys, input[1], t)
  y <- res$y
  t <- res$t

  if (length(input) == 1) {
    # Plot the results for all rows of y against t
    if (nrow(y) == 1) {
      plot(t, y, type = "l", col = "blue", xlab = "Time, sec", ylab = paste("y"), main = "Impulse response")
      grid(5, 5)
    } else {
      par(mfrow = c(1, nrow(y)))
      for (i in 1:nrow(y)) {
        plot(t, y[i, ], type = "l", col = "blue", xlab = "Time, sec", ylab = paste("y", i), main = "Impulse response")
        grid(5, 5)
      }

    }
  }
  if (length(input) > 1) {

    for(i in 1:length(input)){
      impulseplot(sys, i)
    }
  }
  par(mfrow = c(1,1))
}
