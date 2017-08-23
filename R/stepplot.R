
# Plot step response
# stepplot(tf(1, c(1,2,1)))
# stepplot(tf(1, c(1,2,1)), seq(0, 10, 0.1))

#MIMO
# A = rbind(c(0,1), c(-25,-4)); B = rbind(c(1,1), c(0,1));C = rbind(c(1,0), c(0,1)); D = rbind(c(0,0), c(0,0))
# stepplot(ss(A,B,C,D), input = 1:2) # OR
# stepplot(ss(A,B,C,D), input = 1:ncol(D))

#' @export
stepplot <- function (sys, t = NULL, input = 1) {
  if(is.null(t)) {
    t <- seq(0, 5, 0.01)
  }
  res <- step(sys, t, input[1])
  y <- res$y

  if (length(input) == 1) {
  # Plot the results for all rows of y against t
     if (nrow(y) == 1) {
       plot(t, y, type = "l", col = "blue", xlab = "Time, sec", ylab = paste("y"), main = "Step response")
       grid(5, 5)
      } else {
          par(mfrow = c(1, nrow(y)))
        for (i in 1:nrow(y)) {
          plot(t, y[i, ], type = "l", col = "blue", xlab = "Time, sec", ylab = paste("y", i), main = "Step response")
          grid(5, 5)
        }
        par(mfrow = c(1,1))
      }
  }
  if (length(input) > 1) {
    for(i in 1:length(input)){
      stepplot(sys, input = i)
      }
  }
}
