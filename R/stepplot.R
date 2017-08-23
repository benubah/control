# Step Response Plot
#' @export
stepplot <- function (sys, t = NULL, input = 1) {

  if(is.null(t)) {
    tf <- seq(0, 5, 0.01)
  }
  if(!is.null(t)) {
    tf <- t

  }

  res <- step(sys, tf, input[1])
  y <- res$y
  t <- res$t
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
      stepplot(sys, tf, input = i)
      }
  }
}
