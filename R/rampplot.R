# Ramp response plot

#' @export

rampplot <- function (sys, t = NULL, input = 1) {
 res <- ramp(sys, t, input[1])
 y <- res$y
 t <- res$t
 if (length(input) == 1) {
 # Plot the results for all rows of y against t
 if (nrow(y) == 1) {
   plot(t, y, type = "l", lwd = 2, col = "blue", xlab = "Time, sec", ylab = paste("y"), main = "Ramp response")
   grid(5, 5)
 } else {
   par(mfrow = c(1, nrow(y)))
   for (i in 1:nrow(y)) {
     plot(t, y[i, ], type = "l", lwd = 2, col = "blue", xlab = "Time, sec", ylab = paste("y", i), main = "Ramp response")
     grid(5, 5)
   }
   par(mfrow = c(1,1))
 }
}
 if (length(input) > 1) {
   for(i in 1:length(input)){
     rampplot(sys, t, input = i)
   }
 }
}
