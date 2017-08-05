

# Ramp response plot
# rampplot(tf(c(2,1), c(1,1,1)))

#MIMO
#A = rbind(c(0,1), c(-25,-4)); B = rbind(c(1,1), c(0,1));C = rbind(c(1,0), c(0,1)); D = rbind(c(0,0), c(0,0))
#rampplot(ss(A1,B1,C1,D1))
#rampplot(ss(A1,B1,C1,D1), input = 1)
#rampplot(ss(A1,B1,C1,D1), input = 2)
#' @export

rampplot <- function (sys, t = NULL, input = 1) {

 res <- ramp(sys, t, input[1])
 y <- res$y
 t <- res$t
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
