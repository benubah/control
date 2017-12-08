#' @title Proportional-Integral-Derivative (PID) Controller
#'
#' @description
#' \code{pid} Parallel form of the model of a PID controller
#'
#'
#' @details
#'  \code{pid} creates the transfer function model for a PID, PI, PD, and P-controller.
#'
#'
#' @param p Proportional gain. A real and finite value.
#' @param i Integral gain. A real and finite value. set this to zero for PD and P-control
#' @param d Derivative gain. A real and finite value. set this to zero for PI and P-control
#'
#' @return Returns a transfer function model for the PID, PI, PD or P-controller.
#'
#' @examples
#' C <- pid(350,300,50) # PID-control
#' P <- TF(" 1/(s^2 + 10* s + 20)")
#' T <- feedback(TF("C*P"), 1)
#' stepplot(T, seq(0,2,0.01))
#'
#' C <- pid(300,0,0) # P-control
#' T <- feedback(TF("C*P"), 1)
#' stepplot(T, seq(0,2,0.01))
#'
#' C <- pid(30,70,0) # PI-control
#' T <- feedback(TF("C*P"), 1)
#' stepplot(T, seq(0,2,0.01))
#'
#' C <- pid(300,0,10) # PD-control
#' T <- feedback(TF("C*P"), 1)
#' stepplot(T, seq(0,2,0.01))
#'
#' @export

pid <- function(p, i, d){
  #assign("K__p_", p, envir = .GlobalEnv)
  #assign("K__i_", i, envir = .GlobalEnv)
  #assign("K__d_", d, envir = .GlobalEnv)

  # pid in parallel form
  #mdl <- TF("(K__p_) + (K__i_/s) + (K__d_*s)")
  #cat("\n",paste("Kp + Ki * 1/s + Kd * s"), "\n")
  #cat("\n",paste("where Kp =", K__p_,",", "Ki =", K__i_, ",", "Kd =", K__d_), "\n")
  #cat("\n", "Continuous-time PID controller in parallel form.
    #  " ,"\n")


  ## OR
  ##PID-Control
   if(is.numeric(p) && p > 0  && is.numeric(i) && i > 0 && is.numeric(d) && d > 0) {
     mdl <- tf(c(d, p, i), c(1,0))
     cat("\n",paste("Kp + Ki * 1/s + Kd * s"), "\n")
     cat("\n",paste("where Kp =", p,",", "Ki =", i, ",", "Kd =", d), "\n")
     cat("\n", "Continuous-time PID-control in parallel form.
      " ,"\n")
     ## PD-Control
   } else if(is.numeric(p) && p > 0  && is.numeric(d) && d > 0 &&  i <= 0) {
      mdl <- tf(c(d, p), c(1))
     cat("\n",paste("Kp  + Kd * s"), "\n")
     cat("\n",paste("where Kp =", p,",", "Ki =", i, ",", "Kd =", d), "\n")
     cat("\n", "Continuous-time PD-control in parallel form.
      " ,"\n")
     ## PI-Control
   } else if(is.numeric(p) && p > 0  && is.numeric(i) && i > 0 &&  d <= 0) {
      mdl <- tf(c(d, p, i), c(1,0))
     cat("\n",paste("Kp + Ki * 1/s"), "\n")
     cat("\n",paste("where Kp =", p,",", "Ki =", i, ",", "Kd =", d), "\n")
     cat("\n", "Continuous-time PI-control in parallel form.
      " ,"\n")
     ## P-Control
   } else if(is.numeric(p) && p > 0  && d <= 0 &&  i <= 0) {
      mdl <- tf(c(p), c(1))
     cat("\n",paste("Kp "), "\n")
     cat("\n",paste("where Kp =", p,",", "Ki =", i, ",", "Kd =", d), "\n")
     cat("\n", "Continuous-time P-control in parallel form.
      " ,"\n")
   }
  return(mdl)
}
