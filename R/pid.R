#

# Example
# C <- pid(350,300,50)

#' @export
pid <- function(p, i, d){
  assign("K__p_", p, envir = .GlobalEnv)
  assign("K__i_", i, envir = .GlobalEnv)
  assign("K__d_", d, envir = .GlobalEnv)

  # pid in parallel form
  mdl <- TF("K__p_ + (K__i_/s) + (K__d_*s)")
  cat("\n",paste("Kp + Ki * 1/s + Kd * s"), "\n")
  cat("\n",paste("where Kp =", K__p_,",", "Ki =", K__i_, ",", "Kd =", K__d_), "\n")
  cat("\n", "Continuous-time PID controller in parallel form.
      " ,"\n")

  return(mdl)
}
