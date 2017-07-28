#' @title Series Connection of two systems
#'
#' @description \code{series} connects two systems in the series block form below
#'
#'          u --->[System1]--->[System2]----> y
#'
#' @details  \code{seriessys <- series(sys1, sys2)} connects the two state-space systems in series such that the outputs of sys1
#'	specified are connected to the inputs of sys2	specified by input2.
#'  If sys1 and sys2 are both transfer functions,  \code{series(systf1, systf2)} produces the SISO system
#'	in transfer function form obtained by connecting the two SISO
#'	transfer function systems in series.
#' If a system is not in state-space representation, the function
#' tries to form a state-space representation for such system.
#'
#' @param sys1 LTI system object of tf, ss or zpk class
#' @param sys2 LTI system object of tf, ss or zpk class
#'
#' @return The function returns a state-space model of the aggregate system with A, B, C, D matrices
#'
#' @seealso \code{\link{parallel}} \code{\link{feedback}} \code{\link{connect}}
#'
#' @examples
#' series(tf(1, c(1,2,3)), tf(2, c(2,3,5)))
#' sys2 = ss(1,2,3,4)
#' sys3 = ss(6,7,8,9)
#' series(sys2, sys3)
#' series(tf(1, c(1,2,3)), ss(1,2,3,4))
#'
#' @export

# series.R
#
# Syntax: seriessys <- series( sys1, sys2 )
#         seriessys <- series( sys1, sys2 ,outputs1,inputs2)
#
#
#	Series connection of two systems.
#
#		u --->[System1]--->[System2]----> y
#
#	seriessys <- series(sys1, sys2) produces an aggregate
#	state-space system consisting of the series connection of systems
#	1 and 2 that connects all the outputs of system 1 connected to
#	all the inputs of system 2, u2 <- y1.  The resulting system has
#	the inputs of system 1 and the outputs of system 2.
#
#	seriessys <- series(sys1, sys2, output1, input2)
#	connects the two system in series such that the outputs of system
#	1 specified by output1 are connected to the inputs of system 2
#	specified by input2.
# If sys1 and sys2 are both transfer functions,  series() produces the SISO system
#	in transfer function form obtained by connecting the two SISO
#	transfer function systems in series.
#
#	See also: append, parallel, feedback.
#
# Example:
# series(tf(1, c(1,2,3)), tf(2, c(2,3,5)))
# sys2 = ss(1,2,3,4)
# sys3 = ss(6,7,8,9)
# series(sys2, sys3)
# series(tf(1, c(1,2,3)), ss(1,2,3,4))

#' @export
series <- function (sys1, sys2, outputs, inputs) {

  if (class(sys1) == 'tf' && class(sys2) == 'tf') {
    csys1 <- tfchk(sys1$num, sys1$den)
    csys2 <- tfchk(sys2$num, sys2$den)
    sysnum <- pracma::polymul(c(csys1$numc), c(csys2$numc))
    sysden <- pracma::polymul(c(csys1$denc), c(csys2$denc))
    return(tf(sysnum, sysden))
  } else {
      sys1 <- ssdata(sys1)
      sys2 <- ssdata(sys2)
      errmsg <- abcdchk(sys1)
      if(errmsg != "") {
        stop("series: System 1: " + errmsg)
      }
      errmsg <- abcdchk(sys2)
      if(errmsg != "") {
        stop("series: System 2 " + errmsg)
      }
    num_y1 <- nrow( sys1[[4]])
    num_u1 <- ncol( sys1[[4]])

    if (num_u1 == 0) {
      dum <- nrow( sys1[[2]])
      num_u1 <- ncol( sys1[[2]])
      num_y2 <- nrow( sys1[[3]])
      dum <- ncol( sys1[[3]])
    }

    num_y2 <- nrow( sys2[[4]])
    num_u2 <- ncol( sys2[[4]])

    if (num_u2 == 0) {
      dum <- nrow( sys2[[2]])
      num_u2 <- ncol( sys2[[2]])
      num_y2 <- nrow( sys2[[3]])
      dum <- ncol( sys2[[3]])
    }

    if (nargs() == 2) {
      inputs1  <- 1:num_u1
      outputs1 <- 1:num_y1
      inputs2  <- num_u1 + 1:num_u2
      outputs2 <- num_y1 + 1:num_y2
    }
    if (nargs() == 4) {
      inputs1  <- 1:num_u1
      outputs1 <- outputs
      inputs2  <- inputs + num_u1
      outputs2 <- 1:num_y2 + num_y1
    }
    if (length(outputs1) != length(inputs2)) {
      stop("Series: System connection sizes mismatch.")
    }
    # ---Create Connection ---
    appsys <- append(sys1, sys2)
    clpsys <- cloop(appsys, outputs1, inputs2)
    seriessys <- selectsys(clpsys,inputs1,outputs2)
    return(seriessys)
  }
}

