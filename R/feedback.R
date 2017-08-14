#----------------------------------------------------------------------
#
# feedback.R
#
# Usage: fdsys <- feedback(sys1, sys2, e, f)
#
# Feedback connection of two systems.
#
#         u-->O-->[System1]---+--->y
#             |               |
#             +---[System2]<--+
#
# fdsys <- feedback(sys1, sys2, SIGN) produces an
# aggregate state-space system consisting of the feedback connection
# of the two systems 1 and 2.  Typically, system 1 is a plant and
# system 2 is a compensator.   If SIGN=1 then positive feedback is
# used. If SIGN=-1 then negative feedback is used.  In all cases,
# the resulting system has the same inputs and outputs as system 1.
#
# fdsys <- feedback(sys1, sys2, inputs, outputs)
# produces the feedback system formed by feeding all the outputs of
# system2 into the inputs of system 1 specified by INPUTS1 and by
# feeding the outputs of system 2 specified by OUTPUTS1 into all the
# inputs of system 2.  Positive feedback is assumed.  To connect
# with negative feedback, use negative values in the vector INPUTS1.
#
# When sys1 and sys2 are transfer functions, feedback(sys1, sys2, SIGN) produces the SISO
# closed loop system in transfer function form obtained by
# connecting the two SISO transfer function systems in feedback
# with the sign SIGN.
#
# See also: cloop, parallel, series, and connect.
#

# C <- pid(350,300,50)
# P <- TF(" 1/(s^2 + 10* s + 20)")
# feedback(C,P)
# feedback(P,P,1)
# feedback(P,P,-1)
# feedback(P,P)
# feedback(TF("C*P")) # On Octave: feedback(C*P)
# feedback(sys2, tf(1,c(1,2,1)))

# Similar to Octave, feedback(C, P)

#' @export
feedback <- function(sys1, sys2, in1, out1){

  if(nargs() == 1){
    res <- cloop(sys1)
  } else if(nargs() == 2) {

    if( is.numeric(sys2) && length(sys2) == 1 && is.list(sys1) ) {
      res <- cloop(sys1, -sys2)
    } else if (is.numeric(sys1) && length(sys1) == 1 && is.list(sys2)) {
      if( class(sys2) == 'tf') {
        res <- fdbcksys(tf(sys1, 1), sys2)
      }
      if( class(sys2) != 'tf') {
        res <- fdbcksys(tf(sys1, 1), tfdata(sys2))
        res <- ssdata(res)
      }
    } else {
      res <- fdbcksys(sys1, sys2)
    }

  } else if(nargs() == 3){
    res <- fdbcksys(sys1, sys2, in1)
  } else if (nargs() == 4) {
    res <- fdbcksys(sys1, sys2, in1, out1)
  }

  return(res)
}



#' @export
fdbcksys <- function(sys1, sys2, in1, out1) {

  if (class(sys1) == 'tf' && class(sys2) == 'tf') {
    # Assume negative feedback for tf without sign
    csys1 <- tfchk(sys1$num, sys1$den)
    csys2 <- tfchk(sys2$num, sys2$den)
    sgn <- -1
    #print(csys1$numc)
    #print(csys2$numc)
    #print(pracma::polymul(c(csys1$denc), c(csys2$denc)))
    #print(sgn * pracma::polymul(c(csys1$numc),c(csys2$numc)))
    if (nargs() == 3) {
      sgn <- sign(in1)
    }
    sysnum <- pracma::polymul(c(csys1$numc), c(csys2$denc))
    dentmp1 <- pracma::polymul(c(csys1$denc), c(csys2$denc))
    dentmp2 <- sgn * pracma::polymul(c(csys1$numc),c(csys2$numc))
    if ( length(dentmp1) < length(dentmp2) ) {
      dentmp1 <- cbind( matrix(0, 1, length(dentmp2) - length(dentmp1) ), dentmp1)
    }
    if ( length(dentmp2) < length(dentmp1) ) {
      dentmp2 <- c( rep(0, length(dentmp1) - length(dentmp2) ), dentmp2)
    }
    sysden <- dentmp1  - dentmp2
    return(tf(sysnum, sysden))

  } else {

     sys1 <- ssdata(sys1)
    sys2 <- ssdata(sys2)
    errmsg <- abcdchk(sys1)
    if(errmsg != "") {
      stop("Parallel: System 1: " + errmsg)
    }
    errmsg <- abcdchk(sys2)
    if(errmsg != "") {
      stop("Parallel: System 2 " + errmsg)
    }
    num_y1 <- nrow(sys1$D)
    num_u1 <- ncol(sys1$D)
    num_y2 <- nrow(sys2$D)
    num_u2 <- ncol(sys2$D)

    if (nargs() == 2) {
     # assume negative feedback for systems without sign
      inputs1  = -(1:num_u1)
      outputs1 =  1:num_y1
      inputs2  =  (1:num_u2) + num_u1
      outputs2 =  (1:num_y2) + num_y1
    }
    if (nargs() == 3)  {
      # ss Systems with sign
      inputs1  = (1:num_u1)*sign(in1)
      outputs1 = 1:num_y1
      inputs2  = (1:num_u2) + num_u1
      outputs2 = (1:num_y2) + num_y1
    }
    if (nargs() == 4) {
      # ss Systems input and output vectors
      inputs1  = in1
      outputs1 = out1
      inputs2  = (1:num_u2) + num_u1
      outputs2 = (1:num_y2) + num_y1
    }
    if ((max(dim(as.matrix(outputs1))) != max(dim(as.matrix(inputs2)))) || (max(dim(as.matrix(outputs2)))!=max(dim(as.matrix(inputs1))))) {
      stop("Parallel: Feedback connection sizes mismatch.")
    }
    # Create feedback system
    appsys <- append(sys1, sys2)
    clpsys <- cloop(appsys, cbind(outputs1, outputs2), cbind(inputs2, inputs1))
    fdbksys <- selectsys(clpsys, (1:num_u1), (1:num_y1))
    return(fdbksys)
  }
}

