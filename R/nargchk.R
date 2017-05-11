

#----------------------------------------------------------------------
#
# nargchk
#
# Check number of input arguments. Return error message if
# not between min and max.  If it is, return empty matrix.
#
#----------------------------------------------------------------------

nargchk <- function(min,max,number){
  msg <- "";
  if (number < min) {
    msg = "Not enough input arguments.";
  }else if (number > max) {
    msg = "Too many input arguments.";
  }
  return(msg);
}
