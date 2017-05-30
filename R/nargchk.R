# nargchk.R
# This function examines the number of input arguments and returns an error message if
# the number is not between min and max, or else it returns an empty matrix.
#' @export

nargchk <- function(min,max,number){
  errmsg <- "";
    if (number < min) {
       errmsg <- "Input arguments are less than required.";
    } else if (number > max) {
       errmsg <- "Input arguments are more than required.";
    }
  return(errmsg);
}
