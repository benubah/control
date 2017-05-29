# tfchk.R
# Syntax: tf1=tfchk(num,den)
#
# num is the numerator and den is the denominator.
# If the transfer function is not proper given by (num,den) then
# it returns lenth(numc) = length(denc).
# The returned list for a=tfchk(num,den) is:
#       tf1$numc
#       tf1$denc


tfchk <- function(num,den){
  if(!is.matrix(num)){
    num <- matrix(num,nrow=1)
  }
  if(!is.matrix(den)){
    den <- matrix(den,nrow=1)
  }
  if ( is.null(num) ) {
    print("TFCHK: Warning: Transfer function numerator should not be empty.\n");
  }
  if ( is.null(den) ) {
    print("TFCHK: Warning: Transfer function denominator should not be empty.\n");
  }
  if ( !( (nrow(den) == 1) || (ncol(den) == 1)) ) {
    stop("TFCHK: Denominator must be a row vector.");
  }
  if ( (nrow(den) != 1) && (ncol(den) == 1)) {
    stop("TFCHK: Denominator must be a row vector.");
  }
  if (ncol(num) > ncol(den)) {
    print("TFCHK: Transfer function may not be proper.");
  }
  if(ncol(num) <= ncol(den)){
    numc <- cbind(matrix(0,nrow(num), ncol(den)-ncol(num)),num);
  } else{
    numc <- matrix(num, nrow = 1)
  }
  denc <- den;
  return(list(numc=numc,denc=denc));
}

