#' @title Print Polynomial
#'
#' @description
#' Print polynomial as a character string.
#'
#' @details
#' Modified from package *pracma*.
#' Modification: To hide any coefficient and power that is equal to 1
#' So that instead of '1s^3' we have 's^3' and instead of 's^1', we have 's'
#'
#' @param p	numeric vector representing a polynomial
#' @param svar	character representing the unknown, default x.
#' @param smul	multiplication symbol, default *.
#' @param d	 significant digits, default options("digits").
#'
#' @return Returns the usual string representing a polynomial in mathematics.
#'
#' @examples
#' poly2str(c(2, -3, 1, 20, -11))
#'
#' @export
poly2str <- function(p, svar = "x", smul = "*",
                     d = options("digits")$digits) {
  if (length(p) == 0) return("")
  if (!is.numeric(p))
    stop("Argument 'p' must be a numeric vector.")

  while (p[1] == 0 && length(p) > 1)
    p <- p[2:length(p)]
  if (length(p) == 1) return(as.character(p))

  s <- sign(p)
  p <- abs(p)

  p <- formatC(p, digits = d)
  p <- sub("^\\s+", "", p)

  n <- length(p) - 1
  S <- ""
  #print(p)
  s1 <- if (s[1] == 1) "" else "-"

  #added to clean out any first coefficient that is 1. So that instead of 1s^1 we have s^1
  if(p[1] == "1" && n == 1){
    S <- paste(s1, "", smul, svar, "", sep = "")
  } else if(p[1] == "1"){
    S <- paste(s1, "", smul, svar, "^", n, sep = "")
  } else {
    S <- paste(s1, p[1], smul, svar, "^", n, sep = "")
  }



  for (i in 2:(n+1)) {
    if (s[i] == 1) s1 <- " + "
    else if (s[i] == -1) s1 <- " - "
    else next

    if (n-i+1 > 1) {
      #added to clean out any coefficients for s in orders greater than 1
      if(p[i] == "1") {
        S <- paste(S, s1, "", smul, svar, "^", n-i+1, sep="")
      } else {
        S <- paste(S, s1, p[i], smul, svar, "^", n-i+1, sep="")
      }
    } else if (i == n) {
      #added to clean out any coefficients for s in order 1
      if(p[i] == "1") {
        S <- paste(S, s1, "", smul, svar, sep="")
      } else {
        S <- paste(S, s1, p[i], smul, svar, sep="")
      }
    } else {
      S <- paste(S, s1, p[i], sep="")
    }
  }
  return(S)
}
