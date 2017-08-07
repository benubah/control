#' @title Evaluate Transfer function Expressions
#'
#' @description
#' \code{TF} Evaluates a given transfer function expression in the s-domain
#'
#' @details
#' \code{TF} Evaluates a given transfer function polynomial expression in the s-domain.
#' The evaluation of the expressions are performed similar to symbolic math computations for polynomials.
#' A transfer function model is created as the result of the expression evaluation.
#' Thus, this is an alternative way of creating transfer function models following the natural math expressions
#' found in block diagrams. It also provides an alternative way to perform system interconnections. Only transfer
#' function models are currently supported for system interconnection using this function. System interconnections
#' for other models could be performed using the \code{series}, \code{parallel}, \code{feedback} or \code{connect} functions.
#' See the Examples section for further details.
#'
#'
#' @param str_expr  String expression containing the transfer function
#'
#' @return Returns an object of 'tf' class list with a transfer function. Numerator and denominator
#' coefficients could then be retrieved from the object the same way as any other \code{tf} object
#'
#' @seealso \code{\link{tf}} \code{\link{tf2ss}} \code{\link{series}} \code{\link{parallel}}
#'
#' @examples
#'
#' # Example taken from the GitHub page of Julia Control - an electric motor example
#' J <- 2.0
#' b <- 0.04
#' K <- 1.0
#' R <- 0.08
#' L <- 1e-4
#' P <- TF("K/(s*((J*s + b)*(L*s + R) + K^2))")
#' Cls <- TF("P/(1 + P)") # closed-loop connection
#'
#' # More examples
#' TF("s+1")
#' sys1 <- tf(1, c(1, 2, 5))
#' sys2 <- tf(2, c(1, 2, 5))
#' TF("sys1 + sys2") # parallel system interconnection
#' TF("sys1 * sys2") # series system interconnection
#' TF("sys1 - sys2")
#' TF("sys1 - 1")
#' TF("sys1 + 1")
#' TF("sys1 - sys2 + sys2")
#' TF("sys1 / sys2 / sys2")
#'
#' @rdname TFunction
#' @export
TF <- function (str_expr) {
  s <- c(1,0)
  z <- c(1,0)
  args1 <- str_expr

  `+` <- function(...) {
    args <- list(...)

    if (is.list(args[[1]]) && !is.list(args[[2]])) {
      # add code to check the class and use tfdata to make it a tf
      # consider adding code to also do these computations in ss
      # and return an ss object by creating an SS class like TF separately
      if (class(args[[1]]) == 'tf') {
        num1 <- args[[1]]$num
        den1 <- args[[1]]$den
        num2 <- args[[2]]
        den2 <- 1
        p1 <- pracma::polymul(c(num1),c(den2))
        p2 <- pracma::polymul(c(num2),c(den1))
        pnum <- pracma::polyadd(p1,p2)
        pden <- pracma::polymul(c(den1),c(den2))
        res <- list(num = matrix(pnum,nrow=1), den = matrix(pden,nrow=1))
        class(res) <- 'tf'
        return(res)
      } else {
        stop("Transfer Function: sys should be of class tf")
      }

    } else  if (!is.list(args[[1]]) && is.list(args[[2]])) {
      if (class(args[[2]]) == 'tf') {
        num1 <- args[[1]]
        den1 <- 1
        num2 <- args[[2]]$num
        den2 <- args[[2]]$den
        p1 <- pracma::polymul(c(num1),c(den2))
        p2 <- pracma::polymul(c(num2),c(den1))
        pnum <- pracma::polyadd(p1,p2)
        pden <- pracma::polymul(c(den1),c(den2))
        res <- list(num = matrix(pnum,nrow=1), den = matrix(pden,nrow=1))
        class(res) <- 'tf'
        return(res)
      } else {
        stop("Transfer Function: sys should be of class tf")
      }
    } else  if (is.list(args[[1]]) && is.list(args[[2]])) {
      if ((class(args[[1]]) == 'tf') &&  (class(args[[2]]) == 'tf')) {
        num1 <- args[[1]]$num
        den1 <- args[[1]]$den
        num2 <- args[[2]]$num
        den2 <- args[[2]]$den
        p1 <- pracma::polymul(c(num1),c(den2))
        p2 <- pracma::polymul(c(num2),c(den1))
        pnum <- pracma::polyadd(p1,p2)
        pden <- pracma::polymul(c(den1),c(den2))
        res <- list(num = matrix(pnum,nrow=1), den = matrix(pden,nrow=1))
        class(res) <- 'tf'
        return(res)
      } else {
        stop("Transfer Function: sys should be of class tf")
      }
    } else {
      res <- pracma::polyadd(c(args[[1]]),c(args[[2]]))
      #class(res) <- 'tf'
      return(res)
    }
  }


  `-` <- function(...) {
    args <- list(...)

    if (is.list(args[[1]]) && !is.list(args[[2]])) {

      if (class(args[[1]]) == 'tf') {
        num1 <- args[[1]]$num
        den1 <- args[[1]]$den
        num2 <- args[[2]]
        den2 <- 1
        p1 <- pracma::polymul(c(num1),c(den2))
        p2 <- pracma::polymul(c(num2),c(den1))
        pnum <- polysub(p1,p2)
        pden <- pracma::polymul(c(den1),c(den2))
        res <- list(num = matrix(pnum,nrow=1), den = matrix(pden,nrow=1))
        class(res) <- 'tf'
        return(res)
      } else {
        stop("Transfer Function: sys should be of class tf")
      }

    } else  if (!is.list(args[[1]]) && is.list(args[[2]])) {
      if (class(args[[2]]) == 'tf') {
        num1 <- args[[1]]
        den1 <- 1
        num2 <- args[[2]]$num
        den2 <- args[[2]]$den
        p1 <- pracma::polymul(c(num1),c(den2))
        p2 <- pracma::polymul(c(num2),c(den1))
        pnum <- polysub(p1, p2)
        pden <- pracma::polymul(c(den1),c(den2))
        res <- list(num = matrix(pnum,nrow=1), den = matrix(pden,nrow=1))
        class(res) <- 'tf'
        return(res)
      } else {
        stop("Transfer Function: sys should be of class tf")
      }
    } else  if (is.list(args[[1]]) && is.list(args[[2]])) {
      if ((class(args[[1]]) == 'tf') &&  (class(args[[2]]) == 'tf')) {
        num1 <- args[[1]]$num
        den1 <- args[[1]]$den
        num2 <- args[[2]]$num
        den2 <- args[[2]]$den
        p1 <- pracma::polymul(c(num1),c(den2))
        p2 <- pracma::polymul(c(num2),c(den1))
        pnum <- polysub(p1, p2)
        pden <- pracma::polymul(c(den1),c(den2))
        res <- list(num = matrix(pnum,nrow=1), den = matrix(pden,nrow=1))
        class(res) <- 'tf'
        return(res)
      } else {
        stop("Transfer Function: sys should be of class tf")
      }
    } else {
      res <- polysub(c(args[[1]]), c(args[[2]]))
      return(res)
    }
  }


 `*` <- function(...) {
    args <- list(...)

    if (is.list(args[[1]]) && !is.list(args[[2]])) {
      if(class(args[[1]]) == 'tf'){
        num1 <- args[[1]]$num
        den1 <- args[[1]]$den
        num2 <- args[[2]]
        den2 <- 1
        pnum <- pracma::polymul(c(num1),c(num2))
        pden <- pracma::polymul(c(den1),c(den2))
        res <- list(num = matrix(pnum,nrow=1), den = matrix(pden,nrow=1))
        class(res) <- 'tf'
        return(res)
      } else {
        stop("Transfer Function: sys should be of class tf")
      }

    } else  if (!is.list(args[[1]]) && is.list(args[[2]])) {
      if(class(args[[2]]) == 'tf'){
        num1 <- args[[1]]
        den1 <- 1
        num2 <- args[[2]]$num
        den2 <- args[[2]]$den
        pnum <- pracma::polymul(c(num1),c(num2))
        pden <- pracma::polymul(c(den1),c(den2))
        res <- list(num = matrix(pnum,nrow=1), den = matrix(pden,nrow=1))
        class(res) <- 'tf'
        return(res)
      } else {
        stop("Transfer Function: sys should be of class tf")
      }
    } else if (is.list(args[[1]]) && is.list(args[[2]])) {
      if((class(args[[1]]) == 'tf') &&  (class(args[[2]]) == 'tf')){
        num1 <- args[[1]]$num
        den1 <- args[[1]]$den
        num2 <- args[[2]]$num
        den2 <- args[[2]]$den
        pnum <- pracma::polymul(c(num1),c(num2))
        pden <- pracma::polymul(c(den1),c(den2))
        res <- list(num = matrix(pnum,nrow=1), den = matrix(pden,nrow=1))
        class(res) <- 'tf'
        return(res)
      } else {
        stop("Transfer Function: sys should be of class tf")
      }
    } else {
      res <- pracma::polymul(args[[1]],args[[2]])
      #class(res) <- 'tf'
      return(res)
    }
  }


  `/` <- function(...) {
    args <- list(...)

    if (is.list(args[[1]]) && !is.list(args[[2]])) {
      if (class(args[[1]]) == 'tf') {
        num1 <- args[[1]]$num
        den1 <- args[[1]]$den
        num2 <- args[[2]]
        den2 <- 1
        pnum <- pracma::polymul(c(num1),c(den2))
        pden <- pracma::polymul(c(den1),c(num2))
        res <- list(num = matrix(pnum,nrow=1), den = matrix(pden,nrow=1))
        class(res) <- 'tf'
        return(res)
      } else {
        stop("Transfer Function  : sys should be of class tf")
      }

    } else  if (!is.list(args[[1]]) && is.list(args[[2]])) {
      if (class(args[[2]]) == 'tf') {
        num1 <- args[[1]]
        den1 <- 1
        num2 <- args[[2]]$num
        den2 <- args[[2]]$den
        pnum <- pracma::polymul(c(num1),c(den2))
        pden <- pracma::polymul(c(den1),c(num2))
        res <- list(num = matrix(pnum,nrow=1), den = matrix(pden,nrow=1))
        class(res) <- 'tf'
        return(res)
      } else {
        stop("Transfer Function  : sys should be of class tf")
      }
    } else  if (is.list(args[[1]]) && is.list(args[[2]])) {
      if ((class(args[[1]]) == 'tf') &&  (class(args[[2]]) == 'tf')) {
        num1 <- args[[1]]$num
        den1 <- args[[1]]$den
        num2 <- args[[2]]$num
        den2 <- args[[2]]$den
        pnum <- pracma::polymul(c(num1),c(den2))
        pden <- pracma::polymul(c(den1),c(num2))
        res <- list(num = matrix(pnum,nrow=1), den = matrix(pden,nrow=1))
        class(res) <- 'tf'
        return(res)
      } else {
        stop("Transfer Function  : sys should be of class tf")
      }
    } else {
      argnum <- args[[1]]
      argden <- args[[2]]
      res <- list(num = matrix(argnum, nrow=1), den = matrix(argden, nrow=1))
      class(res) <- 'tf'
      return(res)
    }
  }

`^` <- function(...) {
    args <- list(...)
    res <- pracma::polypow(args[[1]],args[[2]])
    #class(res) <- 'tf'
    return(res)
  }
  if (length(args1) > 0) {
    argeval <- eval(parse(text = args1))
    if (is.vector(argeval)) {
       argeval <- list(num = matrix(argeval, nrow = 1), den = matrix(1, nrow = 1))
    }
    class(argeval) <- 'tf'
    return(argeval)
  }
}
#' @export
polysub <- function (a, b) {
  return(pracma::polyadd(a, -b))
}
