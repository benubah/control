#Example from Home page of Julia Control - an electric motor example
#J <- 2.0
#b <- 0.04
#K <- 1.0
#R <- 0.08
#L <- 1e-4
#P <- TF("K/(s*((J*s + b)*(L*s + R) + K^2))")
# Cls <- TF("5*P/(1 + 5*P)")

#TF("s+1")
#sys1 <- tf(1, c(1, 2, 5))
#sys2 <- tf(2, c(1, 2, 5))
#TF("sys1 + sys2")
#TF("sys1 - sys2")
#TF("sys1 - 1")
#TF("sys1 + 1")
#TF("sys1 - sys2 + sys2")
#TF("sys1 * sys2 / sys2")
#TF("sys1 / sys2 / sys2")

#' @export
TF <- function(...){
  s <- c(1,0)
  z <- c(1,0)
  args1 <- list(...)

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
      res <- pracma::polyadd(c(args[[1]]), -c(args[[2]]))
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
      class(res) <- 'tf'
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
    class(res) <- 'tf'
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
