
# @export
polyvalm <- function(p, x) {
  if (length(x) == 0) return(c())
  if (length(p) == 0) return(0 * x)
  if (!is.vector(p, mode="numeric") && !is.vector(p, mode = "complex"))
    stop("Argument 'p' must be a real or complex vector.")
  if (!is.vector(x) && !is.matrix(x))
    stop("Argument 'x' must be a real or complex matrix.")

  n <- length(p)
  if (n == 0){
    y <- pracma::zeros(nrow(x))
  } else {
    id <- diag(1, nrow(x))
    y <- p[1] * id

    for (i in 2:n){
      y <- y %*% x + p[i] * id
    }
  }
  return(y)
}
