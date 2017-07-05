# TESTS

a <- matrix(c(-3, 2,1, 1), byrow = TRUE, ncol = 2)
b <- matrix(c(0, 1), nrow = 2)
c <- matrix(c(1, -1), ncol=2)
q <- t(c)%*%c
r <- 3
res <- care(a,b,q,r)

X <- rbind( c(0.5895174, 1.821575), c(1.8215747, 8.818840))
L <- rbind(-3.502629, -1.436984)
G <- cbind(0.6071916, 2.939613)

context("CARE: (A, B, Q, R) test")
test_that("Alebraic Ricatti Equation Function matches expectation", {
  expect_equal(res$X, X, tolerance = 1e-05)
  expect_equal(res$L, L, tolerance = 1e-05)
  expect_equal(res$G, G, tolerance = 1e-05)
})

X2 <- rbind( c(0.2949113, 0.5198625), c(0.5198625, 3.0197648))
L2 <- rbind(-3.605551,  -1.414214)
G2 <- cbind(0.5198625, 3.019765)
res2 <- care(a,b,q)

context("CARE: (A, B, Q) test")
test_that("Alebraic Ricatti Equation Function matches expectation", {
  expect_equal(res2$X, X2, tolerance = 1e-05)
  expect_equal(res2$L, L2, tolerance = 1e-05)
  expect_equal(res2$G, G2, tolerance = 1e-05)
})
