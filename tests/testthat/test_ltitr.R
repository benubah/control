# TESTS
A <- diag(1,2)
B <- rbind(1,1)
x0 <- rbind(-1, -2)
u <- cbind(1,2,3,4,5)
X1 <- ltitr(A,B,u)
X2 <- ltitr(A,B,u,x0)

expected1 <- rbind(c(0, 1,  3, 6, 10), c(0, 1,  3, 6, 10))
expected2 <- rbind(c(-1, 0,  2, 5, 9), c(-2, -1,  1, 4, 8))

context("LTITR: Single-Input test")
test_that("LTI SISO system response matches expectation", {
  expect_equal(X1, expected1)
  expect_equal(X2, expected2)
  expect_equal(nrow(u), ncol(B))
})

A2 <- rbind(c(0,1), c(-25,-4))
B2 <- rbind(c(1,1), c(0,1))
X3 <- ltitr(A2, B2, rbind(u,u))

expected3 <- rbind(c(0, 2,  5, -46, 94), c(0, 1,  -52, 86, 810))

context("LTITR: Double-Input test")
test_that("LTI MIMO system response matches expectation", {
  expect_equal(X3, expected3)
  expect_equal(nrow(u), ncol(B))
})
