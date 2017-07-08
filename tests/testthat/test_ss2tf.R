
# SIMO Test
A <- rbind(c(0,1), c(-10000,-4))
B <- rbind(0,1)
C <- rbind(c(1,0), c(0,1))
D <- rbind(0,0)

sys2 <- ss2tf(A, B, C, D)

num <- matrix(c(0,0,1,0,1,0), byrow = TRUE, nrow = 2)
den <- matrix(c(1, 4, 10000), nrow = 1)
expectedSys2 <- tf(num, den)

context("SS2TF:  SS conversion to TF model")
test_that("State-space is converted to Transfer Function", {
  expect_equivalent(sys2$num, expectedSys2$num)
  expect_equivalent(sys2$den, expectedSys2$den)

})


context("SS2TF: Output model Check")
test_that("System is a tf object", {
  expect_is(sys2, 'tf')
})
