
# ZPK Test
 A <- rbind(c(0,1), c(-10000,-4))
B <- rbind(0,1)
C <- rbind(c(1,0), c(0,1))
D <- rbind(0,0)
#A <- rbind(c(-2, -1), c(1,0)); B <- rbind(1,0);C <- cbind(0,1); D <- 0;
sys2 <- ss2zp(A, B, C, D)

z <- matrix(c(NA, 0),  nrow = 1)
p <- matrix(c(-2+99.98i, -2-99.98i), nrow = 2)
k <- matrix(c(1, 1), nrow = 2)
expectedSys2 <- zpk(z, p, k)

context("SS2ZP:  SS conversion to ZPK model")
test_that("State-space is converted to Zero-Pole-Gain", {
  expect_equal(sys2$z, expectedSys2$z, tolerance = 1e-05)
  expect_equal(sys2$p, expectedSys2$p, tolerance = 1e-05)
  expect_equal(sys2$k, expectedSys2$k, tolerance = 1e-05)

})


context("SS2ZP: Output model Check")
test_that("System is a zpk object", {
  expect_is(sys2, 'zpk')
})
