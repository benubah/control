
sys1 <- tf2zp(c(1,2), c(1,2,1))
expectedSys <- zpk(-2, c(-1,-1), 1)

context("TF2ZP:  TF conversion to ZPK model")
test_that("Transfer function is converted to zero-pole", {
  expect_equal(sys1$z, expectedSys$z)
  expect_equal(sys1$p, expectedSys$p)
  expect_equal(sys1$k, expectedSys$k)
})


context("TF2ZP: Output model Check")
test_that("System is a zpk object", {
  expect_is(sys1, 'zpk')
})
