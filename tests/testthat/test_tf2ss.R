
sys1 <- tf2ss(1, c(1,2,1))
expectedSys <- ss(c(-2,-1,1,0), c(1,0), c(0,1), 0)

context("TF2SS:  TF conversion to SS model")
test_that("Transfer function is converted to state-space", {
   expect_equivalent(sys1$A, expectedSys$A)
   expect_equivalent(sys1$B, expectedSys$B)
   expect_equivalent(sys1$C, expectedSys$C)
   expect_equivalent(sys1$D, expectedSys$D)
})


context("TF2SS: Output model Check")
test_that("System is a ss object", {
   expect_is(sys1, 'ss')
})
