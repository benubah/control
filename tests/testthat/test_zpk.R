

# ZPK Test

sys1 <- zpk(1, c(1,-1), 1)

context("ZPK: Model Creation Test")
test_that("System is a zpk object", {
  expect_is(sys1, 'zpk')
})

context("ZPK: Model Data Test")
test_that("ZPK values are matrices", {
  expect_true(is.matrix(sys1$z))
  expect_true(is.matrix(sys1$p))
  expect_true(is.matrix(sys1$k))
})
