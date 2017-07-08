

# TF Test

sys1 <- tf(c(1,3), c(1,2,5))

context("TF: Model Creation Test")
test_that("System is a tf object", {
  expect_is(sys1, 'tf')
})

context("TF: Model Data Test")
test_that("TF values are matrices", {
  expect_true(is.matrix(sys1$num))
  expect_true(is.matrix(sys1$den))
})
