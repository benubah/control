

# SS Test

sys1 <- ss(1, 2, 3, 4)

context("SS: Model Creation Test")
test_that("System is a ss object", {
  expect_is(sys1, 'ss')
})

context("SS: Model Data Test")
test_that("SS values are matrices", {
  expect_true(is.matrix(sys1$A))
  expect_true(is.matrix(sys1$B))
  expect_true(is.matrix(sys1$C))
  expect_true(is.matrix(sys1$D))
})
