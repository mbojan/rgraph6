
test_that("errors are given for improper input",{
  expect_error( as_sparse6(matrix(1, 1, 1)), regexp="handles edgelists with 2 columns")
  expect_error( as_sparse6(matrix(1, 3, 3)), regexp="handles edgelists with 2 columns")
  expect_error( as_sparse6(matrix(1, 0, 2)), regexp="handles edgelists with more than 1 row")
  expect_error( as_sparse6(1), regexp="handle class")
})



test_that("works for simple matrices", {
  m <- matrix(c(1,2,1,3), 2, 2,byrow = T) 
  expect_silent(as_sparse6(m))
})
