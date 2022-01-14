
test_that("errors are given for improper input",{
  # expect_error( as_digraph6(matrix(1, 1, 1)), regexp="handles networks of size greater 1") function handles this now
  expect_error( as_digraph6(1), regexp="handle class")
})



test_that("works for simple matrices", {
  m <- matrix(c(0,1,0,0), 2, 2) # one edge size 2
  expect_silent(as_digraph6(m))
})
