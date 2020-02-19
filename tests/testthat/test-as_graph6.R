context("Testing as_graph6")



test_that("errors are given for improper input",{
  expect_error( as_graph6(matrix(1, 1, 1)), regexp="handles networks of sizes 2-62 only")
  expect_error( as_graph6(matrix(1, 63, 63)), regexp="handles networks of sizes 2-62 only")
  expect_error( as_graph6(1), regexp="handle class")
})



test_that("works for simple matrices", {
  m <- matrix(c(0,1,1,0), 2, 2) # complete size 2
  expect_silent(as_graph6(m))
})