context("Testing as_graph6")



test_that("errors are given for improper input",{
  expect_error( as_graph6(matrix(1, 1, 1)), regexp="handles networks of sizes 2-62 only")
  expect_error( as_graph6(matrix(1, 63, 63)), regexp="handles networks of sizes 2-62 only")
  expect_error( as_graph6(1), regexp="handle class")
})