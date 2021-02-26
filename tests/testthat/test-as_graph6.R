
test_that("errors are given for improper input",{
  expect_error( as_graph6(matrix(1, 1, 1)), regexp="handles networks of size greater 1")
  expect_error( as_graph6(1), regexp="handle class")
})



test_that("works for simple matrices", {
  m <- matrix(c(0,1,1,0), 2, 2) # complete size 2
  expect_silent(as_graph6(m))
})





test_that("annoying #9 'corrupted size vs prev_size' crash", {
  m <- structure(c(0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1,  1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1,  1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1,  0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1,  1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1,  1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 1,  0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1,  0), .Dim = c(12L, 12L))
  expect_silent(
    as_graph6(m)
  )
})
