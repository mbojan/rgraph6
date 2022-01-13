
test_that("errors are given for improper input",{
  expect_error( as_sparse6(matrix(1, 1, 1)), regexp="handles edgelists with 2 columns")
  expect_error( as_sparse6(matrix(1, 3, 3)), regexp="handles edgelists with 2 columns")
  # expect_error( as_sparse6(matrix(1, 0, 2)), regexp="handles edgelists with more than 1 row") function handles empty graphs now
  expect_error( as_sparse6(matrix(1, 0, 2))) # added because as_sparse6.matrix needs n as input
  expect_error( as_sparse6(1), regexp="handle class")
})



test_that("works for simple matrices", {
  m <- matrix(c(1,2,1,3), 2, 2,byrow = T) 
  expect_silent(as_sparse6(m, 3))
})



test_that("works for ':Fa@x^' from format documentation", {
  s6 <- ":Fa@x^"
  elist <- matrix(c(1,2, 1,3, 2,3, 6,7), ncol=2, byrow=TRUE)
  expect_equivalent(
    edgelist_from_sparse6(s6)[[1]],
    elist
  )
  
  expect_true(igraph::identical_graphs(
    igraph_from_sparse6(s6)[[1]],
    igraph::graph_from_edgelist(elist, directed=FALSE)  
  ))
})
