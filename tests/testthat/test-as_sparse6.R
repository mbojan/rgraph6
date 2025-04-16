
test_that("errors are given for improper input",{
  # Matrix does not have two columns
  expect_error( as_sparse6(matrix(1, 1, 1)), regexp="handles edgelists with 2 columns")
  
  # Matrix has more than two columns
  expect_error( as_sparse6(matrix(1, 3, 3)), regexp="handles edgelists with 2 columns")
  
  # Bad class
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



test_that("works on lists of edgelists representing graphs of varying size (#27)", {
  l <- list(
    matrix(c(1,2, 2,3, 3,4), ncol=2, byrow=TRUE),
    matrix(c(1,2, 2,3), ncol=2, byrow=TRUE)
  )
  expect_silent(
    r <- as_sparse6(l)
  )
  s6 <- c(":Cdv", ":Bd")
  expect_identical(r, s6)
  expect_equivalent(edgelist_from_sparse6(s6), l)
})




test_that("behaves correctly for edgelists with different maximums (#28)", {
  requireNamespace("igraph")
  
  # 1
  g <- igraph::make_graph(~ A, B, C, D, E -- F)
  elm <- igraph::as_edgelist(g, names=FALSE) # [5, 6]
  expect_identical(
    as_sparse6(g),
    as_sparse6(elm)
  )
  
  # 2
  g <- igraph::make_graph(~ A -- B, C, D, E, F)
  elm <- igraph::as_edgelist(g, names=FALSE) # [1, 2]
  expect_identical(
    as_sparse6(g),
    as_sparse6(elm, n = 6)
  )
  expect_identical(as_sparse6(elm), ":An")
  
  # 3
  g <- igraph::make_graph(~ A, B -- C, D, E, F)
  elm <- igraph::as_edgelist(g, names=FALSE) # [2, 3]
  expect_identical(
    as_sparse6(g),
    as_sparse6(elm, n = 6)
  )
  expect_identical(as_sparse6(elm), ":Bp")
  
  #4
  expect_equivalent(as_sparse6(igraph::make_full_graph(2)),
                   as_sparse6(edgelist_from_sparse6(":An")))
  
  
})
