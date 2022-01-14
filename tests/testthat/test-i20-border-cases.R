
g0d <- igraph::make_empty_graph(directed=TRUE)
g0u <- igraph::make_empty_graph(directed=FALSE)
g1u <- igraph::make_empty_graph(1, directed=FALSE)
g1d <- igraph::make_empty_graph(1, directed=TRUE)



# graph6 -----------------------------------------------------------------

test_that("graph of order 0 as graph6", {
  expect_silent(
    g6 <- as_graph6(g0u)
  )
  expect_identical(g6, rawToChar(as.raw(fN(0))))
})

test_that("graph6 of order 0 as adjacency matrix", {
  expect_silent(
    am <- adjacency_from_graph6(rawToChar(as.raw(fN(0))))[[1]]
  )
  expect_identical(am, matrix(0, 0, 0))
})

test_that("graph6 of order 0 as igraph", {
  requireNamespace("igraph")
  expect_true(igraph::identical_graphs(
    igraph_from_graph6(rawToChar(as.raw(fN(0))))[[1]],
    g0u
  ))
})

test_that("graph6 of order 0 as network", {
  requireNamespace("network")
  expect_identical(
    network_from_graph6(rawToChar(as.raw(fN(0))))[[1]],
    network::network.initialize(n = 0, directed = FALSE)
  )
})

test_that("graph of order 1 as graph6", {
  expect_silent(
    g6 <- as_graph6(g1u)
  )
  expect_identical(g6, rawToChar(as.raw(fN(1))))
})

test_that("graph6 of order 1 as adjacency matrix", {
  expect_silent(
    am <- adjacency_from_graph6(rawToChar(as.raw(fN(1))))[[1]]
  )
  expect_identical(am, matrix(0, 1, 1))
})

test_that("graph6 of order 1 as igraph", {
  requireNamespace("igraph")
  expect_true(igraph::identical_graphs(
    igraph_from_graph6(rawToChar(as.raw(fN(1))))[[1]],
    g1u
  ))
})

test_that("graph6 of order 1 as network", {
  requireNamespace("network")
  expect_identical(
    network_from_graph6(rawToChar(as.raw(fN(1))))[[1]],
    network::network.initialize(n = 1, directed = FALSE)
  )
})



# sparse6 -----------------------------------------------------------------

test_that("graph of order 0 as sparse6", {
  expect_silent(
    s6 <- as_sparse6(g0u)
  )
  string <- paste0(":", rawToChar(as.raw(fN(0))))
  expect_identical(s6, string)
})

test_that("sparse6 of order 0 as edgelist matrix", {
  string <- paste0(":", rawToChar(as.raw(fN(0))))
  expect_silent(
    elist <- edgelist_from_sparse6(string)[[1]]
  )
  expect_identical(
    elist, 
    structure(matrix(0, 0, 2), gorder = 0)
  )
})


test_that("sparse6 of order 0 as igraph", {
  requireNamespace("igraph")
  string <- paste0(":", rawToChar(as.raw(fN(0))))
  expect_true(igraph::identical_graphs(
    igraph_from_sparse6(string)[[1]],
    g0u
  ))
})

test_that("sparse6 of order 0 as network", {
  requireNamespace("network")
  string <- paste0(":", rawToChar(as.raw(fN(0))))
  expect_identical(
    network_from_sparse6(string)[[1]],
    network::network.initialize(n = 0, directed = FALSE)
  )
})



test_that("graph of order 1 as sparse6", {
  expect_silent(
    s6 <- as_sparse6(g1u)
  )
  string <- paste0(":", rawToChar(as.raw(fN(1))))
  expect_identical(s6, string)
})

test_that("sparse6 of order 1 as edgelist matrix", {
  string <- paste0(":", rawToChar(as.raw(fN(1))))
  expect_silent(
    elist <- edgelist_from_sparse6(string)
  )
  expect_identical(
    elist[[1]], 
    structure(matrix(0, 0, 2), gorder = 1)
  )
})

test_that("sparse6 of order 1 as igraph", {
  requireNamespace("igraph")
  string <- paste0(":", rawToChar(as.raw(fN(1))))
  expect_true(igraph::identical_graphs(
    igraph_from_sparse6(string)[[1]],
    g1u
  ))
})

test_that("sparse6 of order 1 as network", {
  requireNamespace("network")
  string <- paste0(":", rawToChar(as.raw(fN(1))))
  expect_identical(
    network_from_sparse6(string)[[1]],
    network::network.initialize(n = 1, directed = FALSE)
  )
})



# digraph6 ----------------------------------------------------------------


test_that("graph of order 0 as digraph6", {
  expect_silent(
    d6 <- as_digraph6(g0d)
  )
  string <- paste0("&", rawToChar(as.raw(fN(0))))
  expect_identical(d6, string)
})

test_that("digraph6 of order 0 as adjacency matrix", {
  string <- paste0("&", rawToChar(as.raw(fN(0))))
  expect_silent(
    A <- adjacency_from_digraph6(string)[[1]]
  )
  expect_identical(
    A, 
    matrix(0, 0, 0)
  )
})


test_that("digraph6 of order 0 as igraph", {
  requireNamespace("igraph")
  string <- paste0("&", rawToChar(as.raw(fN(0))))
  expect_true(igraph::identical_graphs(
    igraph_from_digraph6(string)[[1]],
    g0d
  ))
})

test_that("digraph6 of order 0 as network", {
  requireNamespace("network")
  string <- paste0("&", rawToChar(as.raw(fN(0))))
  expect_identical(
    network_from_digraph6(string)[[1]],
    network::network.initialize(n = 0, directed = TRUE)
  )
})



test_that("graph of order 1 as digraph6", {
  expect_silent(
    d6 <- as_digraph6(g1d)
  )
  #string <- paste0("&", rawToChar(as.raw(fN(1)))) yields '&@' which is wrong according to nauty
  string <- "&@?"
  expect_identical(d6, string)
})

test_that("digraph6 of order 1 as adjacency matrix", {
  string <- paste0("&", rawToChar(as.raw(fN(1))))
  expect_silent(
    A <- adjacency_from_digraph6(string)[[1]]
  )
  expect_identical(
    A, 
    matrix(0, 1, 1)
  )
})

test_that("digraph6 of order 1 as igraph", {
  requireNamespace("igraph")
  string <- paste0("&", rawToChar(as.raw(fN(1))))
  expect_true(igraph::identical_graphs(
    igraph_from_digraph6(string)[[1]],
    g1d
  ))
})

test_that("digraph6 of order 1 as network", {
  requireNamespace("network")
  string <- paste0("&", rawToChar(as.raw(fN(1))))
  expect_identical(
    network_from_digraph6(string)[[1]],
    network::network.initialize(n = 1, directed = TRUE)
  )
})
