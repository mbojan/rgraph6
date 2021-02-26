test_that("this bug is fixed", {
  set.seed(666)
  g <- igraph::sample_gnp(10, 0.1, directed=FALSE) %>%
    igraph::delete_graph_attr("name") %>%
    igraph::delete_graph_attr("type") %>%
    igraph::delete_graph_attr("loops") %>%
    igraph::delete_graph_attr("p")

  s6 <- as_sparse6(g)
  edgelist_from_sparse6(s6)
  expect_true(
    igraph::identical_graphs(g, igraph_from_text(s6)[[1]])  
  )
  
  g6 <- as_graph6(g)
  expect_true(
    igraph::identical_graphs(g, igraph_from_text(g6)[[1]])  
  )
})
