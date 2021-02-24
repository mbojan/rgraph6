#' Convert graph6 symbols to igraph objects
#' 
#' @param object vector of graph6 symbols
#' @param ... other arguments passed to [igraph::graph_from_adjacency_matrix()]
#' 
#' @return A list of igraph objects.
#' 
#' @export
igraph_from_graph6 <- function(object, ...) {
  requireNamespace("igraph", quietly=TRUE)
  amlist <- as_adjacency(object)
  lapply(amlist, igraph::graph_from_adjacency_matrix, mode="undirected", ...)
}
