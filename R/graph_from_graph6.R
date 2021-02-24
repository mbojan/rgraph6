#' Coonvert graph6 symbols to igraph objects
#' 
#' @param object vector of graph6 symbols
#' @param ... other arguments passed to [igraph::graph_from_adjacency_matrix()]
#' 
#' @return A list of igraph objects.
#' 
#' @export
as_igraph <- function(object, ...) {
  stopifnot(inherits(object, "graph6"))
  requireNamespace("igraph", quietly=TRUE)
  amlist <- as_adjacency(object)
  lapply(amlist, igraph::graph_from_adjacency_matrix, mode="undirected", ...)
}
