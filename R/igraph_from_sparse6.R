#' Convert sparse6 symbols to igraph objects
#' 
#' @param object character vector of sparse6 symbols
#' @param ... other arguments passed to [igraph::graph_from_edgelist()]
#' 
#' @return A list of igraph objects.
#' 
#' @export
igraph_from_sparse6 <- function(object, ...) {
  requireNamespace("igraph", quietly=TRUE)
  ellist <- as_elist(object)
  lapply(ellist, igraph::graph_from_edgelist, directed=FALSE, ...)
}
