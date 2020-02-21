#' Convert graph6 symbols to network objects
#' 
#' @param object vector of graph6 symbols
#' @param ... other arguments passed to [network::as.network]
#' 
#' @return A list of network objects.
#' 
#' @export
as_network <- function(object, ...) {
  stopifnot(inherits(object, "graph6"))
  requireNamespace("network", quietly=TRUE)
  amlist <- as_adjacency(object)
  lapply(amlist, network::as.network, directed=FALSE, ...)
}
