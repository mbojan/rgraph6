#' Create network objects from 'graph6', 'sparse6', or 'digraph6' symbols
#' 
#' @param object character vector of 'graph6', 'sparse6', or 'digraph6' symbols
#' 
#' @return A list of 'network' objects.
#' @examples 
#' # complete graph in graph6 format
#' g6 <- "G~~~~{"
#' 
#' # random graph with 15 nodes
#' s6 <- ":NeF?bsl?aNC"
#' 
#' # random directed graph with 10 nodes
#' d6 <- "&I???GGGI?_gG??O???"
#' 
#' network_from_text(g6)
#' network_from_text(c(g6,s6,d6))
#' 
#' @export
network_from_text <- function(object) {
  stopifnot(is.character(object))
  fmt <- guess_format(object)
  rval <- vector(mode = "list", length = length(object))
  rval[fmt == "sparse6"] <- network_from_sparse6(object[fmt == "sparse6"])
  rval[fmt == "graph6"] <- network_from_graph6(object[fmt == "graph6"])
  rval[fmt == "digraph6"] <- network_from_digraph6(object[fmt == "digraph6"])
  rval
}
