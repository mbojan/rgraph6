#' Create network objects from graph6, sparse6, or dgraph6 symbols
#' 
#' @param object character vector with a mixture of graph6, sparse6 or dgraph6
#'   symbols
#' 
#' @return A list of [network](network::network()) objects.
#' 
#' @export
network_from_text <- function(object) {
  stopifnot(is.character(object))
  fmt <- guess_format(object)
  rval <- vector(mode = "list", length = length(object))
  rval[fmt == "sparse6"] <- network_from_sparse6(object[fmt == "sparse6"])
  rval[fmt == "graph6"] <- network_from_graph6(object[fmt == "graph6"])
  rval[fmt == "dgraph6"] <- network_from_dgraph6(object[fmt == "dgraph6"])
  rval
}
