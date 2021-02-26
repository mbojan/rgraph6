#' Create adjacency matrices from graph6, sparse6, or digraph6 symbols
#' 
#' @param object character vector with a mixture of graph6, sparse6 or digraph6
#'   symbols
#'   
#' @details If `object` contains sparse6 symbols, which are in fact encoded
#'   edgelists, the function will return corresponding adjacency matrices
#'   creating temporary igraph objects internally.
#' 
#' @return A list of adjacency matrices.
#' 
#' @export
adjacency_from_text <- function(object, ...) {
  stopifnot(is.character(object))
  fmt <- guess_format(object)
  rval <- vector(mode = "list", length = length(object))
  if("sparse6" %in% fmt) {
    requireNamespace("igraph")
    iglist <- igraph_from_sparse6(object[fmt == "sparse6"])
    rval[fmt == "sparse6"] <- lapply(iglist, igraph::as_adjacency_matrix, sparse = FALSE)
  }
  rval[fmt == "graph6"] <- adjacency_from_graph6(object[fmt == "graph6"])
  rval[fmt == "digraph6"] <- adjacency_from_digraph6(object[fmt == "digraph6"])
  rval
}
