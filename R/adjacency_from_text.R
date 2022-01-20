#' Create adjacency matrices from 'graph6', 'sparse6', or 'digraph6' symbols
#' 
#' @param object character vector of 'graph6', 'sparse6', or 'digraph6' symbols
#' @param ... other arguments, currently ignored
#'   
#' @details If `object` contains 'sparse6' symbols, which are in fact encoded
#'   edgelists, the function will return corresponding adjacency matrices
#'   creating temporary igraph objects internally.
#' 
#' @return A list of adjacency matrices.
#' 
#' @examples 
#' # Graph6 symbols
#' sampleg6
#' adjacency_from_text(sampleg6)
#' 
#' # Sparse6 symbols
#' s6 <- c(":DgXI@G~", ":DgWCgCb")
#' adjacency_from_text(s6)
#' 
#' # Digraph6 symbol
#' d6 <- "&N????C??D?_G??C?????_?C_??????C??Q@O?G?"
#' adjacency_from_text(d6)
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
