#' Create edgelist matrices from graph6, sparse6, or dgraph6 symbols
#' 
#' @param object character vector with a mixture of graph6, sparse6 or dgraph6
#'   symbols
#'   
#' @details If `object` contains graph6 or dgraph6 symbols, which are in fact
#'   encoded adjacency matrices, the function will return corresponding edgelist
#'   matrices creating temporary igraph objects internally.
#' 
#' @return A list of adjacency matrices.
#' 
#' @export
edgelist_from_text <- function(object, ...) {
  stopifnot(is.character(object))
  fmt <- guess_format(object)
  rval <- vector(mode = "list", length = length(object))
  if("graph6" %in% fmt) {
    requireNamespace("igraph")
    iglist <- igraph_from_graph6(object[fmt == "graph6"])
    rval[fmt == "graph6"] <- lapply(iglist, igraph::as_edgelist)
  }
  if("dgraph6" %in% fmt) {
    requireNamespace("igraph")
    iglist <- igraph_from_dgraph6(object[fmt == "dgraph6"])
    rval[fmt == "dgraph6"] <- lapply(iglist, igraph::as_edgelist)
  }
  rval[fmt == "sparse6"] <- edgelist_from_sparse6(object[fmt == "sparse6"])
  rval
}
