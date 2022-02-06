#' Create edgelist matrices from 'graph6', 'sparse6', or 'digraph6' symbols
#' 
#' @param object character vector of 'graph6', 'sparse6', or 'digraph6' symbols
#' @param ... other arguments, currently ignored
#'   
#' @details If `object` contains 'graph6' or 'digraph6' symbols, which are in
#'   fact encoded adjacency matrices, the function will return corresponding
#'   edgelist matrices creating temporary igraph objects internally.
#' 
#' @return A list of edgelist matrices.
#' 
#' @examples 
#' # Graph6 symbols
#' sampleg6
#' edgelist_from_text(sampleg6)
#' 
#' # Sparse6 symbols
#' s6 <- c(":DgXI@G~", ":DgWCgCb")
#' edgelist_from_text(s6)
#' 
#' # Digraph6 symbol
#' d6 <- "&N????C??D?_G??C?????_?C_??????C??Q@O?G?"
#' edgelist_from_text(d6)
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
  if("digraph6" %in% fmt) {
    requireNamespace("igraph")
    iglist <- igraph_from_digraph6(object[fmt == "digraph6"])
    rval[fmt == "digraph6"] <- lapply(iglist, igraph::as_edgelist)
  }
  rval[fmt == "sparse6"] <- edgelist_from_sparse6(object[fmt == "sparse6"])
  rval
}
