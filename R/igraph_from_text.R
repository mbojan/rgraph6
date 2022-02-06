#' Create igraph objects from 'graph6', 'sparse6', or 'digraph6' symbols
#' 
#' @param object character vector of 'graph6', 'sparse6', or 'digraph6' symbols
#' @examples 
#' if(requireNamespace("igraph", quietly=TRUE)) {
#'    # Graph6 symbols
#'    sampleg6
#'    igraph_from_text(sampleg6)
#' 
#'    # Sparse6 symbols
#'    s6 <- c(":DgXI@G~", ":DgWCgCb")
#'    igraph_from_text(s6)
#' 
#'    # Digraph6 symbol
#'    d6 <- "&N????C??D?_G??C?????_?C_??????C??Q@O?G?"
#'    igraph_from_text(d6)
#' }
#' @return A list of 'igraph' objects.
#' 
#' @export
igraph_from_text <- function(object) {
  stopifnot(is.character(object))
  fmt <- guess_format(object)
  rval <- vector(mode = "list", length = length(object))
  rval[fmt == "sparse6"] <- igraph_from_sparse6(object[fmt == "sparse6"])
  rval[fmt == "graph6"] <- igraph_from_graph6(object[fmt == "graph6"])
  rval[fmt == "digraph6"] <- igraph_from_digraph6(object[fmt == "digraph6"])
  rval
}
