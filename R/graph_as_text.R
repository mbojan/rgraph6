#' Encode graph as text
#' 
#' Encode a graph as 'graph6', 'sparse6' or 'digraph6' choosing the format
#' automatically.
#' 
#' @param object igraph/network object or a list thereof
#' @param ... other arguments, currently ignored
#' 
#' @details If `object` is a list it may be a mixture of network and igraph
#' objects.
#' 
#' @return A character vector of encoded graphs.
#' 
#' @export
graph_as_text <- function(object, ...) UseMethod("graph_as_text")

#' @rdname graph_as_text
#' @export
graph_as_text.default <- function(object, ...) {
  fmt <- choose_format(object)
  do.call(paste("as_", fmt), list(object = object))
}

#' @rdname graph_as_text
#' @export
graph_as_text.list <- function(object, ...) {
  lapply(object, graph_as_text)
}
