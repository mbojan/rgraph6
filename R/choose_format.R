#' Choose format heuristically
#' 
#' Given a graph suggest efficient format out of 'graph6', 'sparse6' or 'digraph6'.
#' 
#' @param object Igraph/network object or list thereof
#' @param ... other arguments, currently ignored
#' 
#' @details If `object` is directed, the suggested format is 'digraph6'. If
#'   `object` is undirected the function suggests 'sparse6' if density is less
#'   than 0.15 and 'graph6' otherwise. This rule is approximate and will be
#'   replaced with exact one in the future.
#'   
#' @return Character value out of 'graph6', 'sparse6' or 'digraph6'. If `object`
#'   is a list, a vector of such values of the length equal to the length of
#'   `object`.
#' 
#' @export
choose_format <- function(object, ...) UseMethod("choose_format")

#' @rdname choose_format
#' @export
choose_format.default <- function(object, ...) {
  if(gr_directed(object)) "digraph6" else
    if(gr_density(object) < 0.15) "sparse6" else "graph6"
}

#' @rdname choose_format
#' @export
choose_format.list <- function(object, ...) {
  lapply(object, choose_format)
}
