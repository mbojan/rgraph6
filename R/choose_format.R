#' Choose most efficient format heuristically
#' 
#' Given a graph suggest the most efficient format out of 'graph6', 'sparse6' or
#' 'digraph6'.
#' 
#' @param object Igraph/network object or list thereof
#' @param ... other arguments, currently ignored
#' 
#' @details If `object` is directed, the suggested format is 'digraph6'. If
#'   `object` is undirected the function suggests 'sparse6' if density is less
#'   than 0.15 and 'graph6' otherwise. This rule is approximate.
#'   
#' @return Character value out of 'graph6', 'sparse6' or 'digraph6'. If `object`
#'   is a list, a vector of such values of the length equal to the length of
#'   `object`.
#' @examples 
#' # From igraph ------------------------------------------------------
#' if(requireNamespace("igraph")) {
#'    g <- igraph::graph.famous("Zachary")
#'    choose_format(g)
#'    
#'    set.seed(123)
#'    glist <- list(
#'       igraph::sample_gnp(n = 15, p = 0.1),
#'       igraph::sample_gnp(n = 15, p = 0.2), 
#'       igraph::sample_gnp(n = 15, p = 0.3),
#'       igraph::sample_gnp(n = 15, p = 0.15, directed = TRUE))
#'       
#'    choose_format(glist)
#' }
#' 
#' # From network -----------------------------------------------------
#' if(requireNamespace("network")) {
#'    m <- matrix(rbinom(25,1,.4),15,15)
#'    diag(m) <- 0
#'    g <- network::network(m, directed=FALSE)
#'    choose_format(g)
#' }
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
