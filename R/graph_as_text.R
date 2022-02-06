#' Encode graph as text
#' 
#' Encode a graph as 'graph6', 'sparse6' or 'digraph6' choosing the format
#' automatically.
#' 
#' @param object igraph/network object or a list thereof
#' @param ... other arguments, currently ignored
#' 
#' @details If `object` is a list it may be a mixture of 'network' and 'igraph'
#' objects.
#' 
#' @return A character vector of encoded graphs.
#' 
#' @seealso [choose_format()]
#' 
#' @examples 
#' # From igraph ------------------------------------------------------
#' if(requireNamespace("igraph")) {
#'    g <- igraph::graph.famous("Zachary")
#'    graph_as_text(g)
#'    
#'    glist <- list(
#'       igraph::sample_gnp(n = 15,p = 0.1),
#'       igraph::sample_gnp(n = 15,p = 0.2), 
#'       igraph::sample_gnp(n = 15,p = 0.3))
#'       
#'    graph_as_text(glist)
#' }
#' 
#' # From network -----------------------------------------------------
#' if(requireNamespace("network")) {
#'    m <- matrix(rbinom(25,1,.4),5,5)
#'    diag(m) <- 0
#'    g <- network::network(m, directed=FALSE)
#'    graph_as_text(g)
#' }
#' @export
graph_as_text <- function(object, ...) UseMethod("graph_as_text")

#' @describeIn graph_as_text The default method chooses the encoding format
#'   automatically using [choose_format()].
#'
#' @export
graph_as_text.default <- function(object, ...) {
  fmt <- choose_format(object)
  do.call(paste0("as_", fmt), list(object = object))
}



#' @describeIn graph_as_text The list method applies the default method to each
#'   element.
#' 
#' @export
graph_as_text.list <- function(object, ...) {
  lapply(object, graph_as_text)
}
