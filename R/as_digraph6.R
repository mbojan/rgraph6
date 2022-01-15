#' Convert adjacency matrix to digraph6 symbols
#' 
#' This function converts a graph(s) as adjacency matrices to a digraph6 symbol(s).
#' 
#' @param object a square binary adjacency matrix or a list of thereof
#' 
#' See [rgraph6] for digraph6 format description.
#' 
#' @return A character vector of digraph6 symbols.
#' 
#' @export

as_digraph6 <- function(object) UseMethod("as_digraph6")

#' @rdname as_digraph6
#' @export
as_digraph6.default <- function(object) {
  stop("don't know how to handle class ", dQuote(data.class(object)))
}

#' @rdname as_digraph6
#' @export
#' @examples
#' # From adjacency matrix ----------------------------
#' am1 <- matrix(c(
#'   0,1,0,
#'   0,0,1,
#'   1,0,0),
#'   byrow=TRUE, ncol=3, nrow=3)
#' as_digraph6(am1) 
#' 
as_digraph6.matrix <- function(object) {
  n <- ncol(object)
  # if( n < 2)
  #   stop("as_digraph6 handles networks of size greater 1")
  if( n != nrow(object) )
    stop("'object' must be square matrix")
  v <- c(t(object))
  r <- c( 38,fN(n),  fR( v ) )

  rawToChar(as.raw(r))
}

#' @rdname as_digraph6
#' @export
as_digraph6.list <- function(object) {
  vapply(
    object, 
    function(x) {
      as_digraph6(x)
    },
    character(1)
  )
}

#' @rdname as_digraph6
#' @export
#' @examples
#' # From igraph objects ----------------------------
#' if(requireNamespace("igraph")) {
#'   g <- igraph::graph_from_adjacency(am1)
#'   as_digraph6(g)
#' }
#' 
as_digraph6.igraph <- function(object) {
  requireNamespace("igraph")
  stopifnot(igraph::is_directed(object))
  as_digraph6.matrix( igraph::as_adjacency_matrix(object, sparse=FALSE))
}

#' @rdname as_digraph6
#' @export
#' @examples
#' # From network objects ---------------------------
#' if(requireNamespace("network")) {
#'   net <- network::network(am1)
#'   as_digraph6(net)
#' }
#' 
as_digraph6.network <- function(object) {
  requireNamespace("network")
  stopifnot(network::is.directed(object))
  as_digraph6.matrix( as.matrix(object, type="adjacency"))
}
