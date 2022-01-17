#' Encode network data as 'digraph6' symbols
#' 
#' This is a generic function for encoding directed networks as 'digraph6'
#' symbols. Implemented methods cover adjacency matrices, igraph objects, and
#' networks objects, or list thereof.
#' 
#' @param object a matrix, an igraph object or a network object or a list
#'   thereof. See Methods section below.
#' 
#' @details The 'digraph6' format is designed for directed graphs.
#' 
#' @return A character vector of 'digraph6' symbols.
#' 
#' @export

as_digraph6 <- function(object) UseMethod("as_digraph6")


#' @describeIn as_digraph6 If `object` is a matrix it is interpreted as an
#'   adjacency matrix of a directed graph.
#' 
#' @export
#' @examples
#' # From adjacency matrix ----------------------------------------------------
#' am <- matrix(c(
#'   0,1,0,
#'   0,0,1,
#'   1,0,0),
#'   byrow=TRUE, ncol=3, nrow=3)
#' as_digraph6(am)
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


#' @describeIn as_digraph6 Igraph `object` needs to be a directed. Requires
#'   **igraph** package.
#' 
#' @export
#' @examples
#' # From igraph objects ------------------------------------------------------
#' if(requireNamespace("igraph", quietly=TRUE)) {
#'   g <- igraph::graph_from_adjacency_matrix(am)
#'   as_digraph6(g)
#' }
#' 
as_digraph6.igraph <- function(object) {
  requireNamespace("igraph")
  stopifnot(igraph::is_directed(object))
  as_digraph6.matrix( igraph::as_adjacency_matrix(object, sparse=FALSE))
}

#' @describeIn as_digraph6 Network `object` needs to be directed.
#' @export
#' @examples
#' # From network objects -----------------------------------------------------
#' if(requireNamespace("network", quietly=TRUE)) {
#'   net <- network::network(am)
#'   as_digraph6(net)
#' }
#' 
as_digraph6.network <- function(object) {
  requireNamespace("network")
  stopifnot(network::is.directed(object))
  as_digraph6.matrix( as.matrix(object, type="adjacency"))
}




#' @describeIn as_digraph6 If `object` is a list the function is applied to each
#'   element. Consequently, it can be a list with elements being a mixture of
#'   supported objects (adjacency matrices, igraph, or network).
#' 
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

#' @describeIn as_digraph6 Throws an error about the unhandled class.
#' @export
as_digraph6.default <- function(object) {
  stop("don't know how to handle class ", dQuote(data.class(object)))
}
