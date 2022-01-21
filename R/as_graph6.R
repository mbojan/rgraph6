#' Encode network data as 'graph6' symbols
#' 
#' Generic function encoding undirected networks as 'graph6' symbol(s). See
#' below for available methods.
#' 
#' @param object a matrix, an igraph object or a network object or a list
#'   thereof. See Methods section below.
#' 
#' @details The 'graph6' format is designed for undirected graphs. Error is thrown
#' in case it is given a directed graph.
#' 
#' @return A character vector of 'graph6' symbols.
#' 
#' @export

as_graph6 <- function(object) UseMethod("as_graph6")



#' @describeIn as_graph6 Expects `object` to be a square matrix which is
#'   interpreted as an adjacency matrix of an undirected graph. The function
#'   reads only the upper triangle of the matrix and there is no test whether
#'   the matrix is symmetric.
#'   
#' 
#' @export
#' @examples
#' # From adjacency matrix ----------------------------------------------------
#' am <- matrix(c(
#'   0,1,1,
#'   1,0,0,
#'   1,0,0
#'   ), byrow=TRUE, ncol=3)
#' as_graph6(am)
#' 
as_graph6.matrix <- function(object) {
  n <- ncol(object)
  # if( n < 2)
  #   stop("as_graph6 handles networks of size greater 1")
  if( n != nrow(object) )
    stop("'object' must be square matrix")

  v <- object[ upper.tri(object) ]
  r <- c(
    fN(n),
    if(length(v) > 0) fR(v) else NULL
  )
  rawToChar(as.raw(r))
}





#' @describeIn as_graph6 Igraph `object` needs to be an undirected graph.
#'   Requires \pkg{igraph} package.
#' 
#' @export
#' @examples 
#' # From igraph objects ------------------------------------------------------
#' if(requireNamespace("igraph", quietly=TRUE)) {
#'   g <- igraph::graph_from_adjacency_matrix(am, mode = "undirected")
#'   as_graph6(g)
#' }
#' 
as_graph6.igraph <- function(object) {
  requireNamespace("igraph")
  stopifnot(!igraph::is_directed(object))
  as_graph6.matrix( igraph::as_adjacency_matrix(object, sparse=FALSE))
}



#' @describeIn as_graph6 Network `object` needs to be a directed network.
#'   Requires \pkg{network} package.
#' 
#' @export
#' @examples
#' # From network objects -----------------------------------------------------
#' if(requireNamespace("network", quietly=TRUE)) {
#'   net <- network::network(am, directed=FALSE)
#'   as_graph6(net)
#' }
#' 
as_graph6.network <- function(object) {
  requireNamespace("network")
  stopifnot(!network::is.directed(object))
  as_graph6.matrix( as.matrix(object, type="adjacency"))
}


#' @describeIn as_graph6 If `object` is a list the function is applied to each
#'   element. Consequently, it can be a list with a mixture of supported objects
#'   classes (adjacency matrices, igraph, or network objects).
#' 
#' @export
as_graph6.list <- function(object) {
  vapply(
    object, 
    function(x) {
      as_graph6(x)
    },
    character(1)
  )
}



#' @describeIn as_graph6 The default method throws an error about an unhandled
#'   class.
#' 
#' @export
as_graph6.default <- function(object) {
  stop("don't know how to handle class ", dQuote(data.class(object)))
}
