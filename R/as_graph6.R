#' Convert undirected graphs to graph6 symbols
#' 
#' This function converts undirected graphs to graph6 symbols. Implemented
#' methods expect the graph to be an adjacency matrix, an igraph, or a network
#' object.
#' 
#' @param object a graph or a list of graphs
#' 
#' @details 
#' See [rgraph6] for graph6 format description.
#' 
#' @return A character vector of graph6 symbols.
#' 
#' @export

as_graph6 <- function(object) UseMethod("as_graph6")


#' @rdname as_graph6
#' @method as_graph6 default
#' @export
as_graph6.default <- function(object) {
  stop("don't know how to handle class ", dQuote(data.class(object)))
}

#' @rdname as_graph6
#' @export
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


#' @rdname as_graph6
#' @method as_graph6 list
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



#' @rdname as_graph6
#' @method as_graph6 igraph
#' @export
as_graph6.igraph <- function(object) {
  requireNamespace("igraph")
  stopifnot(!igraph::is_directed(object))
  as_graph6.matrix( igraph::as_adjacency_matrix(object, sparse=FALSE))
}

#' @rdname as_graph6
#' @method as_graph6 network
#' @export
as_graph6.network <- function(object) {
  requireNamespace("network")
  stopifnot(!network::is.directed(object))
  as_graph6.matrix( as.matrix(object, type="adjacency"))
}
