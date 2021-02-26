#' Convert adjacency matrix to dgraph6 symbols
#' 
#' This function converts a graph(s) as adjacency matrices to a dgraph6 symbol(s).
#' 
#' @param object a square binary adjacency matrix or a list of thereof
#' 
#' See [rgraph6] for dgraph6 format description.
#' 
#' @return A character vector of dgraph6 symbols.
#' 
#' @export

as_dgraph6 <- function(object) UseMethod("as_dgraph6")

#' @rdname as_dgraph6
#' @export
as_dgraph6.default <- function(object) {
  stop("don't know how to handle class ", dQuote(data.class(object)))
}

#' @rdname as_dgraph6
#' @export
as_dgraph6.matrix <- function(object) {
  n <- ncol(object)
  if( n < 2)
    stop("as_dgraph6 handles networks of size greater 1")
  if( n != nrow(object) )
    stop("'object' must be square matrix")
  v <- c(t(object))
  r <- c( 38,fN(n),  fR( v ) )

  rawToChar(as.raw(r))
}

#' @rdname as_dgraph6
#' @export
as_dgraph6.list <- function(object) {
  vapply(
    object, 
    function(x) {
      as_dgraph6(x)
    },
    character(1)
  )
}

#' @rdname as_dgraph6
#' @export
as_dgraph6.igraph <- function(object) {
  requireNamespace("igraph")
  stopifnot(igraph::is_directed(object))
  as_dgraph6.matrix( igraph::as_adjacency_matrix(object, sparse=FALSE))
}

#' @rdname as_dgraph6
#' @export
as_dgraph6.network <- function(object) {
  requireNamespace("network")
  stopifnot(network::is.directed(object))
  as_dgraph6.matrix( as.matrix(object, type="adjacency"))
}
