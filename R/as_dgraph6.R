#' Convert adjacency matrix to dgraph6 symbols
#' 
#' This function converts a graph(s) as adjacency matrices to a dgraph6 symbol(s).
#' 
#' @param object a square binary adjacency matrix or a list of thereof
#' @param x a vector of dgraph6 symbols (of class "dgraph6")
#' @param ... other arguments
#' 
#' See [rgraph6] for dgraph6 format description.
#' 
#' @return A vector of class `dgraph6` extending `character` with dgraph6 symbols.
#' 
#' @export

as_dgraph6 <- function(object) UseMethod("as_dgraph6")

#' @rdname as_dgraph6
#' @method as_dgraph6 default
#' @export
as_dgraph6.default <- function(object) {
  stop("don't know how to handle class ", dQuote(data.class(object)))
}

#' @rdname as_dgraph6
#' @method as_dgraph6 matrix
#' @export
as_dgraph6.matrix <- function(object) {
  n <- ncol(object)
  if( n < 2)
    stop("as_dgraph6 handles networks of size greater 1")
  if( n != nrow(object) )
    stop("'object' must be square matrix")
  v <- c(t(object))
  r <- c( 38,fN(n),  fR( v ) )

  
  structure(
    rawToChar(as.raw(r)),
    class=c("dgraph6", "character")
  )
}

#' @rdname as_dgraph6
#' @method as_dgraph6 list
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
#' @method as_dgraph6 igraph
#' @export
as_dgraph6.igraph <- function(object) {
  requireNamespace("igraph")
  stopifnot(igraph::is_directed(object))
  as_dgraph6.matrix( igraph::as_adjacency_matrix(object, sparse=FALSE))
}

#' @rdname as_dgraph6
#' @method as_dgraph6 network
#' @export
as_dgraph6.network <- function(object) {
  requireNamespace("network")
  stopifnot(network::is.directed(object))
  as_dgraph6.matrix( as.matrix(object, type="adjacency"))
}



#' @rdname as_dgraph6
#' @method print dgraph6
#' @export
print.dgraph6 <- function(x, ...) {
  cat("<dgraph6>\n")
  print.default(unclass(x), ...)
}