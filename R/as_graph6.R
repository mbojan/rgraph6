#' Convert adjacency matrix to a graph6 symbol
#' 
#' This function converts a graph adjacency matrix to a graph6 symbol.
#' 
#' If necessary, more details than the description above
#' 
#' @param object a square adjacency matrix containing 0s and 1s
#' 
#' @return A graph6 symbol.
#' 
#' @note This function aim at implementing format and methods originally
#' created by Brendan McKay, \email{bdm@cs.anu.edu.au} and
#' \url{http://cs.anu.edu.au/~bdm}.
#' 
#' @export

as_graph6 <- function(object) UseMethod("as_graph6")


#' @rdname as_graph6
#' @method as_graph6 default
#' @export
as_graph6.default <- function(object) {
  stop("don't know how to handle", data.class(object))
}

#' @rdname as_graph6
#' @method as_graph6 matrix
#' @export
as_graph6.matrix <- function(object) {
  if( ncol(object) != nrow(object) )
    stop("'object' must be square matrix")
  n <- ncol(object)
  v <- object[ upper.tri(object) ]
  r <- c( fN(n),  fR( v ) )
  rawToChar(as.raw(r))
}
