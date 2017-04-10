#' Convert a graph6 symbol to adjacency matrix
#' 
#' This function takes a graph6 symbol and converts it to a symmetric square
#' adjacency matrix.
#' 
#' @param object character, a graph6 symbol
#' 
#' @return A square symmetric adjacency matrix representing the graph.
#' 
#' @note This function aim at implementing format and methods originally
#' created by Brendan McKay, \email{bdm@cs.anu.edu.au} and
#' \url{http://cs.anu.edu.au/~bdm}.
#' 
#' @references \url{http://cs.anu.edu.au/people/bdm/data/formats.txt}
#' 
#' @export

as_adjacency <- function(object) UseMethod("as_adjacency")



#' @rdname as_adjacency
#' @method as_adjacency default
#' @export
as_adjacency.default <- function(object) {
  as_adjacency.graph6(
    structure(
      as.character(object),
      class=c("graph6", "character")
    )
  )
}



#' @rdname as_adjacency
#' @method as_adjacency graph6
#' @export
as_adjacency.graph6 <- function(object) {
  structure(
    lapply(object, as_amatrix),
    names = object
  )
}



as_amatrix <- function(object) {
  r <- charToRaw(object)
  if( as.numeric(r[1]) == 126 ) # number of actors larger than 63
    stop("networks larger than 63 nodes not yet supported")
  else
  {
    rn <- r[1]
    rg <- r[ seq(2, length(r)) ]
  }
  n <- as.numeric(rn) - 63
  g <- sapply(as.numeric(rg)-63, function(x)
    expandToLength( d2b(x, as="numeric")[[1]], l=ceiling(length(x)/6)*6, what=0, where="start") )
  g <- g[ seq(1, n*(n-1)/2) ]
  trval <- matrix(0, ncol=n, nrow=n)
  trval[ upper.tri(trval) ] <- g
  rval <- t(trval)
  rval[ upper.tri(rval) ] <- g
  rval
}