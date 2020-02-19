#' Convert adjacency matrix to a graph6 symbol
#' 
#' This function converts a graph(s) as adjacency matrices to a graph6 symbol(s).
#' 
#' @param object a square binary adjacency matrix or a list of thereof
#' @param x a vector of graph6 symbols (of class "graph6")
#' @param ... other arguments
#' 
#' See \code{\link{rgraph6}} for graph6 format description.
#' 
#' @return A vector of class \code{graph6} extending \code{character} with graph6 symbols.
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
#' @method as_graph6 matrix
#' @export
as_graph6.matrix <- function(object) {
  n <- ncol(object)
  if( n < 2 | n > 62)
    stop("as_graph6 handles networks of sizes 2-62 only")
  if( n != nrow(object) )
    stop("'object' must be square matrix")
  n <- ncol(object)
  v <- object[ upper.tri(object) ]
  r <- c( fN(n),  fR( v ) )
  structure(
    rawToChar(as.raw(r)),
    class=c("graph6", "character")
  )
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



fN <- function(x) {
  # x = integer
  if( x < 0 )  stop("'x' must be non-negative")
  if( x >= 0 && x <= 62 ) {
    return(x+63)
  } else {
    e <- d2b2(x) # convert to binary
    v <- expand_to_length(e, l=ceiling(length(x)/6)*6, what=0, where="start")
  }
  rval <- splitInto(v, 6)
  rval
}


fR <- function(object) {
  if( !all( object %in% c(0,1) ) )
    stop("argument must contain only 0s or 1s")
  k <- length(object)
  # make 'v' be of length divisible by 6 by adding 0s at the end
  if( (k %% 6) == 0 ) {
    v <- object
  } else {
    v <- expand_to_length(object, l=ceiling(k/6)*6, what=0, where="end")
  }
  # split 'v' to vectors of length 6
  rval <- split(v, rep( seq(1, length(v)/6), each=6))
  # get the names as collapsed binary numbers
  nams <- sapply(rval, paste, collapse="")
  # convert the vectors into decimal numbers adding 63 beforehand
  # rval <- b2d2(rval) + 63
  rval <- lapply(rval, function(x) b2d2(x) + 63)
  names(rval) <- nams
  return(rval)
}





#' @rdname as_graph6
#' @method print graph6
#' @export
print.graph6 <- function(x, ...) {
  cat("<graph6>\n")
  print.default(unclass(x), ...)
}