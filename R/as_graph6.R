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
  structure(
    rawToChar(as.raw(r)),
    class=c("graph6", "character")
  )
}






fN <- function(x) {
  # x = integer
  if( x < 0 )  stop("'x' must be non-negative")
  if( x >= 0 && x <= 62 ) {
    return(x+63)
  } else {
    e <- d2b(x) # convert to binary
    v <- expandToLength(e, l=ceiling(length(x)/6)*6, what=0, where="start")
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
    v <- expandToLength(object, l=ceiling(k/6)*6, what=0, where="end")
  }
  # split 'v' to vectors of length 6
  rval <- split(v, rep( seq(1, length(v)/6), each=6))
  # get the names as collapsed binary numbers
  nams <- sapply(rval, paste, collapse="")
  # convert the vectors into decimal numbers adding 63 beforehand
  rval <- b2d(rval) + 63
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