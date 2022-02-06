#' Parsing `digraph6` symbols
#' 
#' @description These functions take a vector of 'digraph6' symbols and return a
#'   list of other types of objects:
#'   
#' @name from_digraph6
#' 
#' @return The returned object is:
#' 
#' @seealso [as_digraph6()] for encoding objects as 'digraph6' symbols.



#' @rdname from_digraph6
#' 
#' @description - [adjacency_from_digraph6()] creates adjacency matrices
#' 
#' @param d6 character vector of 'digraph6' symbols
#' 
#' @return - for [adjacency_from_digraph6()], a list of the same length as
#'   its input of square symmetric adjacency matrices.
#' 
#' @export
#' 
#' @examples
#' am <- matrix(rbinom(16, 1, 0.3), 4, 4)
#' d6 <- as_digraph6(am)
#' 
#' # To adjacency matrix ------------------------------------------------------
#' adjacency_from_digraph6(d6)
#' 
adjacency_from_digraph6 <- function(d6) {
  structure(
    lapply(d6, as_amatrix_digraph6),
    names = d6
  )
}


as_amatrix_digraph6 <- function(object) {
  r <- charToRaw(object)
  if( as.numeric(r[2]) == 126 & as.numeric(r[3]==126)){ #n>= 258048
    rn <- r[4:9]
    n <- b2d(unlist(lapply(as.numeric(rn)-63, 
                           function(x) expand_to_length( d2b(x), l=ceiling(length(x)/6)*6, 
                                                         what=0, where="start") )))
    rg <- r[seq(10,length(r))]    
  } else if(as.numeric(r[2]) == 126 & as.numeric(r[3]!=126)){ #n>=63 & n<=258047
    rn <- r[3:5]
    n <- b2d(unlist(lapply(as.numeric(rn)-63, 
                           function(x) expand_to_length( d2b(x), l=ceiling(length(x)/6)*6, 
                                                         what=0, where="start") )))
    rg <- r[seq(6,length(r))]
  }    
  else{ #n<63
    rn <- r[2]
    rg <- r[ seq(3, length(r)) ]
    n <- as.numeric(rn) - 63
    if(n<2){
      return(matrix(0,n,n))
    }
  }
  g <- sapply(as.numeric(rg)-63, function(x)
    expand_to_length( d2b(x), l=ceiling(length(x)/6)*6, what=0, where="start") )
  g <- g[ seq(1, n^2) ]
  rval <- matrix(g,ncol = n, nrow = n, byrow = TRUE)
  rval
}




#' @rdname from_digraph6
#' 
#' @description - [igraph_from_digraph6()] creates 'igraph' objects. Requires
#'   package \pkg{igraph} to be installed.
#'   
#' @param ... other arguments, see Details. 
#' 
#' @details For [igraph_from_digraph6()] additional arguments are passed to
#'   [igraph::graph_from_adjacency_matrix()]
#' 
#' @return - for [igraph_from_digraph6()], a list of 'igraph' objects
#' 
#' @export
#' 
#' @examples
#' # To igraph objects --------------------------------------------------------
#' if(requireNamespace("igraph", quietly=TRUE)) {
#'   igraph_from_digraph6(d6)
#' }
#' 
igraph_from_digraph6 <- function(d6, ...) {
  requireNamespace("igraph")
  amlist <- adjacency_from_digraph6(d6)
  lapply(amlist, igraph::graph_from_adjacency_matrix, mode="directed", ...)
}









#' @rdname from_digraph6
#' 
#' @description - [network_from_digraph6()] creates 'network' objects. Requires
#'   package \pkg{network} to be installed.
#' 
#' @details For [network_from_digraph6()] additional arguments are passed to
#'   [network::as.network()]
#' 
#' @return - for [network_from_digraph6()], a list of 'network' objects
#' 
#' @export
#' 
#' @examples 
#' # To network objects -------------------------------------------------------
#' if(requireNamespace("network", quietly=TRUE)) {
#'   network_from_digraph6(d6)
#' }
network_from_digraph6 <- function(d6, ...) {
  requireNamespace("network")
  amlist <- adjacency_from_digraph6(d6)
  lapply(amlist, network::as.network, directed=TRUE, ...)
}
