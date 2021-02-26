#' Functions parsing graph6 symbols
#' 
#' @description These functions take a vector of graph6 symbols and return a
#'   list of other types of objects:
#'   
#' @name from_graph6
#' 
#' @return The returned object is:
#' 
#' @seealso [as_graph6()] for saving objects as graph6 symbols.



#' @rdname from_graph6
#' 
#' @description - [adjacency_from_graph6()] creates adjacency matrices
#' 
#' @param g6 character vector of graph6 symbols
#' 
#' @return - for [adjacency_from_graph6()], a list of the same length as
#'   its input of square symmetric adjacency matrices.
#' 
#' @export
adjacency_from_graph6 <- function(g6) {
  structure(
    lapply(g6, as_amatrix_graph6),
    names = g6
  )
}

# Actual computation for a single graph6 symbol
as_amatrix_graph6 <- function(object) {
  r <- charToRaw(object)
  if( as.numeric(r[1]) == 126 & as.numeric(r[2]==126)){ #n>= 258048
    rn <- r[3:8]
    n <- b2d(unlist(lapply(as.numeric(rn)-63, 
                           function(x) expand_to_length( d2b(x), l=ceiling(length(x)/6)*6, 
                                                         what=0, where="start") )))
    rg <- r[seq(9,length(r))]    
  } else if(as.numeric(r[1]) == 126 & as.numeric(r[2]!=126)){ #n>=63 & n<=258047
    rn <- r[2:4]
    n <- b2d(unlist(lapply(as.numeric(rn)-63, 
                           function(x) expand_to_length( d2b(x), l=ceiling(length(x)/6)*6, 
                                                         what=0, where="start") )))
    rg <- r[seq(5,length(r))]
  }    
  else{ #n<63
    rn <- r[1]
    rg <- r[ seq(2, length(r)) ]
    n <- as.numeric(rn) - 63
  }
  g <- sapply(as.numeric(rg)-63, function(x)
    expand_to_length( d2b(x), l=ceiling(length(x)/6)*6, what=0, where="start") )
  g <- g[ seq(1, n*(n-1)/2) ]
  trval <- matrix(0, ncol=n, nrow=n)
  trval[ upper.tri(trval) ] <- g
  rval <- t(trval)
  rval[ upper.tri(rval) ] <- g
  rval
}



#' @rdname from_graph6
#' 
#' @description - [igraph_from_graph6()] creates igraph objects. Requires
#'   package \pkg{igraph} to be installed.
#'   
#' @param ... other arguments, see Details. 
#' 
#' @details For [igraph_from_graph6()] additional arguments are passed to
#'   [igraph::graph_from_adjacency_matrix()]
#' 
#' @return - for [igraph_from_graph6()], a list of igraph objects
#' 
#' @export
igraph_from_graph6 <- function(g6, ...) {
  requireNamespace("igraph")
  amlist <- adjacency_from_graph6(g6)
  lapply(amlist, igraph::graph_from_adjacency_matrix, mode="undirected", ...)
}




#' @rdname from_graph6
#' 
#' @description - [network_from_graph6()] creates network objects. Requires
#'   package \pkg{network} to be installed.
#' 
#' @details For [network_from_graph6()] additional arguments are passed to
#'   [network::as.network()]
#' 
#' @return - for [network_from_graph6()], a list of network objects
#' 
#' @export
network_from_graph6 <- function(g6, ...) {
  requireNamespace("network")
  amlist <- adjacency_from_graph6(g6)
  lapply(amlist, network::as.network, directed=FALSE, ...)
}
