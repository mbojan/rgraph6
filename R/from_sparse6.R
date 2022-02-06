#' Parsing 'sparse6' symbols
#' 
#' @description These functions take a character vector of 'sparse6' symbols and
#'   return a list of other types of objects:
#'   
#' @name from_sparse6
#' 
#' @return The returned object is:
#' 
#' @seealso [as_sparse6()] for encoding network data objects as 'sparse6'
#'   symbols.




#' @rdname from_sparse6
#' 
#' @description - [edgelist_from_sparse6()] creates edgelist matrices
#' 
#' @param s6 character vector of 'sparse6' symbols
#' 
#' @return - for [edgelist_from_sparse6()], a list of the same length as its
#'   input of two-column edgelist matrices. The matrix has a `gorder` attribute
#'   storing the number of vertices in the graph.
#' 
#' @export
#' 
#' @examples 
#' elm <- structure(c(1, 1, 2, 2, 4, 4, 5, 6, 9, 10, 7, 8, 4, 8, 6, 8,  
#'   8, 5, 4, 6), .Dim = c(10L, 2L))
#' s6 <- as_sparse6(elm, n = 10)
#' 
#' # To edgelist matrix -------------------------------------------------------
#' edgelist_from_sparse6(s6)
#' 

edgelist_from_sparse6 <- function(s6) {
  test_sparse6(s6)
  structure(
    lapply(s6, as_elist_sparse6),
    names = s6
  )
}


as_elist_sparse6 <- function(object){
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
    
    #hardcoding graphs with n<2
    if(n<2){
      return(structure(matrix(0,0,2),gorder = n))
    }
    # hardcoding complete graph with 2 nodes 
    if(n==2 & as.numeric(rg[1])==110){
      return(structure(matrix(c(1,2),1,2),gorder = n))
    }
    
  }
  #capture all empty graphs
  if(as.numeric(rg[1])==0){
    return(structure(matrix(0,0,2),gorder = n))
  }
  
  g <- c(sapply(as.numeric(rg)-63, function(x) expand_to_length( d2b(x), l=6, what=0, where="start")))
  k <- length(d2b(n-1))
  g <- expand_to_length(g,l=ceiling(length(g)/(k+1))*(k+1),what=1,where="end")
  
  gm <- matrix(g,ncol=k+1,byrow = TRUE)
  u <- apply(gm[,2:(k+1)],1, b2d)
  b <- gm[,1]
  
  el <- decodeElist(u,b)+1
  
  #make sure that all edges are valid
  idx <- el[ ,1] != 0 & el[ ,2] != 0 & el[,1]<= n & el[,2] <= n
  structure(el[idx,], gorder = n)
}





#' @rdname from_sparse6
#' 
#' @description - [igraph_from_sparse6()] creates 'igraph' objects. Requires
#'   package \pkg{igraph} to be installed.
#'   
#' @return - for [igraph_from_sparse6()], a list of 'igraph' objects
#' 
#' @export
#' 
#' @examples 
#' # To igraph object ---------------------------------------------------------
#' if(requireNamespace("igraph", quietly=TRUE)) {
#'   igraph_from_sparse6(s6)
#' }
#' 
igraph_from_sparse6 <- function(s6) {
  requireNamespace("igraph")
  ellist <- edgelist_from_sparse6(s6)
  my_graph_from_edgelist <- function(el) {
    gempty <- igraph::make_empty_graph(attr(el, "gorder"), directed=FALSE)
    igraph::add_edges(gempty, t(el))
  }
  lapply(ellist, my_graph_from_edgelist)
}





#' @rdname from_sparse6
#' 
#' @description - [network_from_sparse6()] creates 'network' objects. Requires
#'   package \pkg{network} to be installed.
#' 
#' @return - for [network_from_sparse6()], a list of 'network' objects
#' 
#' @export
#' 
#' @examples
#' # To network object --------------------------------------------------------
#' if(requireNamespace("network", quietly=TRUE)) {
#'   network_from_sparse6(s6)
#' }
#' 
network_from_sparse6 <- function(s6) {
  requireNamespace("network")
  elist <- edgelist_from_sparse6(s6)
  lapply(
    elist,
    function(el) network::add.edges(
      network::network.initialize(attr(el, "gorder"), directed=FALSE),
      el[,1], el[,2]
    )
  )
}
