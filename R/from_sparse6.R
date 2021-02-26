#' Functions parsing sparse6 symbols
#' 
#' @description These functions take a vector of sparse6 symbols and return a
#'   list of other types of objects:
#'   
#' @name from_sparse6
#' 
#' @return The returned object is:
#' 
#' @seealso [as_sparse6()] for saving objects as sparse6 symbols.




#' @rdname from_sparse6
#' 
#' @description - [edgelist_from_sparse6()] creates edgelist matrices
#' 
#' @param s6 character vector of sparse6 symbols
#' 
#' @return - for [edgelist_from_sparse6()], a list of the same length as
#'   its input of two-column edgelist matrices.
#' 
#' @export

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
  el[idx,]
}





#' @rdname from_sparse6
#' 
#' @description - [igraph_from_sparse6()] creates igraph objects. Requires
#'   package \pkg{igraph} to be installed.
#'   
#' @param ... other arguments, see Details. 
#' 
#' @details For [igraph_from_sparse6()] additional arguments are passed to
#'   [igraph::graph_from_edgelist()]
#' 
#' @return - for [igraph_from_sparse6()], a list of igraph objects
#' 
#' @export
igraph_from_sparse6 <- function(s6, ...) {
  requireNamespace("igraph")
  ellist <- edgelist_from_sparse6(s6)
  lapply(ellist, igraph::graph_from_edgelist, directed=FALSE, ...)
}





#' @rdname from_sparse6
#' 
#' @description - [network_from_sparse6()] creates network objects. Requires
#'   package \pkg{network} to be installed.
#' 
#' @details For [network_from_sparse6()] additional arguments are passed to
#'   [network::as.network()]
#' 
#' @return - for [network_from_sparse6()], a list of network objects
#' 
#' @export
network_from_sparse6 <- function(s6, ...) {
  requireNamespace("network")
  elist <- edgelist_from_sparse6(s6)
  lapply(elist, network::as.network, directed=FALSE, ...)
}
