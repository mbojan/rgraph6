#' Convert sparse6 symbols to edgelists
#' 
#' This function takes a vector of sparse6 symbols and converts them into
#' undirected edgelists.
#' 
#' @param object character vector of sparse6 symbols
#' 
#' @return
#' A list of length `length(object)` of edgelists corresponding to input sparse6 symbols.
#' 
#' @references \url{http://cs.anu.edu.au/people/bdm/data/formats.txt}
#' 
#' @export

as_elist <- function(object) UseMethod("as_elist")


#' @rdname as_elist
#' @method as_elist default
#' @export
as_elist.default <- function(object) {
  as_elist.sparse6(
    structure(
      as.character(object),
      class=c("sparse6", "character")
    )
  )
}

#' @rdname as_elist
#' @method as_elist graph6
#' @export
as_elist.graph6 <- function(object) {
  stop("graph(s) are in graph6 format. use 'as_adjacency()' instead.")
}

#' @rdname as_elist
#' @method as_elist dgraph6
#' @export
as_elist.dgraph6 <- function(object) {
  stop("graph(s) are in dgraph6 format. use 'as_adjacency()' instead.")
}


#' @rdname as_elist
#' @method as_elist sparse6
#' @export
as_elist.sparse6 <- function(object) {
  structure(
    lapply(object, as_elist_sparse6),
    names = object
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