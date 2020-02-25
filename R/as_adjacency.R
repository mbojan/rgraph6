#' Convert graph6 symbols to adjacency matrices
#' 
#' This function takes a vector of graph6 symbols and converts them into
#' symmetric square adjacency matrices.
#' 
#' @param object character vector of graph6 symbols
#' 
#' @return
#' A list of length `length(object)` of square symmetric
#' adjacency matrices corresponding to input graph6 symbols.
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
    lapply(object, as_amatrix_graph6),
    names = object
  )
}

#' @rdname as_adjacency
#' @method as_adjacency dgraph6
#' @export
as_adjacency.dgraph6 <- function(object) {
  structure(
    lapply(object, as_amatrix_dgraph6),
    names = object
  )
}

#' @rdname as_adjacency
#' @method as_adjacency sparse6
#' @export
as_adjacency.sparse6 <- function(object) {
  stop("graph(s) are in sparse6 format. use 'as_elist()' instead.")
}


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

as_amatrix_dgraph6 <- function(object) {
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
  g <- sapply(as.numeric(rg)-63, function(x)
    expand_to_length( d2b(x), l=ceiling(length(x)/6)*6, what=0, where="start") )
  g <- g[ seq(1, n^2) ]
  rval <- matrix(g,ncol = n, nrow = n, byrow = TRUE)
  rval
}