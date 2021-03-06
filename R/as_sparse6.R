#' Encode network data as 'sparse6' symbols
#'
#' Generic function encoding network data as 'sparse6' symbol(s). See below for
#' available methods.
#'
#' @param object an edgelist, igraph, or network object or a list thereof. See
#'   Methods section below.
#' @param ... other arguments passed to/from other methods
#' 
#' @seealso See [rgraph6] for 'sparse6' format description.
#' 
#' @return A character vector with 'sparse6' symbols.
#' 
#' @export
as_sparse6 <- function(object, ...) UseMethod("as_sparse6")



#' @describeIn as_sparse6 Encode edgelist. Requries additional argument `n` with
#' the number of vertices of the graph.
#' 
#' @param n number of vertices in the graph
#'
#' @export
as_sparse6.matrix <- function(object, n, ...) {
  nc <- ncol(object)
  nr <- nrow(object)
  if( nc != 2)
    stop("as_sparse6 only handles edgelists with 2 columns")
  if(nr==0){
    stop("as_sparse6 only handles edgelists with more than 1 row")
  }
  # bring edgelist in right order if needed
  if(!(all(object[,1]>object[,2]) & all(diff(object[,1])>=0))){
    object <- t(apply(object, 1, sort, decreasing = TRUE))
    object <- object[order(object[ ,1]), ]  
  }
  
  # n <- max(object)
  stopifnot(n >= max(object))
  k <- length(d2b(n - 1))

  object <- object - 1
  lbit  <- diff(c(0, object[,1]))
  curv <- sum(lbit)
  
  # ubits <- t(sapply(object[ ,2], function(x) expand_to_length(d2b(x), l = k, what = 0,where = "start")))
  # bits <- c(t(cbind(lbit,ubits)))
  ubits <- sapply(object[ ,2], function(x) expand_to_length(d2b(x), l = k, what = 0,where = "start"))
  bits <- c(rbind(lbit,ubits))
  
  if(any(lbit>1)){
    bits[bits>2] <- 2
    idx <- which(bits==2)
    
    bits <- as.character(bits)
    
    vbits <- t(sapply(object[lbit > 1,1],function(x) expand_to_length(d2b(x),l = k,what = 0,where = "start")))  
    vbits <- cbind(1,vbits,0)
    vbits <- apply(vbits,1,paste0,collapse="")
    bits[idx] <- vbits
    bits <- as.numeric(unlist(strsplit(bits,"",fixed=TRUE)))
  }
  
  if(k < 6 & n == 2^k & (-length(bits))%%6 >= k & curv < (n-1)){
    bits <- c(bits,0)
  }
  v <- expand_to_length(bits, l=ceiling(length(bits)/6)*6, what=1, where="end")
  r <- c(58,fN(n),fR(v))
  
  rawToChar(as.raw(r))
}

#' @describeIn as_sparse6 Encode [igraph][igraph::igraph] objects. If the graph
#'   is directed an error is thrown. Package \pkg{igraph} needs to be installed.
#' @importFrom methods as
#' @export
as_sparse6.igraph <- function(object, ...) {
  requireNamespace("igraph")
  stopifnot(!igraph::is_directed(object))
  
  if (requireNamespace("Matrix", quietly = TRUE)) {
    A <- igraph::as_adj(object,type = "upper",sparse = TRUE)
    B <- as(A, "dgTMatrix")
    el <- cbind(B@j,B@i)+1
  } else{
    el <- igraph::as_edgelist(object, names = FALSE)
  }
  
  as_sparse6.matrix(el, n = igraph::gorder(object))
}

#' @describeIn as_sparse6 Encode [network][network::network()] objects. If the
#'   network is directed and error is thrown. Package \pkg{network} needs
#'   to be installed.
#' @export
as_sparse6.network <- function(object, ...) {
  requireNamespace("network")
  stopifnot(!network::is.directed(object))
  as_sparse6.matrix( as.matrix(object, matrix.type = "edgelist"), n = network::network.size(object))
}



#' @describeIn as_sparse6 Each element of the list is encoded independently with
#'   one of the other methods.
#' @export
as_sparse6.list <- function(object, ...) {
  vapply(
    object,
    function(x) {
      as_sparse6(x, ...)
    },
    character(1)
  )
}

#' @describeIn as_sparse6 The default method fails gracefully.
#' @export
as_sparse6.default <- function(object, ...) {
  stop("don't know how to handle class ", dQuote(data.class(object)))
}

