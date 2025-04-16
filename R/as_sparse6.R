#' Encode network data as `sparse6` symbols
#'
#' Generic function encoding network data as 'sparse6' symbol(s). See below for
#' available methods.
#'
#' @param object an edgelist, igraph, or network object or a list thereof. See
#'   Methods section below.
#' @param ... other arguments passed to/from other methods
#' 
#' @seealso The 'sparse6' format is designed for undirected graphs. Error is
#'   thrown in case it is given a directed graph.
#' 
#' @return A character vector of 'sparse6' symbols.
#' 
#' @export
as_sparse6 <- function(object, ...) UseMethod("as_sparse6")



#' @describeIn as_sparse6 Expects `object` to be a two-column matrix of
#'   *integers* which is interpreted as an edgelist of an undirected graph. By
#'   default the network size is inferred to be the maximal element of `object`.
#'   This can be overridden by providing the network size via the `n` argument,
#'   the results will not be identical though (see the Examples).
#' 
#' @param n number of vertices in the graph
#' 
#' @examples
#' # From edgelist matrix -----------------------------------------------------
#' elm <- matrix(c(
#'   1, 2,
#'   2, 3,
#'   3, 4
#' ), ncol=2, byrow=TRUE)
#' as_sparse6(elm) # 1--2, 2--3, 3--4
#' as_sparse6(elm + 6) # 1, 2, 3, 4, 5, 6, 7--8, 8--9, 9--10
#' as_sparse6(elm, n = 10) # 1--2, 2--3, 3--4, 5, 6, 7, 8, 9, 10
#'
#' @export
as_sparse6.matrix <- function(object, n = max(object, 0), ...) {
  nc <- ncol(object)
  nr <- nrow(object)
  if( nc != 2)
    stop("as_sparse6 only handles edgelists with 2 columns")
  if(!is.numeric(object))
    stop("Edgelist matrix needs to be numeric while it is ", mode(object))
  object <- structure(as.integer(object), dim = dim(object))
  # if(nr==0){
  #   stop("as_sparse6 only handles edgelists with more than 1 row")
  # }
  # bring edgelist in right order if needed
  if(!(all(object[,1]>object[,2]) & all(diff(object[,1])>=0))){
    object <- t(apply(object, 1, sort, decreasing = TRUE))
    object <- object[order(object[ ,1]),,drop=FALSE]  
  }
  
  # n <- max(object)
  # catch boundary case of empty graph
  if(nr!=0){
    stopifnot(n >= max(object))  
  } else{
    stopifnot(n>=0)
  }
  
  #circumvent -1 as input to d2b
  if(n>0){
    k <- length(d2b(n - 1))
  } else{
    k <- 65
  }
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




#' @describeIn as_sparse6 Igraph `object` needs to be an undirected graph.
#'   Requires \pkg{igraph} package.
#'  
#' @importFrom methods as
#' @export
#' @examples 
#' # From igraph objects ------------------------------------------------------
#' if(requireNamespace("igraph")) {
#'   g <- igraph::graph_from_edgelist(elm, directed=FALSE)
#'   as_sparse6(g)
#' }
#' 
as_sparse6.igraph <- function(object, ...) {
  requireNamespace("igraph")
  stopifnot(!igraph::is_directed(object))
  
  if (requireNamespace("Matrix", quietly = TRUE)) {
    A <- igraph::as_adjacency_matrix(object,type = "upper",sparse = TRUE)
    B <- as(A, "TsparseMatrix")
    el <- cbind(B@j,B@i)+1
  } else{
    el <- igraph::as_edgelist(object, names = FALSE)
  }
  
  as_sparse6.matrix(el, n = igraph::gorder(object))
}

#' @describeIn as_sparse6 Network `object` needs to be a directed network.
#'   Requires \pkg{network} package.
#'   
#' @export
#' @examples
#' # From network objects --------------------------------
#' if(requireNamespace("network")) {
#'   net <- network::network(elm, directed=FALSE)
#'   as_graph6(net)
#' }
#' 
as_sparse6.network <- function(object, ...) {
  requireNamespace("network")
  stopifnot(!network::is.directed(object))
  as_sparse6.matrix( as.matrix(object, matrix.type = "edgelist"), n = network::network.size(object))
}



#' @describeIn as_sparse6 If `object` is a list the function is applied to each
#'   element. Consequently, it can be a list with a mixture of supported objects
#'   classes (edgelist matrices, igraph, or network objects).
#'   
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

