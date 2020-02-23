#' Convert edgelist to sparse6 symbols
#'
#' This function converts a graph(s) edgelist to a sparse6 symbol(s).
#'
#' @param object an edgelist or a list of thereof
#' @param x a vector of sparse6 symbols (of class "sparse6")
#' @param ... other arguments
#'
#' See [rgraph6] for sparse6 format description.
#'
#' @return A vector of class `sparse6` extending `character` with sparse6 symbols.
#'
#' @export

as_sparse6 <- function(object) UseMethod("as_sparse6")

#' @rdname as_sparse6
#' @method as_sparse6 default
#' @export
as_sparse6.default <- function(object) {
  stop("don't know how to handle class ", dQuote(data.class(object)))
}

#' @rdname as_sparse6
#' @method as_sparse6 matrix
#' @export
as_sparse6.matrix <- function(object) {
  nc <- ncol(object)
  nr <- nrow(object)
  if( nc != 2)
    stop("as_sparse6 only handles edgelists with 2 columns")
  if(nr==0){
    stop("as_sparse6 only handles edgelists with more than 1 row")
  }
  object <- t(apply(object,1,sort,decreasing= TRUE))
  object <- object[order(object[,1]),]
  
  n <- max(object)
  k <- length(d2b(n-1))

  object <- object-1
  lbit  <- diff(c(0,object[,1]))
  curv <- sum(lbit)
  
  ubits <- t(sapply(object[,2],function(x) expand_to_length(d2b(x),l=k,what=0,where="start")))
  bits <- c(t(cbind(lbit,ubits)))
  
  if(any(lbit>1)){
    bits[bits>2] <- 2
    idx <- which(bits==2)
    
    bits <- as.character(bits)
    
    vbits <- t(sapply(object[lbit>1,1],function(x) expand_to_length(d2b(x),l=k,what=0,where="start")))  
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
  
  structure(
    rawToChar(as.raw(r)),
    class=c("sparse6", "character")
  )
}

#' @rdname as_sparse6
#' @method as_sparse6 list
#' @export
as_sparse6.list <- function(object) {
  vapply(
    object,
    function(x) {
      as_sparse6(x)
    },
    character(1)
  )
}

#' @rdname as_sparse6
#' @method as_sparse6 igraph
#' @export
as_sparse6.igraph <- function(object) {
  requireNamespace("igraph")
  stopifnot(!igraph::is_directed(object))
  as_sparse6.matrix( igraph::as_edgelist(object, names = FALSE))
}

#' @rdname as_sparse6
#' @method as_sparse6 network
#' @export
as_sparse6.network <- function(object) {
  requireNamespace("network")
  stopifnot(!network::is.directed(object))
  as_sparse6.matrix( as.matrix(object, matrix.type = "edgelist"))
}

#' @rdname as_sparse6
#' @method print sparse6
#' @export
print.sparse6 <- function(x, ...) {
  cat("<sparse6>\n")
  print.default(unclass(x), ...)
}