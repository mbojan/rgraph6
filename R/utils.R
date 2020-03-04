# Expand vector by adding values at the start or end
# 
# Expand a sequence \code{x} to be of length \code{l} by adding \code{what} at
# the start or end.
# 
# @param x sequence to be expanded
# @param l target length
# @param what what to replicate at the start or end
# @param where either "start" or "end"
# 
# @return 
# A vector of length \code{l}.
# 
expand_to_length <- function(x, l=ceiling(length(x)/6)*6L, what=0L, where=c("end", "start")) {
  where <- match.arg(where)
  if( length(x) == l )
    return(x)
  else
  {
    add <- rep(what, l - length(x))
    rval <- switch( where,
                    end= c(x, add),
                    start= c( add, x) )
    return(rval)
  }
}


expandToLength <- expand_to_length




###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# split a vector into subvectors of length 'l' and return a list
###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

split_into <- function(x, l) UseMethod("split_into")

splitInto <- split_into

split_into.character <- function(x, l) {
  f <- seq(1, length(x), by=l)
  rval <- substring(x, f, f+l-1)
  rval
}

split_into.numeric <- function(x, l) {
    ch <- paste(x, collapse="")
    rval <- splitInto( ch, l=l)
    as.numeric( unlist(strsplit(rval, "")) )
}
	




# Create random graphs for testing purposes -------------------------------

# Create a random adjacency matrix of given size and tie probability.
# 
# @param size network size
# @param p tie probability
makeg <- function(size, p)
{
  # vector for lower triangle
  v <- sample(0:1, size*(size-1)/2, replace=TRUE, prob=c(1-p, p))
  # graph adjacency matrix
  m <- matrix(0, ncol=size, nrow=size)
  # fill-in the triangles
  m[lower.tri(m)] <- v
  tm <- t(m)
  tm[lower.tri(tm)] <- v
  t(tm)
}

# Create a random directed adjacency matrix of given size and tie probability.
# 
# @param size network size
# @param p tie probability
maked <- function(size, p)
{
  # vector for lower triangle
  v <- sample(0:1, size^2, replace=TRUE, prob=c(1-p, p))
  # graph adjacency matrix
  m <- matrix(v, ncol=size, nrow=size)
  diag(m) <- 0
  m
}
