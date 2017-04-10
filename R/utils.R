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
expand_to_length <- function(x, l=ceiling(length(x)/6)*6, what=0, where=c("end", "start")) {
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


expandToLength <- expand_to_length###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# split a vector into subvectors of length 'l' and return a list
###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

setGeneric("splitInto", function(x, l) standardGeneric("splitInto"))

setMethod("splitInto", "character",
function(x, l)
{
    f <- seq(1, length(x), by=l)
    rval <- substring(x, f, f+l-1)
    rval
} )

setMethod("splitInto", "numeric",
function(x, l)
{
    ch <- paste(x, collapse="")
    rval <- splitInto( ch, l=l)
    as.numeric( unlist(strsplit(rval, "")) )
} )
	

