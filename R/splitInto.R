###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
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
	

