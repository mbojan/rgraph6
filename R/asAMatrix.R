###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# convert 'graph6' symbol to graph adjacency matrix
###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

setGeneric("asAMatrix", function(object) standardGeneric("asAMatrix"))

setMethod( "asAMatrix", "character",
function(object)
{
    r <- charToRaw(object)
    if( as.numeric(r[1]) == 126 ) # number of actors larger than 63
	stop("networks larger than 63 nodes not yet supported")
    else
    {
	rn <- r[1]
	rg <- r[ seq(2, length(r)) ]
    }
    n <- as.numeric(rn) - 63
    g <- sapply(as.numeric(rg)-63, function(x)
	expandToLength( dec2bin(x), l=ceiling(length(x)/6)*6, what=0, where="start") )
    g <- g[ seq(1, n*(n-1)/2) ]
    trval <- matrix(0, ncol=n, nrow=n)
    trval[ upper.tri(trval) ] <- g
    rval <- t(trval)
    rval[ upper.tri(rval) ] <- g
    rval
} )

