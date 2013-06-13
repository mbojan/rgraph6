###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# represent an undirected graph as a 'graph6' symbol
###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

setGeneric("asGraph6", function(object) standardGeneric("asGraph6"))

# for adjacency matrix
setMethod("asGraph6", "matrix",
function(object)
{
    if( ncol(object) != nrow(object) )
	stop("'object' must be square matrix")
    n <- ncol(object)
    v <- object[ upper.tri(object) ]
    r <- c( fN(n),  fR( v ) )
    rawToChar(as.raw(r))
} )

