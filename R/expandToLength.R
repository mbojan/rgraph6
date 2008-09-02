###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# expand a sequence 'x' to be of length 'l' by adding 'what' at the start or
# end
###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

expandToLength <- function(x, l=ceiling(length(x)/6)*6,
    what=0, where="end")
{
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

