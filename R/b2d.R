###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
### converting binary numbers to decimal with C code
###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

b2d <- function(bin)
{
    if(!is.numeric(bin))
	stop("'bin' must be of mode numeric, got", mode(bin))
    if(!is.vector(bin))
	stop("'bin' must be a vector, got", class(bin))
    if( !all(bin %in% c(0,1)) )
	stop("'bin' must contain only 0s and 1s")
    # call C code
    rval <- .C( "bin2dec",
	binary=as.integer(bin),
	decimal=as.integer(0),
	len=as.integer(length(bin)) )
    return(rval$decimal)
}

