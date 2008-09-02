###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
### converting decimal numbers to binary using C code
###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

d2b <- function(dec)
{
    if(!is.numeric(dec))
	stop("'dec' must be numeric, got", mode(dec))
    if( dec < 0 )
	stop("'dec' is negative")
    if( dec==0 )
	return(as.character(0))
    if( dec==1 )
	return(as.character(1))
    if( length(dec) > 1 )
    {
	dec <- dec[1]
	warning("taking only first element of 'dec'")
    }
    if( (dec %% 1) != 0 )
    {
	dec <- round(dec)
	warning("taking integer part of 'dec'")
    }
    # length of the output
    len <- floor(log(dec, 2)+1)
    rval <- .C("dec2bin",
	decimal=as.integer(dec),
	binary=raw(len),
	len=as.integer(len) )
    return(rawToChar(rval$binary))
}


