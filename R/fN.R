###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Function N(.) as in http://cs.anu.edu.au/people/bdm/data/formats.txt
###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

fN <- function(x)
# x = integer
{
    if( x < 0 )  stop("'x' must be non-negative")
    if( x >= 0 && x <= 62 )
	return(x+63)
    else
    {
	e <- dec2bin(x) # convert to binary
	v <- expandToLength(e, l=ceiling(length(x)/6)*6, what=0, where="start")
    }
    rval <- splitInto(v, 6)
    rval
}

