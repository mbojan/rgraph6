###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# converting a decimal number to a binary number
###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

dec2bin <- function(dec, num=TRUE)
{
    rval <- d2b(dec)
    if(num)
	return( as.numeric(strsplit(rval, "")[[1]]) )
    else
    rval
}

