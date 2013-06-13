library(rgraph6)

dec2bin(0)
dec2bin(1)

bin2dec(1)
bin2dec(0)
bin2dec("001")

# check some numbers
f <- function(dec)
{
    b <- dec2bin(dec)
    d <- bin2dec(b)
    bin <- dec2bin(d)
    if( !( identical(dec, d) & identical( b, bin ) ) )
    {
	rval <- c( dec, paste(b, collapse=""), d, paste(bin, collapse="") )
	names(rval) <- c("origDEC", "prodBIN", "reprodDEC", "reprodBIN")
	return(rval)
    }
}

r <- NULL
for( i in 1:256)
    r <- rbind(r, f(i))
r



