###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# converting a binary number (sequence of 0s and 1s) to a decimal number
###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# this is user-friendly interface to b2d

setGeneric("bin2dec", function(bin) standardGeneric("bin2dec"))


# given a numeric vector return the decimal number

setMethod("bin2dec", "numeric",
function(bin)
{
    b2d( bin=bin )
} )


# a character scalar is assumed to contain 1s and 0s only

setMethod("bin2dec", "character",
function(bin)
{
    if(length(bin) > 1)
    {
	bin <- bin[1]
	warning("taking only the first element of 'bin'")
    }
    arg <- as.numeric( strsplit(bin, "")[[1]] )
    b2d(arg)
} )

