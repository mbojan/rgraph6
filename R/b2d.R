###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
### converting binary numbers to decimal with C code
###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-



#' Conversions between binary and decimal numbers
#' 
#' These functions convert between binary numbers (as numeric vectors) and
#' decimal numbers.
#' 
#' 
#' @aliases b2d d2b
#' @param bin numeric vector of 0s and 1s representing the number in binary
#' format
#' @param dec numeric non-negative scalar, decimal number to be converted
#' @return For \code{b2d} the decimal number representing the binary input.
#' For \code{d2b} the numeric vector representing the decimal as binary number:
#' sequence of 0s and 1s.
#' @seealso \code{\link{bin2dec}} and \code{\link{dec2bin}} for more user
#' friendly interface.
#' @keywords math
#' @examples
#' 
#' b2d(c(1, 1, 0, 1))
#' d2b(13)
#' 
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

