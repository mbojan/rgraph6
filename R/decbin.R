#' Converting between decimal and binary numbers
#' 
#' Functions for converting decimal numbers to binary numbers and vice versa.
#' 
#' @param dec single decimal number to be converted
#' @param as one of "character" or "numeric", type of value returned, see Value
#' 
#' @return 
#' Function \code{d2b}, depending on \code{as}, returns a character vector or
#' list of numeric vectors. In both cases the length is equal to the length of 
#' \code{dec}.
#' 
#' @name decbin
#' @export
d2b <- function(dec, as=c("character", "numeric")) {
  as <- match.arg(as)
  rval <- vapply(dec, d2b_c, character(1))
  if(as == "numeric") return( lapply(strsplit(rval, ""), as.numeric) )
  else rval
}

d2b_c <- function(dec)
{
  if(!is.numeric(dec)) stop("'dec' must be numeric, got", mode(dec))
  if( dec < 0 ) stop("'dec' is negative")
  if( dec==0 ) return(as.character(0))
  if( dec==1 ) return(as.character(1))
  if( length(dec) > 1 ) {
    dec <- dec[1]
    warning("taking only first element of 'dec'")
  }
  if( (dec %% 1) != 0 ) {
    dec <- round(dec)
    warning("taking integer part of 'dec'")
  }
  # length of the output
  len <- floor(log(dec, 2)+1)
  rval <- .C(
    "dec2bin",
    decimal=as.integer(dec),
    binary=raw(len),
    len=as.integer(len) 
  )
  rawToChar(rval$binary)
}




#' @rdname decbin
#' 
#' @param bin either a character vector of strings or a list of numeric vectors.
#'   In both cases only 0s and 1s as elements are allowed.
#'   
#' @return 
#' Function \code{b2d} returns an integer vector of decimal numbers.
#' 
#' @export

b2d <- function(bin) UseMethod("b2d")

#' @rdname decbin
#' @method b2d character
#' @export
b2d.character <- function(bin) {
  l <- lapply( strsplit(bin, ""), as.integer)
  b2d.list(l)
}

#' @rdname decbin
#' @method b2d list
#' @export
b2d.list <- function(bin) {
  vapply(bin, b2d_c, integer(1))
}

b2d_c <- function(bin) {
  if(!is.numeric(bin)) stop("'bin' must be of mode numeric, got", mode(bin))
  if(!is.vector(bin)) stop("'bin' must be a vector, got", class(bin))
  if( !all(bin %in% c(0,1)) ) stop("'bin' must contain only 0s and 1s")
  # call C code
  rval <- .C(
    "bin2dec",
    binary=as.integer(bin),
    decimal=as.integer(0),
    len=as.integer(length(bin)) 
  )
  return(rval$decimal)
}



