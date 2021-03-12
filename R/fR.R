# The fR() function from http://users.cecs.anu.edu.au/~bdm/data/formats.txt
# 
# Encoding binary vectors as encoded printable ASCII characters.
# 
# @oaram object numeric vector of 0s and 1s
# 
# @return A list of integers named with pasted binary vectors

fR <- function(object) {
  if( !all( object %in% c(0,1) ) )
    stop("argument must contain only 0s or 1s")
  k <- length(object)
  # make 'v' be of length divisible by 6 by adding 0s at the end
  if( (k %% 6) == 0 ) {
    v <- object
  } else {
    v <- expand_to_length(object, l=ceiling(k/6)*6, what=0, where="end")
  }
  # split 'v' to vectors of length 6
  rval <- split(v, rep( seq(1, length.out = length(v)/6), each=6))
  # get the names as collapsed binary numbers
  nams <- sapply(rval, paste, collapse="")
  # convert the vectors into decimal numbers adding 63
  rval <- lapply(rval, function(x) b2d(x) + 63)
  names(rval) <- nams
  return(rval)
}
