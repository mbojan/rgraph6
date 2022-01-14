# The N() function from http://users.cecs.anu.edu.au/~bdm/data/formats.txt
# 
# @param x non-negative integer

fN <- function(x){
  if(x < 0)  stop("'x' must be non-negative")
  if( x >= 0 && x <= 62 ) {
    return(x+63)
  } else if( x >= 63 & x<= 258047){
    e <- d2b(x)
    e <- expand_to_length(e, l=18, what=0, where="start")
    return(c(126,fR(e)))
  } else if( x > 258047 ){
    e <- d2b(x)
    e <- expand_to_length(e, l=36, what=0, where="start")
    return(c(126,126,fR(e)))
  }
}
