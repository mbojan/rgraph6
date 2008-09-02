###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# Function R(.) as in http://cs.anu.edu.au/people/bdm/data/formats.txt
###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

setGeneric("fR", function(object) standardGeneric("fR"))

setMethod("fR", "numeric", # numeric vector of 0s and 1s
function(object)
{
    if( !all( object %in% c(0,1) ) )
	stop("argument must contain only 0s or 1s")
    k <- length(object)
    # make 'v' be of length divisible by 6 by adding 0s at the end
    if( (k %% 6) == 0 )
	v <- object
    else
	v <- expandToLength(object, l=ceiling(k/6)*6, what=0, where="end")
    # split 'v' to vectors of length 6
    rval <- split(v, rep( seq(1, length(v)/6), each=6))
    # get the names as collapsed binary numbers
    nams <- sapply(rval, paste, collapse="")
    # convert the vectors into decimal numbers adding 63 beforehand
    rval <- sapply(rval, function(x) bin2dec(x) + 63 )
    names(rval) <- nams
    return(rval)
} )

