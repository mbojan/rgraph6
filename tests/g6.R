###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
# testing g6 conversions
###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-


library(rgraph6)


###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
### check some random graphs
###-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

# how many networks to test
howmany <- 500

# network sizes to test
sizes <- 2:9

# create a random un-directed graph matrix of given size and given tie
# probability
makeg <- function(size, p)
{
    # vector for lower triangle
    v <- sample(0:1, size*(size-1)/2, replace=TRUE, prob=c(1-p, p))
    # graph adjacency matrix
    m <- matrix(0, ncol=size, nrow=size)
    # fill-in the triangles
    m[lower.tri(m)] <- v
    tm <- t(m)
    tm[lower.tri(tm)] <- v
    return(t(tm))
}

# list of graphs
gl <- lapply( seq(1, howmany), function(x)
    makeg( sample(sizes,1), runif(1) ) )



# from graph matrix to graph6 format
r <- NULL
err <- NULL
for( i in seq(1, howmany))
{
    g6 <- try(asGraph6(gl[[i]]))
    if(!is.character(g6))
	err <- c(err, g)
    else
	r <- c(r, g6)
}

summary(err)


err <- NULL
r2 <- NULL
for( i in seq(1, length(r)))
{
    g <- try( asAMatrix(r[i]) )
    if(class(g) == "try-error")
	err <- c(err, r[i])
    else
	r2 <- c(r2, g)
}

summary(err)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

testg <- function(g)
{
    g6 <- try(asGraph6(g))
    rg <- try(asAMatrix(g6))
    rg6 <- try(asGraph6(rg))
    rval <- TRUE
    if(!identical(g6, rg6))
    {
	cat("G6\n")
	cat(g, " ", rg)
	rval <- FALSE
    }
    if(!identical(g, rg))
    {
	cat("Matrices:\n")
	print( g[lower.tri(g)] )
	print( rg[lower.tri(rg)] )
	rval <- FALSE
    }
    return(rval)
}

r <- NULL
for( i in seq(1, howmany) )
{
    g <- makeg( sample(sizes, 1), p=runif(1) )
    r <- c(r, testg(g))
}

summary(r)

#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-

### full test on the external g6 file

testg6 <- function(g6)
{
    g <- try(asAMatrix(g6))
    rg6 <- try(asGraph6(g))
    rg <- try(asAMatrix(rg6))
    rval <- TRUE
    if(!identical(g6, rg6))
    {
	cat("G6\n")
	cat(g, " ", rg)
	rval <- FALSE
    }
    if(!identical(g, rg))
    {
	cat("Matrices:\n")
	print( g[lower.tri(g)] )
	print( rg[lower.tri(rg)] )
	rval <- FALSE
    }
    return(rval)
}

# test all g6 networks
if(FALSE)
{
    d <- readLines("all2_9canon.g6")
    r <- NULL
    for( i in seq(1, length(d)) )
	r <- c(r, testg6(d[i]))
    summary(r)
    if(!all(r))
	which(!r)
}

