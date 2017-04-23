set.seed(666)

# how many networks to test
howmany <- 20

# network sizes to test
sizes <- round(seq(2, 62, length=15))

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
  t(tm)
}


for( s in sizes ) {
  
  context(paste("Converting", howmany, "random graphs of size", s, "(matrix <-> graph6)"))
  
  # list of graphs
  matlist <- lapply( 
    seq(1, howmany), 
    function(x) makeg(s, runif(1)) 
  )
  names(matlist) <- vapply(matlist, function(m) paste(m[lower.tri(m)], collapse=""), character(1))
  for(i in seq(along=matlist)) {
    g6 <- try(as_graph6(matlist[[i]]))
    test_that(
      paste0("Running as_graph6 on ", names(matlist[i])), {
        expect_s3_class(g6, "graph6")
      }
    )
    
    m <- try(as_adjacency(g6)[[1]])
    test_that(
      paste0("Running as_adjacency on ", g6), {
        expect_is(m, "matrix")
        expect_true(ncol(m) == nrow(m))
        expect_type(m, "double")
        expect_identical(matlist[[i]], m)
      }
    )
  }
  
}
