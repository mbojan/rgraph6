context("Preparing random graphs")

set.seed(666)

# how many networks to test
howmany <- 5

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
  t(tm)
}

# list of graphs
glist <- lapply( 
  seq(1, howmany), 
  function(x) makeg( sample(sizes,1), runif(1) ) 
)
names(glist) <- vapply(glist, function(m) paste(m[lower.tri(m)], collapse=""), character(1))




context("Converting random graphs: matrix <-> graph6")

for(i in seq(along=glist)) {
  g6 <- try(as_graph6(glist[[i]]))
  test_that(
    paste0("Running as_graph6 on ", names(glist[i])), {
      expect_s3_class(g6, "graph6")
    }
  )
  
  m <- try(as_adjacency(g6)[[1]])
  test_that(
    paste0("Running as_adjacency on ", g6), {
      expect_is(m, "matrix")
      expect_true(ncol(m) == nrow(m))
      expect_type(m, "double")
      expect_identical(glist[[i]], m)
    }
  )
}
