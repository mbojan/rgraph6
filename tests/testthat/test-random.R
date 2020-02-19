set.seed(666)

# How many networks to test
howmany <- 40


# Network sizes to test
sizes <- c(2, 3, round(seq(4, 62, length= howmany - 2)))

# Create a random adjacency matrix of given size and tie probability.
# 
# @param size network size
# @param p tie probability
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
  p <- runif(1)
  
  m <- makeg(s, p) # adjacency matrix
  mname <- paste(m[lower.tri(m)], collapse="")

  context(paste0("Testing matrix -> graph6 conversion on graph ", paste(deparse(m), collapse=" ")))

  expect_silent(
    g6 <- as_graph6(m)
  )
  expect_s3_class(g6, "graph6")
  
  context(paste0("Testing matrix <- graph6 conversion on graph ", g6))
  expect_silent(
    m2 <- as_adjacency(g6)[[1]]
  )
  expect_is(m2, "matrix")
  expect_true(ncol(m2) == nrow(m2))
  expect_type(m2, "double")
  expect_identical(m, m2)
}
