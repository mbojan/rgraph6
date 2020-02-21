context("Test graph6 <-> matrix conversions on some random graphs")


set.seed(666)

# How many networks to test
howmany <- 50

# Network sizes to test
sizes <- round(seq(2, 128, length=howmany))



for( s in sizes ) {
  p <- runif(1)
  
  m <- makeg(s, p) # adjacency matrix
  mname <- paste(m[lower.tri(m)], collapse="")
  
  test_that(paste0("Converting matrix <-> graph6 on graph of size ", s), {
    expect_silent(
      g6 <- as_graph6(m)
    )
    expect_s3_class(g6, "graph6")
    expect_silent(
      m2 <- as_adjacency(g6)[[1]]
    )
    expect_is(m2, "matrix")
    expect_true(ncol(m2) == nrow(m2))
    expect_type(m2, "double")
    expect_identical(m, m2)
  })
}
