
# Test graph6 <-> matrix conversions on some random graphs ----------------



set.seed(666)

# How many networks to test
howmany <- 5

# Network sizes to test
sizes <- round(seq(2, 128, length=howmany))



for( s in sizes ) {
  p <- runif(1)
  
  m <- makeg(s, p) # adjacency matrix
  mname <- paste(m[lower.tri(m)], collapse="")
  
  test_that(paste0("Converting matrix <-> graph6 on graph of size ", s), {
    expect_silent(
      g6 <- as_graph6(!!m)
    )
    expect_silent(
      m2 <- adjacency_from_graph6(!!g6)[[1]]
    )
    expect_is(!!m2, "matrix")
    expect_true(ncol(!!m2) == nrow(!!m2))
    expect_type(!!m2, "double")
    expect_identical(!!m, m2)
  })
  
  test_that(paste0("Converting igraph <-> graph6 on graph of size ", s), {
    requireNamespace("igraph", quietly=TRUE)
    ig <- igraph::graph_from_adjacency_matrix(m, mode="undirected")
    ig6 <- as_graph6(ig)
    ig2 <- igraph_from_graph6(ig6)
    expect_true(
      igraph::identical_graphs(!!ig, ig2[[1]])
    )
  })
  
  test_that(paste0("Converting network <-> graph6 on graph of size ", s), {
    requireNamespace("network", quietly=TRUE)
    net <- network::as.network(m, directed=FALSE)
    ng6 <- as_graph6(net)
    net2 <- network_from_graph6(ng6)
    expect_identical(!!net, net2[[1]])
  })
}


# Test digraph6 <-> matrix conversions on some random graphs ---------------


for( s in sizes ) {
  p <- runif(1)
  
  m <- maked(s, p) # adjacency matrix
  mname <- paste(m, collapse="")

  test_that(
    paste0("matrix <-> digraph6 works for ", paste(deparse(m), collapse=" ")), {
      expect_silent(
        d6 <- as_digraph6(!!m)
      )
      expect_silent(
        m2 <- adjacency_from_digraph6(!!d6)[[1]]
      )
      expect_is(m2, "matrix")
      expect_true(ncol(m2) == nrow(m2))
      expect_type(m2, "double")
      expect_identical(!!m, m2)
    }
  )
}



# Test sparse6 <-> edgelist conversions on some random graphs -------------

howmany <- 5
sizes <- round(seq(100, 200, length=howmany))

for( s in sizes ) {
  # Generate
  p <- runif(1, min=0.05, max=0.15) # only sparse networks
  m <- makeg(s, p) # adjacency matrix
  mname <- paste(m[lower.tri(m)], collapse="")
  m[lower.tri(m)] <- 0
  m <- which(m==1,arr.ind = T)
  m <- t(apply(m,1,sort,decreasing= TRUE))
  m <- m[order(m[,1]),]
  colnames(m) <- NULL
  mode(m) <- "double"
  
  test_that(
    paste0("Converting matrix <-> sparse6 on ", mname), {
    expect_silent(
      s6 <- as_sparse6(m, n = s)
    )
    expect_silent(
      m2 <- edgelist_from_sparse6(s6)[[1]]
    )
    m2 <- t(apply(m2,1,sort,decreasing= TRUE))
    m2 <- m2[order(m2[,1]),]
    
    expect_is(m2, "matrix")
    expect_type(m2, "double")
    expect_true(all(m[,1]==m2[,1]))
    expect_true(all(m[,2]==m2[,2]))
  })
}