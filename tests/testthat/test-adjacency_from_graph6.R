
test_that("converting full network of size 2", {
  m <- matrix(c(0,1,1,0), 2, 2)
  expect_identical(
    m,
    adjacency_from_graph6(as_graph6(m))[[1]]
  )
})

test_that("converting empty network of size 2", {
  m <- matrix(c(0,0,0,0), 2, 2)
  expect_identical(
    m,
    adjacency_from_graph6(as_graph6(m))[[1]]
  )
})

test_that("converting empty network of size 12", {
  m <- matrix(rep(0, 12*12), 12, 12)
  expect_identical(
    m,
    adjacency_from_graph6(as_graph6(m))[[1]]
  )
})
