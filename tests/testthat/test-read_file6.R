test_that("reading G6/S6 files name with and without the header", {
  expect_silent(
    rhg <- read_file6(
      system.file("exdata/sample-geng-header.g6", package="rgraph6"),
      type = "adjacency"
    )
  )
  expect_silent(
    rnhg <- read_file6(
      system.file("exdata/sample-geng-noheader.g6", package="rgraph6"),
      type = "adjacency"
    )
  )
  expect_identical(rhg, rnhg)
  expect_silent(
    rhs <- read_file6(
      system.file("exdata/sample-geng-header.s6", package="rgraph6"),
      type = "adjacency"
    )
  )
  expect_identical(rhg, rhs)
  expect_silent(
    rnhs <- read_file6(
      system.file("exdata/sample-geng-noheader.s6", package="rgraph6"),
      type = "adjacency"
    )
  )
  expect_identical(rhg, rnhs)
})
