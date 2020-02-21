context("Testing as_graph6")



test_that("errors are given for improper input",{
  expect_error( as_graph6(matrix(1, 1, 1)), regexp="handles networks of size greater 1")
  expect_error( as_graph6(1), regexp="handle class")
})



test_that("works for simple matrices", {
  m <- matrix(c(0,1,1,0), 2, 2) # complete size 2
  expect_silent(as_graph6(m))
})





test_that("annoying 'corrupted size vs prev_size' crash", {
  m <- structure(c(0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 1, 1,  1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 0, 1, 1, 1,  1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 1, 1, 0, 1,  0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1,  1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1,  1, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 1,  0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 1, 1, 1, 0, 1,  0), .Dim = c(12L, 12L))
  expect_silent(
    as_graph6(m)
  )
})






# Some low-level tests ----------------------------------------------------



context("Test fN(n) for 0 <= n <= 62")

for(n in round(seq(0, 62, length=10))) {
  test_that(
    paste0("fN(n) for n=", n), {
      expect_equal(fN(n), n + 63)
    })
} 







context("fN(n) 63 <= n <= 258047")

for(n in round(seq(63, 258047, length=10))) {
  test_that(
    paste0("fN(n) for n=", n), {
      r <- fN(n)
      expect_length(r, 4)
      expect_true(all(unlist(r) >= 63))
      expect_true(all(unlist(r) <= 126))
    })
}



# This test seems to be going beyond the integer range from certain value of `n`
# an larger. It still does work for the graphs up to size 128. To investigate.
#
# context("fN(n) 258048 <= n <= 68719476735")
# 
# 
# for(n in round(seq(258048, 68719476735, length=10))) {
#   test_that(
#     paste0("fN(n) for n=", n), {
#       r <- fN(n)
#       expect_length(r, 8)
#       expect_true(all(unlist(r) >= 63))
#       expect_true(all(unlist(r) <= 126))
#     })
# }
