context("Testing decimal <-> binary conversion")

library(rgraph6)


test_that("Decimal -> binary works for selected numbers", {
  expect_equal( d2b(0), 0)
  expect_equal( d2b(1), 1)
})

test_that("Binary -> decimal work for selected numbers", {
  expect_equal(b2d(1), 1)
  expect_equal(b2d(0), 0)
})

test_that("Binary -> decimal works for character input", {
  expect_equal(b2d("001"), 1)
})


s <- quote(1:256)
test_that(paste0("Conversion works for numbers ", deparse(s)), {
  # Conversions
  d <- data.frame(dec = eval(s))
  d$xbin <- sapply(d$dec, function(x) paste(d2b(x), collapse=""))
  d$xdec <- sapply(d$xbin, b2d)
  
  expect_false( any(with(d, dec != xdec)) )
})






