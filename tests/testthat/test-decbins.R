context("Testing decimal <-> binary conversion")

library(rgraph6)


test_that("Decimal -> binary works for selected numbers", {
  expect_equal( dec2bin(0), 0)
  expect_equal( dec2bin(1), 1)
})

test_that("Binary -> decimal work for selected numbers", {
  expect_equal(bin2dec(1), 1)
  expect_equal(bin2dec(0), 0)
})

test_that("Binary -> decimal works for character input", {
  expect_equal(bin2dec("001"), 1)
})


s <- quote(1:256)
test_that(paste0("Conversion works for numbers ", deparse(s)), {
  # Conversions
  d <- data.frame(dec = eval(s))
  d$xbin <- sapply(d$dec, function(x) paste(dec2bin(x), collapse=""))
  d$xdec <- sapply(d$xbin, bin2dec)
  
  expect_false( any(with(d, dec != xdec)) )
})






