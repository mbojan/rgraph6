# Testing decimal -> binary conversion ------------------------------------


test_that("Decimal -> binary works for selected scalars", {
  expect_equal( d2b(0), 0)
  expect_equal( d2b(1), 1)
  expect_equal( d2b(10), c(1,0,1,0))
})



s <- quote(1:256)

test_that(paste0("Decimal -> binary returns proper numeric vectors for ", deparse(s)), {
  rval <- lapply(eval(s), d2b)
  expect_type(rval, "list")
  expect_true( all(vapply(rval, is.numeric, logical(1))) )
  expect_equal(
    vapply(rval, function(x) sum(2^seq(length(x)-1, 0) * x), numeric(1)),
    eval(s)
  )
})




# Testing binary -> decimal -----------------------------------------------

test_that("Binary -> decimal work for selected numbers", {
  expect_equal(b2d(1), 1)
  expect_equal(b2d(0), 0)
})

test_that("Binary -> decimal works for character input", {
  expect_equal(b2d(c(0,0,1)), 1)
})





# Testing decimal <-> binary conversion -----------------------------------

s <- quote(1:256)
test_that(paste0("Conversion works for numbers ", deparse(s)), {
  # Conversions
  d <- data.frame(dec = eval(s))
  d$xbin <- lapply(d$dec, d2b)
  d$xdec <- lapply(d$xbin, b2d)
  
  expect_false( any(with(d, dec != xdec)) )
})






