
test_that("Examples of fN() from documentation work", {
  expect_equal(fN(30), 93)
  expect_equivalent(fN(12345), as.list(c(126, 66, 63, 120)))
  expect_equivalent(fN(460175067), as.list(c(126, 126, 63, 90, 90, 90, 90, 90)))
})




for(n in round(seq(0, 62, length=10))) {
  test_that(
    paste0("fN(n) for n=", n), {
      expect_equal(fN(!!n), !!n + 63)
    })
} 


for(n in round(seq(63, 258047, length=10))) {
  test_that(
    paste0("fN(n) for n=", n), {
      r <- fN(n)
      expect_length(r, 4)
      expect_true(all(unlist(r) >= 63))
      expect_true(all(unlist(r) <= 126))
    })
}

for(n in round(seq(258048, 68719476735, length=10))) {
  test_that(
    paste0("fN(n) for n=", n), {
      r <- fN(n)
      expect_length(r, 8)
      expect_true(all(unlist(r) >= 63))
      expect_true(all(unlist(r) <= 126))
    })
}


test_that("Examples of fR() from documentation work", {
  expect_equivalent(
    fR(c(1,0,0,0,1,0,1,1,0,0,0,1,1,1,0,0)),
    as.list(c(97, 112, 111))
  )
})

test_that("Graph example from documentation works", {
  expect_equal(fN(5), 68)
  expect_equivalent(
    fR(c(0, 1,0, 0,1,0, 1,0,0,1)),
    as.list(c(81, 99))
  )
})
