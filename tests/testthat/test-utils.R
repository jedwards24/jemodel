test_that("Standardise and normalise work", {
  expect_error(normalise(c(1, NA)), "at least two")
  expect_error(standardise(c(1, NA)), "at least two")
  expect_error(normalise(list(1:10)), "atomic numeric vector")
  expect_error(standardise(list(1:10)), "atomic numeric vector")
  x <- c(3:1, NA)
  expect_equal(standardise(x), c(1, 0, -1, NA))
  expect_equal(normalise(x), c(1, 0.5, 0, NA))
})
