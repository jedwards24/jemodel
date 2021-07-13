test_that("one_hot works", {
  hot1 <- one_hot(yield~ ., npk)
  hot2 <- one_hot(yield~ ., npk, intercept = TRUE)
  expect_true("matrix" %in% class(hot1))
  expect_equal(ncol(hot1) + 1L, ncol(hot2))
  expect_true("(Intercept)" %in% colnames(hot2))
  expect_false("(Intercept)" %in% colnames(hot1))
  expect_true("block1" %in% colnames(hot1))
})

test_that("one_hot does not change ordered factors", {
  dat <- data.frame(x = factor(c(1, 4, 2, 2), ordered = TRUE), y = 0)
  hot <- one_hot(y ~ x, dat, intercept = TRUE)
  mm <- model.matrix(y ~ x, dat)
  expect_identical(hot, mm)
})



