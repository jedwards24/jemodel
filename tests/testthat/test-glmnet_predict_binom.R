test_that("glmnet_predict_binom() works", {
  f1 <- readRDS(test_path("testdata", "glmnet_predict_binom", "fit1.RDS"))
  f2 <- readRDS(test_path("testdata", "glmnet_predict_binom", "fit2.RDS"))
  xmat <- readRDS(test_path("testdata", "glmnet_predict_binom", "xmat.RDS"))
  p1 <- glmnet_predict_binom(f1, xmat)
  p2 <- glmnet_predict_binom(f1, xmat)
  expect_equal(p1$Good, p2$Good)
  expect_equal(p1$Bad, p2$Bad)
})
