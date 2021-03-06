test_that("coef_to_table() works", {
  fit1 <- readRDS("../testdata/coef/fit_no_inter.RDS")
  fit2 <- readRDS("../testdata/coef/fit_inter_2.RDS")
  fit3 <- readRDS("../testdata/coef/fit_inter_3.RDS")
  col_names <- readRDS("../testdata/coef/col_names.RDS")
  tbl1 <- coef_to_table(fit1, var_names = col_names)
  tbl2 <- coef_to_table(fit2, var_names = col_names)
  tbl3 <- coef_to_table(fit3, var_names = col_names)
  tbl4 <- coef_to_table(fit3)
  expect_equal(names(tbl1), c("name", "coef", "var", "level"))
  expect_equal(ncol(tbl2), 8L)
  expect_equal(ncol(tbl3), 11L)
  expect_equal(names(tbl4), c("name", "coef"))
})
