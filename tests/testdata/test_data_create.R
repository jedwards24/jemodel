# Create some fitted glmnet models for tests

library(tidyverse)
library(glmnet)

set.seed(21)
dt <- ggplot2::diamonds %>%
  dplyr::mutate(top = (cut == "Ideal") %>% factor(levels = c(FALSE, TRUE))) %>%
  dplyr::select(-cut) %>%
  dplyr::sample_n(500) %>%
  mutate(clarity = factor(clarity, ordered = FALSE)) #changed to test both types of factors

# No interactions
xmat <- model.matrix(top ~ ., dt)[, -1]
set.seed(22)
fit <- cv.glmnet(x=xmat, y = dt$top, family="binomial", nfolds = 3)

coef_to_table(fit, names(dt))

# This has interactions but no ordered levels
xmat2 <- model.matrix(top ~ . * table, dt)[, -1]
set.seed(22)
fit2 <- cv.glmnet(x=xmat2, y = dt$top, family="binomial", nfolds = 3)
coef(fit2, s = "lambda.min")

# multiple interactions and ordered levels
xmat3 <- model.matrix(top ~ . * color * table, dt)[, -1]
set.seed(22)
fit3 <- cv.glmnet(x=xmat3, y = dt$top, family="binomial", nfolds = 3)

coef(fit3, s = "lambda.min")
coef(fit3)

if (F){
  saveRDS(fit, "tests/testdata/coef/fit_no_inter.RDS", version = 2)
  saveRDS(fit2, "tests/testdata/coef/fit_inter_2.RDS", version = 2)
  saveRDS(fit3, "tests/testdata/coef/fit_inter_3.RDS", version = 2)
  saveRDS(names(dt), "tests/testdata/coef/col_names.RDS", version = 2)
}
edwards::object_size_all()
