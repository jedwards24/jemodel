# Create some fitted glmnet models for tests

library(tidyverse)
library(glmnet)

set.seed(21)
dt <- ggplot2::diamonds %>%
  dplyr::mutate(top = (cut == "Ideal") %>% factor(levels = c(F, T))) %>%
  dplyr::select(-cut) %>%
  dplyr::sample_n(200) %>%
  mutate(clarity = factor(clarity, ordered = FALSE)) #changed to test both types of factors

# No interactions
xmat <- model.matrix(top ~ ., dt)[, -1]
set.seed(22)
fit <- cv.glmnet(x=xmat, y = dt$top, family="binomial", nfolds = 3)

glmnet_to_table(fit, names(dt))

# This has interactions but no ordered levels
xmat2 <- model.matrix(top ~ . * table, dt)[, -1]
set.seed(22)
fit2 <- cv.glmnet(x=xmat2, y = dt$top, family="binomial", nfolds = 3)

glmnet_to_table(fit2, names(dt))
glmnet_to_table(fit2, names(dt), s = "lambda.min")


# multiple interactions and ordered levels
xmat3 <- model.matrix(top ~ . * color * table, dt)[, -1]
set.seed(22)
fit3 <- cv.glmnet(x=xmat3, y = dt$top, family="binomial", nfolds = 3)

glmnet_to_table(fit3, names(dt))
glmnet_to_table(fit3, names(dt), s = "lambda.min")

if (F){
  saveRDS(fit, "tests/testdata/glmnet/fit_simple.RDS", version = 2)
  saveRDS(fit3, "tests/testdata/glmnet/fit_complex.RDS", version = 2)
}
object_size_all()
