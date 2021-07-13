
# ranger  -----------

set.seed(21)
dt <- ggplot2::diamonds %>%
  dplyr::mutate(top = ifelse(cut == "Ideal", 1, 0) %>% factor(levels = c(1, 0))) %>%
  dplyr::select(-cut) %>%
  dplyr::sample_n(1000)

rf <- ranger::ranger(top ~ . , dt, seed = 20, keep.inbag = T, num.trees = 1000)
rf2 <- ranger::ranger(top ~ . , dt, seed = 20, keep.inbag = F, num.trees = 1000)

rang_oob_err(rf, dt)

rf <- ranger::ranger(top ~ . , dt, seed = 20, keep.inbag = TRUE, num.trees = 200)
rang_oob_err(rf, dt, plot = F)
expect_snapshot_value(rang_oob_err(rf, dt, plot = FALSE), style = "deparse")


rang_mtry(dt, top ~ ., 1:8, num.trees = 100)

dt[1, 2] <- NA
count_nas2(dt, all = T)
count_matches2(dt, c("F", "I1"), all = T, sort = T)

# one-hot -----------
one_hot(yield~ ., npk) %>% typeof()
nm1 <- model.matrix(yield~ ., npk) %>% colnames
nm2 <- one_hot(yield~ ., npk) %>% colnames
nm1
nm2
parsnip::contr_one_hot
#compare with makeX()
# Add to notes?
library(tidyverse)
library(glmnet)
library(edwards)
dt <- slice_sample(diamonds, n = 100)
dt <- mpg %>%
  mutate(across(where(is.character), as.factor))

x1 <- makeX(dt)
head(x1)
dim(x1)
colnames(x1)
x2 <- one_hot_mm(~., dt)
nm1 <- colnames(x1)
nm2 <- colnames(x2)

identical(x1, x2)
compare_sets(nm1, nm2)
setdiff(nm1, nm2)
compare_sets(nm1, nm2, summary = FALSE) %>% prinf

