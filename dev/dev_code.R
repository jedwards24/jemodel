

# glmnet tools -------------
library(tidyverse)
library(glmnet)
#library(glmnetUtils)
library(edwards)
#source("funcs/glmnet.R")
set.seed(21)
dt <- ggplot2::diamonds %>%
  dplyr::mutate(top = (cut == "Ideal") %>% factor(levels = c(F, T))) %>%
  dplyr::select(-cut) %>%
  dplyr::sample_n(500) %>%
  dplyr::mutate(clarity = factor(clarity, ordered = F)) #changed to test both types of factors
dt
dt$clarity %>% levels


xmat <- model.matrix(top ~ ., dt)[, -1]
colnames(xmat)

set.seed(22)
fit0 <- cv.glmnet(x=xmat, y = dt$top, family="binomial", nfolds = 3)

xmat <- model.matrix(top ~ . * table, dt)[, -1]
colnames(xmat)

set.seed(22)
fit <- cv.glmnet(x=xmat, y = dt$top, family="binomial", nfolds = 3)
plot(fit)
colnames(xmat)
glmnet_to_table(fit, names(dt), s = "lambda.min")
glmnet_to_table(fit, names(dt))
glmnet_to_table(fit)
gg <- glmnet_to_table(fit, names(dt))
gg %>% tidyr::separate(.data$name, c("name01", "name02", "nm"), sep = ":", remove = FALSE, fill = "right")


glmnet_to_table(fit, names(dt), s = 10^-3)
glmnet_to_table(fit, s = 10^-3)

tt$name %>% str_extract_all(var_reg)
tt$name %>% str_remove_all(paste0("(?!((", var_reg, "))).+"))

# multiple interactions
xmat2 <- model.matrix(top ~ . * color * table, dt)[, -1]
set.seed(22)
fit2 <- cv.glmnet(x=xmat2, y = dt$top, family="binomial", nfolds = 3)
plot(fit2)
colnames(xmat2)
glmnet_to_table2(fit2, names(dt)) %>% prinf

glmnet_to_table2(fit2, var_names = names(dt), s = 10 ^ -2) %>% print(n=Inf)
glmnet_to_table2(fit2, s = 10 ^ -2) %>% print(n=Inf)
glmnet_to_table2(fit2)
coef(fit)

coef_to_table_simple <- function(fit, ..., min_coef=1E-10) {
  ce <- coef(fit, ...)
  coef_mat <- as.matrix(ce)
  tibble::tibble(name = rownames(coef_mat), coef = coef_mat[, 1]) %>%
    dplyr::filter(abs(coef) >= min_coef) %>%
    dplyr::arrange(dplyr::desc(coef))
}

# If var_names is not supplied then this is just coef_to_table_simple().
coef_to_table <- function(fit, var_names = NULL, ..., min_coef=1E-10) {
  tbl <- coef_to_table_simple(fit = fit, ..., min_coef = min_coef)
  if (is.null(var_names)) return(tbl)
  n_vars <- max(stringr::str_count(tbl$name, ":")) + 1
  if (n_vars == 1){
    tbl <- tbl %>%
      dplyr::mutate(var = match_parent(.data$name, var_names)) %>%
      dplyr::mutate(level = extract_level(.data$name, .data$var))
    return(tbl)
  }
  nms <- sprintf("name%02d", 1 : n_vars)
  tbl2 <- select(tbl, name) %>%
    tidyr::separate(.data$name, nms, sep = ":", remove = FALSE, fill = "right")
  names_list <- map(2 : ncol(tbl2), ~select(tbl2, .))
  bind_cols(tbl, map_dfc(names_list, ~expand_interaction(., var_names))) %>%
    select(name, coef, contains("interact"), everything())
}

test_split <- function(fit, var_names, s="lambda.1se", min_coef=1E-10) {
  tbl <- coef_to_table_simple(fit = fit, s = s, min_coef = min_coef)
  n_vars <- max(stringr::str_count(tbl$name, ":")) + 1
  if (n_vars == 1){
    tbl <- tbl %>%
      dplyr::mutate(var = match_parent(.data$name, var_names)) %>%
      dplyr::mutate(level = extract_level(.data$name, .data$var))
    return(tbl)
  }
  nms <- sprintf("name%02d", 1 : n_vars)
  tbl2 <- select(tbl, name) %>%
    tidyr::separate(.data$name, nms, sep = ":", remove = FALSE, fill = "right")
  names_list <- map(2 : ncol(tbl2), ~select(tbl2, .))
  bind_cols(tbl, map_dfc(names_list, ~expand_interaction(., var_names))) %>%
    select(name, coef, contains("interact"), everything())
}
debugonce(coef_to_table)
coef_to_table(fit, var_names = names(dt), s = 10 ^ -2)

test_split(fit0, var_names = names(dt), s = 10 ^ -2)
sprintf("name%02d", 1:3)


nms <- paste0("name", 1 : 3)
 x2 <- select(x, name) %>%
  tidyr::separate(.data$name, nms, sep = ":", remove = FALSE, fill = "right")
map(2 : ncol(x2), ~x2[, .])
x_list <- map(2 : ncol(x2), ~select(x2, .))
expand_interaction(x_list[[1]], names(dt))

expand_interaction <- function(tbl, var_names) {
  id <- str_extract(names(tbl), "\\d+$")
#  names(tbl) <- str_remove_all(names(tbl), "\\d*")
  names(tbl) <- "interact"
  tbl %>%
    dplyr::mutate(var = match_parent(.data$interact, var_names)) %>%
    dplyr::mutate(level = extract_level(.data$interact, .data$var)) %>%
    setNames(., paste0(names(.), "_", id))
}
paste0(names(x), "555")

?setNames
# explore glmnet fit object---------
names(fit)
fit$lambda %>% head
fit$call
fit$nzero
fit$name
fit$glmnet.fit %>% names
fit$glmnet.fit$lambda %>% length()
fit$glmnet.fit$classnames
fit$glmnet.fit$beta %>% head
fit$glmnet.fit$beta %>% dim # 20 rows (vars) x 100 cols (lambda)

fit$glmnet.fit$a0 %>% head
fit$glmnet.fit$call
fit$glmnet.fit$beta %>% str

coef(fit) %>% as.matrix() #coefs at s = "lanmbda.1se" (default s)
glmnet_to_table(fit, names(dtin), s = min(fit$lambda), min_coef = 0) %>% prinf # 2 intercepts??

fit2 <- glmnet::cv.glmnet(x=x_mat, y = dt$top, family="binomial")
names(fit2)

# all lambda models
xmat <- model.matrix(top ~ ., dt)[, -1]
set.seed(22)
fit <- cv.glmnet(x=xmat, y = dt$top, family="binomial")
plot(fit)

beta_mat <- as.matrix(fit$glmnet.fit$beta)
tb <- beta_mat %>% t() %>% as_tibble() %>%
  mutate(s = fit$lambda) %>%
  mutate(intercept = fit$glmnet.fit$a0) %>%
  mutate(nzero = fit$nzero) %>%
  mutate(dev = fit$cvm) %>%
  select(s, nzero, dev, intercept, everything())
tb
view(tb)
names(fit)
fit$cvm %>% head
fit$cvup %>% head
fit$cvsd %>% head
fit$glmnet.fit$dev.ratio
fit$glmnet.fit$nulldev
log(s_all) %>% head

plot(x = log(fit$lambda), y = fit$cvm, typ = 'l')

rownames(beta_mat)
inter <- fit$glmnet.fit$a0

tb %>%
  select(-nzero, -dev, -intercept) %>%
  pivot_longer(-s) %>%
  ggplot(aes(log(s), value, colour = name, group = name)) +
  geom_line()

# nzero vs dev
# only plot best dev for each nzero
tb %>%
  select(nzero, dev) %>%
  group_by(nzero) %>%
  filter(dev == min(dev)) %>%
  ggplot(aes(nzero, dev)) +
  geom_line()

# min s to be included
s_all <- sort(fit$lambda)
level_names <- rownames(as.matrix(coef(fit, s="lambda.1se")))

#ns <- length(s_all)
max_include <- numeric(length(level_names))
for (s in s_all){
  ce <- as.vector(coef(fit, s=s))
  max_include[abs(ce) > 0] <- s
}
ranks <- tibble(var = level_names, max_s_include = max_include, rank = row_number(desc(max_s_include)))
arrange(ranks, rank) %>% prinf

as.vector(coef(fit, s=s))

glmnet(x=xmat, y = dt$top, family="binomial") %>% plot(label = T)

# glmnet assessments
set.seed(23)
test <- ggplot2::diamonds %>%
  dplyr::mutate(top = (cut == "Ideal") %>% factor(levels = c(F, T))) %>%
  dplyr::select(-cut) %>%
  dplyr::sample_n(500) %>%
  dplyr::mutate(clarity = factor(clarity, ordered = F))
testx <- model.matrix(top ~ . * table, test)[, -1]
ass <- assess.glmnet(fit, newx = testx, newy = test$top)

roc <- roc.glmnet(fit, newx = testx, newy = test$top)
plot(roc)
confusion(fit, newx = testx, newy = test$top)

# coef ------
coef_to_table <- function(fit, ..., min_coef=1E-10) {
  ce <- coef(fit, ...)
  coef_mat <- as.matrix(ce)
  tibble::tibble(name = rownames(coef_mat), coef = coef_mat[, 1]) %>%
    dplyr::filter(abs(coef) >= min_coef) %>%
    dplyr::arrange(dplyr::desc(coef))
}

mt <- as_tibble(mtcars) %>%
  mutate(across(c(cyl, vs, am, gear, carb), ~factor(., ordered = TRUE)))
mt
head(mtcars)
library(glmnet)
xmt <- model.matrix(mpg ~ ., data = mt)[, -1]
lm <- lm(mpg ~ ., data = mtcars)
glm <- cv.glmnet(xmt, mtcars$mpg, nfolds = 3)
library(dplyr)
coef_to_table(lm)
coef_to_table(glm, s = "lambda.min")
glmnet_to_table(glm, s = "lambda.min", var_names = names(mt))
coef(glm, s = "lambda.min")

coef(glm) %>% as.matrix()

# dummy var naming--------
#See recipes::dummy_names()
# base::make.names() forces valid names. Only chars, numbers, ., and _ are allowed.
# Any restricted character is replaced by .

data.frame(`a^b` = 1:3)
?data.frame

# model.matrix colnames may not be unique e.g. see code
# In this case, it is impossible to retrieve var & level correctly.b
n = 20
f1 <- letters[1:5]
f2 <- paste0("z", letters[1:5])
df <- tibble(y = rnorm(n), xz = factor(sample(f1, n, replace = T)),
             x = factor(sample(f2, n, replace = T)))
xmat <- model.matrix(y ~., data = df)
one_hot(y~. , data = df)
?one_hot
lm <- lm(y~., data = df)
coef_to_table(lm) %>% arrange(name)
?model.matrix
