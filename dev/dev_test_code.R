
# ranger  -----------

set.seed(21)
dt <- ggplot2::diamonds %>%
  dplyr::mutate(top = ifelse(cut == "Ideal", 1, 0) %>% factor(levels = c(1, 0))) %>%
  dplyr::select(-cut) %>%
  dplyr::sample_n(100)

rf <- ranger::ranger(top ~ . , dt, seed = 20, keep.inbag = T, num.trees = 1000)
rf2 <- ranger::ranger(top ~ . , dt, seed = 20, keep.inbag = F, num.trees = 1000)

rang_oob_err(rf, dt)
rang_mtry(dt, top ~ ., 1:8, num.trees = 100)

dt[1, 2] <- NA
count_nas2(dt, all = T)
count_matches2(dt, c("F", "I1"), all = T, sort = T)
