set.seed(21)
dt <- ggplot2::diamonds %>%
  dplyr::mutate(top = ifelse(cut == "Ideal", 1, 0) %>% factor(levels = c(1, 0))) %>%
  dplyr::sample_n(100)

dt2 <- dplyr::mutate(dt, top = (cut == "Ideal") %>% factor(levels = c(FALSE, TRUE))) %>%
  dplyr::select(-cut)
dt <- dplyr::select(dt, -cut)

test_that("rang_oob_error() works", {
  rf <- ranger::ranger(top ~ . , dt, seed = 20, keep.inbag = TRUE, num.trees = 200)
  rf2 <- ranger::ranger(top ~ . , dt, seed = 20, keep.inbag = FALSE, num.trees = 20)
  expect_error(rang_oob_err(rf2, dt, plot = FALSE), "Must use `keep.inbag = TRUE`")
#  expect_known_hash(rang_oob_err(rf, dt, plot = FALSE), hash = "5cb256f2ca")
  expect_snapshot_value(rang_oob_err(rf, dt, plot = FALSE), style = "serialize")
})

test_that("rang_roc_cut() and roc_cut() work", {
  rf1 <- ranger::ranger(top ~ . , dt, seed = 20, num.trees = 100, probability = TRUE)
  rf2 <- ranger::ranger(top ~ . , dt, seed = 20, num.trees = 100, probability = FALSE)
  rf3 <- ranger::ranger(top ~ . , dt2, seed = 20, num.trees = 100, probability = TRUE)
  out <- c(sensitivity = 0.8837209, specificity = 0.8947368, cutoff = 0.4741270, auc = 0.8816809)
  expect_message(cut1 <- rang_roc_cut(rf1, dt$top, plot = FALSE), "Using 1 as positive class")
  expect_message(cut2 <- rang_roc_cut(rf3, dt$top, plot = FALSE), "Using TRUE as positive class")
  expect_equal(cut1, out, tolerance = 1e-6)
  expect_equal(cut2, out, tolerance = 1e-6)
  expect_equal(roc_cut(rf1$predictions[, 1], dt$top, plot = FALSE),
               out,
               tolerance = 1e-6)
  expect_error(rang_roc_cut(rf2, dt$top, plot = FALSE), "matrix")
})
