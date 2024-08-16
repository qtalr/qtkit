
# Load the data
data_path <- system.file("extdata", "bigrams_data.rds", package = "qtkit")
data <- readRDS(data_path)

test_that("calc_assoc_metrics handles invalid inputs", {
  expect_error(calc_assoc_metrics(data, "nonexistent", "token_index", "type"))
  expect_error(calc_assoc_metrics(data, "doc_index", "nonexistent", "type"))
  expect_error(calc_assoc_metrics(data, "doc_index", "token_index", "nonexistent"))
  expect_error(calc_assoc_metrics(data, "doc_index", "token_index", "type", "invalid"))
})

test_that("calc_assoc_metrics calculates metrics correctly", {
  data <- data.frame(
    doc_index = c(1, 1, 1, 2),
    token_index = c(1, 2, 3, 1),
    type = c("word1", "word2", "word3", "word2")
  )
  result <- calc_assoc_metrics(data, "doc_index", "token_index", "type", "all", TRUE)
  expect_equal(nrow(result), 6)
  expect_equal(ncol(result), 8)
  expect_equal(colnames(result), c("y", "x", "p_xy", "p_x", "p_y", "pmi", "dice_coeff", "g_score"))
})

