# Test: calc_type_metrics

# Load the data
data_path <- system.file("extdata", "types_data.rds", package = "qtkit")

data <- readRDS(data_path)

test_that("returns expected output", {
  result <- calc_type_metrics(
    data = data,
    type = letter,
    document = doc_id,
    frequency = c("rf", "orf"),
    dispersion = c("df", "idf", "dp")
  )
  # Add your expectations here. For example:
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 5) # Assuming there are 5 unique types in the data
  expect_equal(ncol(result), 7) # type, n, rf, orf, df, idf, dp
})

test_that("handles invalid inputs", {
  expect_error(calc_type_metrics(data, "nonexistent", document))
  expect_error(calc_type_metrics(data, letters, "nonexistent"))
  expect_error(calc_type_metrics("not a data frame", letters, doc_id))
  expect_error(calc_type_metrics(data, letters, doc_id, "invalid"))
  expect_error(calc_type_metrics(data, letters, doc_id, dispersion = "invalid"))
})
