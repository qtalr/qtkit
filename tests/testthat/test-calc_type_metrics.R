# Test: calc_type_metrics

# Load the data
data_path <- system.file("extdata", "types_data.rds", package = "qtkit")
data <- readRDS(data_path)

test_that("calc_type_metrics returns expected output", {
  result <- calc_type_metrics(
    data = data,
    type = type,
    document = document,
    frequency = c("rf", "orf"),
    dispersion = c("df", "idf", "dp")
  )
  # Add your expectations here. For example:
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 5) # Assuming there are 5 unique types in the data
  expect_equal(ncol(result), 7) # type, n, rf, orf, df, idf, dp
})

test_that("calc_type_metrics handles invalid inputs", {
  expect_error(calc_type_metrics(data, "nonexistent", document))
  expect_error(calc_type_metrics(data, type, "nonexistent"))
  expect_error(calc_type_metrics("not a data frame", type, document))
  expect_error(calc_type_metrics(data, type, document, "invalid"))
  expect_error(calc_type_metrics(data, type, document, dispersion = "invalid"))
})
