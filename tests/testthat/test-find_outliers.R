
# Tests for find_outliers()
test_that("Finds outlier(s) correctly", {
  test_data <- data.frame(x = c(1, 2, 3, 4, 5, 100))
  result <- find_outliers(test_data, x)
  expect_equal(nrow(result), 1)
  expect_equal(result$x, 100)
})

test_that("Returns message when no outliers found", {
  test_data <- data.frame(x = c(1, 2, 3, 4, 5))
  expect_message(find_outliers(test_data, x), "No outliers found.")
})

test_that("Throws an error when the first argument is not a data.frame", {
  expect_error(find_outliers(1, x), "The first argument must be a data.frame.")
})

test_that("Throws an error when the second argument is missing", {
  test_data <- data.frame(x = c(1, 2, 3, 4, 5))
  expect_error(find_outliers(test_data), "The second argument must be unquoted.") # nolint
})

test_that("Throws an error when the second argument is not a symbol", {
  test_data <- data.frame(x = c(1, 2, 3, 4, 5))
  expect_error(find_outliers(test_data, "x"), "The second argument must be a symbol.") # nolint
})

test_that("Throws an error when the second argument is not a variable in the data", {
  test_data <- data.frame(x = c(1, 2, 3, 4, 5))
  expect_error(find_outliers(test_data, y), "The second argument must be a variable in the data.") # nolint
})
