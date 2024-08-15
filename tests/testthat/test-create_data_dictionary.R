
library(dplyr)

test_that("create_data_dictionary function works correctly", {

  # Create a simple data frame for testing
  test_data <- data.frame(
    a = 1:5,
    b = letters[1:5],
    c = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  # Define the expected output
  expected_output <- tibble::tibble(
    variable = c("a", "b", "c"),
    name = NA_character_,
    type = c("integer", "character", "logical"),
    description = NA_character_
  )

  # Use the function to create a data dictionary
  result <- create_data_dictionary(test_data, tempfile())

  # Check if the result matches the expected output
  expect_equal(result, expected_output, ignore_attr = TRUE)
})
