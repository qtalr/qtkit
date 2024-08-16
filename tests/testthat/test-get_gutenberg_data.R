library(dplyr)

test_that("get_gutenberg_data function works correctly", {
  # Relies on internet connection
  skip_on_cran()
  # Create a temporary directory for testing
  temp_dir <- tempdir()
# Define the parameters for the function
  lcc_subject <- "JC"
  n_works <- 5
  confirmed <- TRUE
  # Call the function with the test parameters
  set.seed(123)
  get_gutenberg_data(
    target_dir = temp_dir,
    lcc_subject = lcc_subject,
    n_works = n_works,
    confirmed = confirmed
  )
  # Define the expected output file path
  expected_file <-
    file.path(temp_dir, paste0("works_", tolower(lcc_subject), ".csv"))
  # Check if the output file was created
  expect_true(file.exists(expected_file))
  # Read the output file
  output_data <- read.csv(expected_file)
  # Check if the output data has the expected number of rows
  expect_equal(length(unique(output_data$gutenberg_id)), n_works)
  # Check if the output data has the expected columns
  expected_columns <-
    c("gutenberg_id",
      "lcc",
      "gutenberg_bookshelf",
      "gutenberg_author_id",
      "author",
      "title",
      "text")
  expect_equal(colnames(output_data), expected_columns)
  file.remove(expected_file)
})
