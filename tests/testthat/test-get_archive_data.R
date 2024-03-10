# Load the testthat library
library(testthat)

# Create a temporary directory for the test
temp_dir <- tempdir()

# Define a test for the case where the target directory does not exist
test_that("downloads and extracts the archive file when the target directory does not exist", {
  # Define the URL and target directory for the test
  url <- "https://raw.githubusercontent.com/qtalr/qtkit/main/inst/extdata/test_data.zip"

  # Create the target directory path
  target_dir <- file.path(temp_dir, "test_data")

  # Call the get_archive_data function with the test URL and target directory
  get_archive_data(url = url, target_dir = target_dir, confirmed = TRUE)

  # Check that the target directory exists
  expect_true(dir.exists(target_dir))

  # Check that the extracted files are present in the target directory
  expect_true(file.exists(file.path(target_dir, "file1.txt")))
  expect_true(file.exists(file.path(target_dir, "file2.txt")))

  # Remove the target directory
  unlink(target_dir, recursive = TRUE, force = TRUE)

})



# Define a test for the case where the target directory already exists and the force argument is FALSE
test_that("does not download or extract the archive file when the target directory exists and force is FALSE", {
  # Define the URL and target directory for the test
  url <- "https://raw.githubusercontent.com/qtalr/qtkit/main/inst/extdata/test_data.zip"

  # Create the target directory path
  target_dir <- file.path(temp_dir, "test_data")

  # Create the target directory for the test
  dir.create(target_dir, showWarnings = FALSE)

  # Call the get_archive_data function with the test URL and target directory
  get_archive_data(url = url, target_dir = target_dir, force = FALSE, confirmed = TRUE)

  # Check that the extracted files are not present in the target directory
  expect_false(file.exists(file.path(target_dir, "file1.txt")))
  expect_false(file.exists(file.path(target_dir, "file2.txt")))

  # Remove the target directory
  unlink(target_dir)
})


# Define a test for the case where the target directory already exists and the force argument is TRUE
test_that("downloads and extracts the archive file when the target directory exists and force is TRUE", {
  # Define the URL and target directory for the test
  url <- "https://raw.githubusercontent.com/qtalr/qtkit/main/inst/extdata/test_data.zip"

  # Create the target directory path
  target_dir <- file.path(temp_dir, "test_data")

  # Create the target directory for the test
  dir.create(target_dir, showWarnings = FALSE)

  # Call the get_archive_data function with the test URL and target directory
  get_archive_data(url = url, target_dir = target_dir, force = TRUE, confirmed = TRUE)

  # Check that the extracted files are present in the target directory
  expect_true(file.exists(file.path(target_dir, "file1.txt")))
  expect_true(file.exists(file.path(target_dir, "file2.txt")))
})
