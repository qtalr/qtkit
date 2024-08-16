
# Test get_archive_data() ------------------------------------------

# Create a temporary directory for the test
temp_dir <- tempdir()

# Define the URL and target directory for the test
url <- "https://raw.githubusercontent.com/qtalr/qtkit/main/inst/extdata/test_data.zip"

# Define a test for the case where the target directory does not exist
test_that("do, when the target directory does not exist", {
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

test_that("do not, when the target directory exists and force is FALSE", {

  # Create the target directory path
  target_dir <- file.path(temp_dir, "test_data")

  # Create the target directory for the test
  dir.create(target_dir, showWarnings = FALSE)

  # Call the get_archive_data function with the test URL and target directory
  get_archive_data(
    url = url,
    target_dir = target_dir,
    force = FALSE,
    confirmed = TRUE
  )

  # Check that the extracted files are not present in the target directory
  expect_false(file.exists(file.path(target_dir, "file1.txt")))
  expect_false(file.exists(file.path(target_dir, "file2.txt")))

  # Remove the target directory
  unlink(target_dir)
})

test_that("do, when the target directory exists and force is TRUE", {

  # Create the target directory path
  target_dir <- file.path(temp_dir, "test_data")

  # Create the target directory for the test
  dir.create(target_dir, showWarnings = FALSE)

  # Call the get_archive_data function with the test URL and target directory
  get_archive_data(
    url = url,
    target_dir = target_dir,
    force = TRUE,
    confirmed = TRUE
  )

  # Check that the extracted files are present in the target directory
  expect_true(file.exists(file.path(target_dir, "file1.txt")))
  expect_true(file.exists(file.path(target_dir, "file2.txt")))
})

# Remove the temporary directory
unlink(temp_dir)

# Test clean_filenames() ------------------------------------------

# Create a temporary directory for the test
temp_dir <- tempdir()

test_that("cleans filenames in the target directory", {
  # Create a test directory with a file that has a space in the name
  test_dir <- file.path(temp_dir, "test_dir")
  dir.create(test_dir, showWarnings = FALSE)
  file.create(file.path(test_dir, "file with space.txt"))

  # Call the clean_filenames function with the test directory
  clean_filenames(test_dir)

  # Check that the file with a space in the name has been renamed
  expect_true(file.exists(file.path(test_dir, "file_with_space.txt")))

  # Remove the test directory
  unlink(test_dir, recursive = TRUE, force = TRUE)
})

# Test validate_file_extension() ------------------------------------

test_that("validate_file_extension returns an error for unsupported", {
  unsupported_extensions <- c("txt", "csv", "rar")

  for (ext in unsupported_extensions) {
    expect_error(
      validate_file_extension(ext),
      "Target file given is not supported"
    )
  }
})

test_that("validate_file_extension does not return an error for supported", {
  supported_extensions <- c("zip", "gz", "tar", "tgz")

  for (ext in supported_extensions) {
    expect_silent(validate_file_extension(ext))
  }
})
