
# Test get_talkbank_data --------------------------------------

# Create a temporary directory for the test
temp_dir <- tempdir()

# Create variables
valid_name <- "ca"
valid_path <- c("ca", "Nahuatl")
valid_dir <- file.path(temp_dir, "data")

invalid_name <- "nonexistent_corpus"
invalid_path <- c("nonexistent_corpus", "nonexistent_language")
invalid_dir <- "nonexistent_directory"

# Helper functions
# validate_inputs_gtd ------

# valid inputs
with_mock_api({
  test_that("valid inputs", {
    # Test with the mocked API response
    expect_silent(validate_inputs_gtd(
      valid_name, valid_path, valid_dir, FALSE, TRUE))
  })
})

# invalid corpus name
with_mock_api({
  test_that("invalid corpus name", {
    expect_error(validate_inputs_gtd(
      invalid_name, valid_path, valid_dir, FALSE, TRUE))
  })
})

# invalid corpus path
with_mock_api({
  test_that("invalid corpus path", {
    expect_error(validate_inputs_gtd(
      valid_name, invalid_path, valid_dir, FALSE, TRUE))
  })
})

# invalid boolean force
with_mock_api({
  test_that("invalid boolean force", {
    expect_error(validate_inputs_gtd(
      valid_name, valid_path, valid_dir, NULL, TRUE))
  })
})

# invalid boolean confirmed
with_mock_api({
  test_that("invalid boolean confirmed", {
    expect_error(validate_inputs_gtd(
      valid_name, valid_path, valid_dir, FALSE, NULL))
  })
})

# write_to_disk function ------
# Mock functions and variables
mock_func_success <- function(corpus_name, corpus_path) {
  return(data.frame(a = 1, b = 2))
}

mock_func_timeout <- function(corpus_name, corpus_path) {
  start_time <- Sys.time()
  print("Starting timeout test\n")
  while (as.numeric(Sys.time() - start_time, units = "secs") < 3) {
    # Busy-wait for 3 seconds
  }
  return(data.frame(a = 1, b = 2))
}

mock_func_error <- function(corpus_name, corpus_path) {
  stop("mock error")
}

# Tests
test_that("write_to_disk successfully writes data to disk", {
  target_dir <- file.path(temp_dir, "test_data")
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  file_path <- file.path(target_dir, "test.csv")
  result <-
    write_to_disk(
      mock_func_success,
      corpus_name,
      corpus_path,
      file_path,
      timeout = 5)
  expect_null(result)
  expect_true(file.exists(file_path))
  expect_equal(read.csv(file_path), data.frame(a = 1, b = 2))
  unlink(target_dir, recursive = TRUE, force = TRUE)
})

test_that("write_to_disk handles timeout correctly", {
  # Relies on internet connection
  skip_on_cran()
  corpus_name <- "ca"
  corpus_path <- c("ca", "CABNC")
  file_path <- file.path(temp_dir, "test.csv")
  result <-
    write_to_disk(getTokens, corpus_name, corpus_path, file_path, timeout = 1)
  expect_null(result)
  expect_message(write_to_disk(
    getTokens, corpus_name, corpus_path, file_path, timeout = 1),
    regex = "timed out at 1 seconds")
})

test_that("write_to_disk does not write to disk on function error", {
  corpus_name <- "ca"
  corpus_path <- c("ca", "CABNC")
  file_path <- file.path(temp_dir, "test.csv")
  write_to_disk(
    mock_func_error, corpus_name, corpus_path, file_path, timeout = 5)
  expect_false(file.exists(file_path))
})

test_that("write_to_disk handles empty input data correctly", {
  corpus_name <- "ca"
  corpus_path <- c("ca", "CABNC")
  file_path <- file.path(temp_dir, "test.csv")
  mock_func_empty <- function(corpus_name, corpus_path) {
    return(data.frame())
  }
  result <-
    write_to_disk(
      mock_func_empty, corpus_name, corpus_path, file_path, timeout = 5)
  expect_true(is.null(result))
  expect_true(file.exists(file_path))
  expect_error(read.csv(file_path), "no lines available in input")
  file.remove(file_path)
})

# Clean up
# Remove the temporary directory
unlink(temp_dir)
