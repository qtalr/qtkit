# Test: curate_enntt_data

test_that("curate_enntt_data handles valid input correctly", {
  # Setup test data path
  test_dir <- system.file("extdata", "simul_enntt", package = "qtkit")

  # Run function
  result <- curate_enntt_data(test_dir)

  # Test structure
  expect_s3_class(result, "data.frame")
  expect_named(result, c("session_id", "speaker_id", "state", "session_seq", "text", "type"))

  # Test content
  expect_type(result$session_id, "character")
  expect_type(result$speaker_id, "character")
  expect_type(result$state, "character")
  expect_type(result$session_seq, "character")
  expect_type(result$text, "character")
  expect_type(result$type, "character")

  # Test for non-empty result
  expect_gt(nrow(result), 0)
})

test_that("curate_enntt_data fails gracefully with invalid directory", {
  expect_error(
    curate_enntt_data("nonexistent_directory"),
    "Directory does not exist:"
  )
})

test_that("curate_enntt_data fails gracefully with empty directory", {
  # Create temporary empty directory
  temp_dir <- tempdir()
  on.exit(unlink(temp_dir, recursive = TRUE))

  expect_error(
    curate_enntt_data(temp_dir),
    "No .dat or .tok files found in directory"
  )
})

test_that("curate_enntt_data handles mismatched dat/tok files", {
  # Setup test directory with mismatched files
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "mismatched")
  dir.create(test_dir, showWarnings = FALSE)
  on.exit(unlink(test_dir, recursive = TRUE))

  # Create sample mismatched files
  writeLines(
    "<?xml version='1.0' encoding='UTF-8'?><root><line session_id='1' mepid='123' state='FR' seq_speaker_id='1'></line></root>",
    file.path(temp_dir, "mismatched", "test.dat")
  )
  writeLines(
    c("text1", "text2"), # Two lines instead of one
    file.path(temp_dir, "mismatched", "test.tok")
  )

  expect_error(
    curate_enntt_data(file.path(temp_dir, "mismatched")),
    "Mismatched lengths between DAT and TOK files"
  )
})

# Helper function to create valid test files
create_valid_test_files <- function(dir_path) {
  dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)

  # Create valid .dat file
  writeLines(
    "<line session_id='1' mepid='123' state='FR' seq_speaker_id='1'></line>",
    file.path(dir_path, "test.dat")
  )

  # Create matching .tok file
  writeLines(
    "Sample text content",
    file.path(dir_path, "test.tok")
  )
}

test_that("curate_enntt_data processes valid files correctly", {
  # Setup test directory with valid files
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "valid")
  create_valid_test_files(test_dir)
  on.exit(unlink(test_dir, recursive = TRUE))

  result <- curate_enntt_data(test_dir)

  expect_equal(nrow(result), 1)
  expect_equal(result$session_id, "1")
  expect_equal(result$speaker_id, "123")
  expect_equal(result$state, "FR")
  expect_equal(result$session_seq, "1")
  expect_equal(result$text, "Sample text content")
  expect_equal(result$type, "test")
})
