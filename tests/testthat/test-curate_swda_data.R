# Test: curate_swda_data

test_that("curate_swda_data handles valid input correctly", {
  # Use the simulated data directory
  test_dir <- system.file("extdata", "simul_swda", package = "qtkit")

  # Run the function
  result <- curate_swda_data(test_dir)

  # Check the structure of the returned data
  expect_s3_class(result, "data.frame")
  expect_named(
    result,
    c("doc_id", "damsl_tag", "speaker_id", "speaker", "turn_num", "utterance_num", "utterance_text")
  )

  # Check data types of specific columns
  expect_type(result$doc_id, "character")
  expect_type(result$damsl_tag, "character")
  expect_type(result$speaker_id, "character")
  expect_type(result$speaker, "character")
  expect_type(result$turn_num, "character") # Turn numbers are read as character initially
  expect_type(result$utterance_num, "character")
  expect_type(result$utterance_text, "character")

  # Check for non-empty result
  expect_gt(nrow(result), 0)

  # Check specific values from the simul_swda files
  expect_equal(result$doc_id[1], "4325")
  expect_equal(result$speaker_id[1], "1632")
  expect_match(result$utterance_text[1], "Meep.  /")
})


test_that("curate_swda_data fails gracefully with invalid directory", {
  # Test with a non-existent directory
  expect_error(
    curate_swda_data("nonexistent_directory"),
    "Directory does not exist: nonexistent_directory" # Check for the specific error message
  )
})

test_that("curate_swda_data handles empty directory", {
  # Create a temporary empty directory
  temp_dir <- tempdir()
  dir.create(temp_dir)
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Expect a warning and an empty data frame
  expect_warning(
    result <- curate_swda_data(temp_dir),
    "No .utt files found in directory"
  )
  expect_equal(nrow(result), 0)
  expect_s3_class(result, "data.frame") # Ensure an empty data frame is returned
})


test_that("curate_swda_data handles malformed .utt file", {
  # Create a temporary directory and a malformed .utt file
  temp_dir <- tempdir()
  dir.create(temp_dir)
  malformed_file <- file.path(temp_dir, "malformed.utt")
  writeLines("This is a malformed file", malformed_file) # Missing speaker info and utterances
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Expect an error
  expect_error(
    curate_swda_data(temp_dir),
    "Could not find speaker information in file"
  )
})


test_that("curate_swda_data handles partially malformed .utt file (missing speaker info)", {
  # Create a temporary directory and a malformed .utt file (missing speaker info)
  temp_dir <- tempdir()
  dir.create(temp_dir)
  malformed_file <- file.path(temp_dir, "malformed.utt")
  writeLines(c("*A: Hello", "*B: Hi"), malformed_file) # Missing speaker info line
  on.exit(unlink(temp_dir, recursive = TRUE))

  # Expect an error
  expect_error(
    curate_swda_data(temp_dir),
    "Could not find speaker information in file"
  )
})
