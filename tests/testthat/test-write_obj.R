library(testthat)

test_that("saves an object to a file", {
  # Create a sample object to save
  l <- list(a = 1, b = 2)

  # Specify the file name and target directory
  file <- "test_object.R"
  target_dir <- tempdir()

  # Call the write_obj function to save the object
  write_obj(l, file = file, target_dir = target_dir)

  # Check that the file exists
  expect_true(file.exists(file.path(target_dir, file)))

  # Load the saved object from the file
  loaded_obj <- dget(file.path(target_dir, file))

  # Check that the loaded object is the same as the original object
  expect_equal(l, loaded_obj)
})

test_that("throws an error if the file is not specified", {
  # Create a sample object to save
  l <- list(a = 1, b = 2)

  # Specify the target directory
  target_dir <- tempdir()

  # Expect an error to be thrown when the write_obj function is called
  # with a NULL file argument
  expect_error(write_obj(l, file = NULL, target_dir = target_dir))
})

test_that("uses the block label as the file name if it is specified", {
  # Create a sample object to save
  l <- list(a = 1, b = 2)

  # Specify the block label and target directory
  block_label <- "test_label"
  target_dir <- tempdir()

  # Set the block label option to the specified value
  knitr::opts_current$set(label = block_label)

  # Call the write_obj function to save the object
  write_obj(l, target_dir = target_dir)

  # Check that the file was saved with the block label as the file name
  expect_true(file.exists(file.path(target_dir, block_label)))
})

# Test that the function creates the target directory if it does not exist
test_that("creates the target directory if it does not exist", {
  # Create a sample object to save
  l <- list(a = 1, b = 2)

  # Specify the file name and target directory
  file <- "test_object.R"
  target_dir <- file.path(tempdir(), "non_existent_dir")

  # Call the write_obj function to save the object
  write_obj(l, file = file, target_dir = target_dir)

  # Check that the target directory was created
  expect_true(dir.exists(target_dir))

  # Check that the file was saved in the target directory
  expect_true(file.exists(file.path(target_dir, file)))
})
