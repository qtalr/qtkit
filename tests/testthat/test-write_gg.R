library(testthat)
library(ggplot2)

test_that("write_gg saves the plot correctly", {
  # Create a temporary directory for testing
  temp_dir <- tempdir()

  p <- ggplot(mtcars, aes(mpg, disp)) +
    geom_point()

  # Test saving as .pdf
  write_gg(p, "test_plot", temp_dir, "pdf")
  expect_true(file.exists(file.path(temp_dir, "test_plot.pdf")))

  # Test saving as .png
  write_gg(p, "test_plot", temp_dir, "png")
  expect_true(file.exists(file.path(temp_dir, "test_plot.png")))

  # Test saving as .jpeg
  write_gg(p, "test_plot", temp_dir, "jpeg")
  expect_true(file.exists(file.path(temp_dir, "test_plot.jpeg")))

  # Test saving as .tiff
  write_gg(p, "test_plot", temp_dir, "tiff")
  expect_true(file.exists(file.path(temp_dir, "test_plot.tiff")))

  # Test saving as .svg
  write_gg(p, "test_plot", temp_dir, "svg")
  expect_true(file.exists(file.path(temp_dir, "test_plot.svg")))

  # Test that the function returns the correct file path
  returned_path <- write_gg(p, "test_plot", temp_dir, "pdf")
  expect_equal(returned_path, file.path(temp_dir, "test_plot.pdf"))

  # Test that the function creates the directory if it does not exist
  new_dir <- file.path(temp_dir, "new_dir")
  write_gg(p, "test_plot", new_dir, "pdf")
  expect_true(dir.exists(new_dir))

  # Test that write_gg works correctly when block label is specified
  knitr::opts_current$set(label = "test_label")
  write_gg(p, target_dir = temp_dir)
  expect_true(file.exists(file.path(temp_dir, "test_label.pdf")))
  knitr::opts_current$set(label = NULL)

  # Test that write_gg works correctly when plot is not specified
  write_gg(file = "last_plot", target_dir = temp_dir, device = "pdf")
  expect_true(file.exists(file.path(temp_dir, "last_plot.pdf")))
})
