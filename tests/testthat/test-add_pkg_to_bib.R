
test_that("add_pkg_to_bib writes citations to a bib file", {
  # Create a temporary file for testing
  temp_file <- tempfile(fileext = ".bib")

  # Call add_pkg_to_bib function with pkg_name as "knitr" and
  # bib_file as temp_file
  add_pkg_to_bib("ggplot2", temp_file)

  # Read the content of bib file
  bib_content <- readLines(temp_file)

  # Test that the bib file is not empty
  expect_true(length(bib_content) > 0)

  # Test that the bib file contains citation for the "knitr" package
  expect_true(table(grepl("R-ggplot2", bib_content))[2] == 1)

  # Delete the temporary file
  unlink(temp_file)
})
