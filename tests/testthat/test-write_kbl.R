library(testthat)
library(knitr)
library(kableExtra)

with_chromium_available <- function(code) {
  skip_if_not(is_chromium_available(), "Chromium not available")
  force(code)
}

test_that("write_kbl saves the kable correctly", {
  with_chromium_available({
    # Create a temporary directory for testing
    temp_dir <- tempdir()

    # Create a kable object
    t <- knitr::kable(mtcars[1:5, ], format = "html")

    # Test saving as .pdf
    write_kbl(t, "test_kable", temp_dir, "pdf")
    expect_true(file.exists(file.path(temp_dir, "test_kable.pdf")))

    # Test saving as .html
    write_kbl(t, "test_kable", temp_dir, "html")
    expect_true(file.exists(file.path(temp_dir, "test_kable.html")))

    # Test saving as .tex
    write_kbl(t, "test_kable", temp_dir, "latex")
    expect_true(file.exists(file.path(temp_dir, "test_kable.tex")))

    # Test saving as .png
    write_kbl(t, "test_kable", temp_dir, "png")
    expect_true(file.exists(file.path(temp_dir, "test_kable.png")))

    # Test saving as .jpeg
    write_kbl(t, "test_kable", temp_dir, "jpeg")
    expect_true(file.exists(file.path(temp_dir, "test_kable.jpeg")))

    # Test that write_kbl works correctly when block label is specified
    knitr::opts_current$set(label = "test_label")
    write_kbl(t, target_dir = temp_dir)
    expect_true(file.exists(file.path(temp_dir, "test_label.pdf")))
    knitr::opts_current$set(label = NULL)
  })
})
