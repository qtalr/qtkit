# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(qtkit)
library(httptest)
library(chromote)

# Function to delete Crashpad directories
cleanup_crashpad <- function() {
  crashpad_dirs <- dir(tempdir(), full.names = TRUE, pattern = "Crashpad")
  if (length(crashpad_dirs) > 0) {
    unlink(crashpad_dirs, recursive = TRUE)
  }
}

testthat::setup(cleanup_crashpad)
testthat::teardown(cleanup_crashpad)

# Function to check if Chromium is
# available and can be initialized
is_chromium_available <- function() {
  tryCatch({
    chromote::Chromote$new()
    TRUE
  }, error = function(e) {
    FALSE
  })
}

# Set the timeout for chromote (Windows only issue)
# https://github.com/rstudio/chromote/issues/114#issuecomment-1675406196
options(chromote.timeout = 60)

test_check("qtkit")
