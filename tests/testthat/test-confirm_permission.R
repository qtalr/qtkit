library(testthat)

test_that("confirm_permission returns TRUE when user confirms", {
  # Mock user input
  mock_readline <- function(prompt) {
    return("y")
  }

  # Mock interactive environment
  mock_interactive <- function() {
    return(TRUE)
  }

  # Replace readline and interactive functions with mocks
  with_mocked_bindings(
    readline = mock_readline,
    interactive = mock_interactive,
    {
      # Call the function
      result <- confirm_permission()

      # Check if the result is TRUE
      expect_true(result)
    }
  )
})

test_that("confirm_permission returns FALSE when user does not confirm", {
  # Mock user input
  mock_readline <- function(prompt) {
    return("n")
  }

  # Mock interactive environment
  mock_interactive <- function() {
    return(TRUE)
  }

  # Replace readline and interactive functions with mocks
  with_mocked_bindings(
    readline = mock_readline,
    interactive = mock_interactive, {
      # Call the function
      result <- confirm_permission()

      # Check if the result is FALSE
      expect_false(result)
    }
  )
})
