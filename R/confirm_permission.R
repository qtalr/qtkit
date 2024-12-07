#' Confirm User Permission for Data Usage
#'
#' Internal function that prompts the user to confirm they have permission to use
#' the data. Only prompts in interactive sessions.
#'
#' @details
#' This function displays a prompt asking users to confirm they have permission
#' to use the data. It returns TRUE if confirmed, FALSE otherwise. The function
#' only prompts in interactive sessions.
#'
#' @return Logical value:
#' - TRUE if user confirms permission
#' - FALSE if user denies permission or session is non-interactive
#'
#' @keywords internal
#' @noRd
confirm_permission <- function() {
  # Confirm that the user has permission to use the data
  # If not, stop the script
  if (interactive()) {
    message("Are you aware of the permissions to use this data?")
    confirm <- readline(prompt = "Proceed? (y/n): ")
    if (tolower(confirm) == "y") {
      return(TRUE)
    } else {
      message("Please review the documentation and try again.")
      invisible(FALSE)
    }
  }
}

interactive <- NULL
readline <- NULL
