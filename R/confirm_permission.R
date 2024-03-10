#' Confirm permission to use data
#'
#' This function is for internal use only.
#'
#' This function confirms that the user has permission to use the data.
#' If not, the script is returns FALSE and stops.
#' @noRd
#' @keywords internal
#' @return TRUE if the user confirms permission, FALSE otherwise
#'
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
