#' Write a kable object to a file
#'
#' @description This function is a wrapper around `save_kable` from the
#' `kableExtra` package that allows you to write a kable object as part of
#' a knitr document as an output for later use. It is designed to be used
#' in a code block. The file name, if not specified, will be the label of
#' the code block.
#'
#' @param kbl_obj The knitr_kable object to be written.
#' @param file The name of the file to be written. If not specified,
#' the name will be based on the current knitr code block label.
#' @param target_dir The directory where the file will be written. If not
#' specified, the current working directory will be used.
#' @param device The device to be used for saving the file. Options
#' include "pdf" (default), "html", "latex", "png", and "jpeg". Note that
#' a Chromium-based browser (e.g., Google Chrome, Chromium, Microsoft Edge or
#' Brave) is required on your system for all options except "latex'. If a
#' suitable browser is not available, the function will stop and return an
#' error message.
#' @param bs_theme The Bootstrap theme to be applied to the kable object
#' (only applicable for HTML output). Default is "bootstrap".
#' @param ... Additional arguments to be passed to the `save_kable`
#' function from the `kableExtra` package.
#'
#' @return The path of the written file.
#'
#' @examples
#' \dontrun{
#' library(knitr)
#'
#' table_dir <- file.path(tempdir(), "table")
#'
#' mtcars_kbl <- kable(
#'  x = mtcars[1:5, ],
#'  format = "html")
#'
#' # Write a kable object as a PDF file
#' write_kbl(
#'  kbl_obj = mtcars_kbl,
#'  file = "kable_pdf",
#'  target_dir = table_dir,
#'  device = "pdf")
#'
#' # Write a kable as an HTML file with a custom Bootstrap theme
#' write_kbl(
#'  kbl_obj = mtcars_kbl,
#'  file = "kable_html",
#'  target_dir = table_dir,
#'  device = "html",
#'  bs_theme = "flatly")
#'
#' unlink(table_dir)
#' }
#' @export write_kbl
#' @keywords publishing
write_kbl <-
  function(
    kbl_obj,
    file = NULL,
    target_dir = NULL,
    device = "pdf",
    bs_theme = "bootstrap",
    ...) {
    # Retrieve the label of the current code chunk
    block_label <- knitr::opts_current$get("label")
    # If file name is not specified, use the block label as the file name this
    # is going tooo far
    if (is.null(file)) {
      if (is.null(block_label)) {
        # Stop execution and throw an error if both file name
        # and block label are not specified
        stop("File must be specified")
      }
      # Use the block label as the file name
      file <- block_label
    }
    # If target directory is not specified, use the current working directory
    if (is.null(target_dir)) {
      target_dir <- getwd()
    }
    # If the target directory does not exist, create it
    if (!dir.exists(target_dir)) {
      dir.create(target_dir, recursive = TRUE)
      # Output a message indicating that the directory has been created
      message("Directory created: ", target_dir)
    }
    # Construct the full file path
    extension <- switch(device,
      "html" = ".html",
      "pdf" = ".pdf",
      "latex" = ".tex",
      "png" = ".png",
      "jpeg" = ".jpeg"
    )
    # Check if a Chromium-based browser is required
    if (device %in% c("html", "pdf", "png", "jpeg")) {
      browser_path <- check_chromium_browser()
      if (!is.null(browser_path)) {
        if (file.exists(browser_path)) {
          options(chromote.browser_path = browser_path)
        } else {
          stop("The detected browser path does not exist: ", browser_path)
        }
      } else {
        msg <- paste0("A Chromium-based browser (e.g., Google Chrome, ",
          "Chromium, Microsoft Edge, or Brave) is required on your system ",
          "to save kable objects in ", device, " format.")
        stop(msg)
      }
    }
    file <- paste0(file, extension)
    file <- file.path(target_dir, file)
    # Save the kable to the specified file
    kableExtra::save_kable(kbl_obj, file, bs_theme, ...)
    # Return the file path, invisibly
    return(invisible(file))
  }

# Helper functions
check_chromium_browser <- function() {
  if (requireNamespace("chromote", quietly = TRUE)) {
    tryCatch({
      browser_path <- chromote::find_chrome()
      if (!is.null(browser_path) && file.exists(browser_path)) {
        return(browser_path)
      }
    }, error = function(e) {
      warning("Error in chromote::find_chrome(): ", e$message)
    })
  }
  return(NULL)
}
