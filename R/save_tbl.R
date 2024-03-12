#' Save a table as a file
#'
#' @description This function is a wrapper around `save_kable` from the
#' kableExtra package that allows you to save a table as part of a knitr
#' document. It is designed to be used in a code block. The file name,
#' if not specified, will be the label of the code block.
#'
#' @param table The table to be saved.
#' @param file The name of the file to be saved. If not specified,
#' the name will be based on the current knitr block label.
#' @param target_dir The directory where the file will be saved. If not
#' specified, the current working directory will be used.
#' @param bs_theme The Bootstrap theme to be applied to the table (only
#' applicable for HTML output). Default is "bootstrap".
#' @param device The device to be used for saving the table. Options
#' include "pdf", "html", "latex", "png", and "jpeg". Default is "pdf".
#' @param ... Additional arguments to be passed to the save_kable function
#' from the kableExtra package.
#'
#' @return The path of the saved file.
#'
#' @examples
#' \dontrun{
#' # Save a table as a PDF file
#' save_tbl(mtcars, file = "mtcars_table", device = "pdf")
#'
#' # Save a table as an HTML file with a custom Bootstrap theme
#' save_tbl(mtcars, file = "mtcars_table", device = "html", bs_theme = "flatly")
#' }
#'
#' @importFrom kableExtra save_kable
#' @importFrom knitr opts_current
#'
#' @export save_tbl
#' @keywords publishing
save_tbl <-
  function(table, file = NULL, target_dir = NULL,
           bs_theme = "bootstrap", device = "pdf", ...) {
    # Retrieve the label of the current code chunk
    block_label <- knitr::opts_current$get("label")

    # If file name is not specified, use the block label as the file name
    if (is.null(file)) {
      if (is.null(block_label)) {
        # Stop execution and throw an error if both file name
        # and block label are not specified
        stop("file must be specified")
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
    file <- paste0(file, extension)
    file <- file.path(target_dir, file)

    # Save the table to the specified file
    kableExtra::save_kable(table, file, bs_theme, ...)
    # Return the file path, invisibly
    return(invisible(file))
  }
