#' Save ggplot Objects to Files
#'
#' @description
#' A wrapper around `ggsave` that facilitates saving ggplot objects within knitr
#' documents. Automatically handles file naming and directory creation, with
#' support for multiple output formats.
#'
#' @details
#' This function extends `ggplot2::ggsave` by:
#' - Using knitr code block labels for automatic file naming
#' - Creating target directories if they don't exist
#' - Supporting multiple output formats (PDF, PNG, JPEG, TIFF, SVG)
#' - Applying custom themes to plots before saving
#'
#' @param gg_obj The ggplot to be written. If not specified, the last
#' ggplot created will be written.
#' @param file The name of the file to be written. If not specified, the
#' label of the code block will be used.
#' @param target_dir The directory where the file will be written. If not
#' specified, the current working directory will be used.
#' @param device The device to be used for saving the ggplot. Options
#' include "pdf" (default), "png", "jpeg", "tiff", and "svg".
#' @param theme The ggplot2 theme to be applied to the ggplot. Default is
#' the theme specified in the ggplot2 options.
#' @param ... Additional arguments to be passed to the `ggsave`
#' function from the `ggplot2` package.
#'
#' @return The path of the written file.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' plot_dir <- file.path(tempdir(), "plot")
#'
#' # Write a ggplot object as a PDF file
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point()
#'
#' write_gg(
#'   gg_obj = p,
#'   file = "plot_file",
#'   target_dir = plot_dir,
#'   device = "pdf"
#' )
#'
#' unlink(plot_dir)
#' }
#' @importFrom ggplot2 ggsave
#' @importFrom knitr opts_current
#'
#' @export write_gg
#' @keywords publishing
write_gg <-
  function(
      gg_obj = NULL,
      file = NULL,
      target_dir = NULL,
      device = "pdf",
      theme = NULL,
      ...) {
    # Retrieve the label of the current code chunk
    block_label <- knitr::opts_current$get("label")
    # If ggplot is not specified, use the last ggplot created
    if (is.null(gg_obj)) {
      gg_obj <- ggplot2::last_plot()
    }
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
      "pdf" = ".pdf",
      "png" = ".png",
      "jpeg" = ".jpeg",
      "tiff" = ".tiff",
      "svg" = ".svg"
    )
    file <- paste0(file, extension)
    file <- file.path(target_dir, file)
    # Write the ggplot to the specified file
    ggplot2::ggsave(file, gg_obj, device = device, ...)
    # Return the file path, invisibly
    return(invisible(file))
  }
