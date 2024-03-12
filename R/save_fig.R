#' Save a plot as a file
#'
#' @description This function is a wrapper around `ggsave` that allows
#' you to save a plot as part of a knitr document. It is designed to be
#' used in a code block. The file name, if not specified, will be the
#' label of the code block.
#'
#' @param plot The plot to be saved. If not specified, the last plot
#' created will be saved.
#' @param file The name of the file to be saved. If not specified, the
#' label of the code block will be used.
#' @param target_dir The directory where the file will be saved. If not
#' specified, the current working directory will be used.
#' @param theme The ggplot2 theme to be applied to the plot. Default is
#' the theme specified in the ggplot2 options.
#' @param device The device to be used for saving the plot. Options
#' include "pdf", "png", "jpeg", "tiff", and "svg".
#' @param ... Additional arguments to be passed to `ggsave`.
#'
#' @return The path of the saved file.
#'
#' @examples
#' \dontrun{
#' # Save a plot as a PDF file
#' p1 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point()
#' save_fig(p1, file = "mtcars_plot", device = "pdf")
#' }
#'
#' @importFrom ggplot2 ggsave
#' @importFrom knitr opts_current
#'
#' @export save_fig
#' @keywords publishing
save_fig <- function(plot = NULL, file = NULL, target_dir = NULL,
                     theme = NULL, device = "pdf", ...) {
  # Retrieve the label of the current code chunk
  block_label <- knitr::opts_current$get("label")

  # If plot is not specified, use the last plot created
  if (is.null(plot)) {
    plot <- ggplot2::last_plot()
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

  # Save the plot to the specified file
  ggplot2::ggsave(file, plot, device = device, ...)
  # Return the file path, invisibly
  return(invisible(file))
}
