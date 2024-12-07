#' Write an R object as a file
#'
#' @description
#' A wrapper around `dput` that facilitates saving R objects within knitr documents.
#' Automatically handles file naming and directory creation, with support for
#' preserving object structure and attributes.
#'
#' @details
#' This function extends `dput` functionality by:
#' - Using knitr code block labels for automatic file naming
#' - Creating target directories if they don't exist
#' - Preserving complex object structures and attributes
#' - Supporting all R object types (vectors, lists, data frames, etc.)
#' 
#' Objects saved with this function can be read back using the standard
#' `dget` function.
#'
#' @param obj The R object to be written.
#' @param file The name of the file to be written. If not specified, the
#' label of the code block will be used.
#' @param target_dir The directory where the file will be written. If not
#' specified, the current working directory will be used.
#' @param ... Additional arguments to be passed to `dput`.
#'
#' @return The path of the written file.
#'
#' @examples
#' \dontrun{
#' obj_dir <- file.path(tempdir(), "obj")
#'
#' # Write a data frame as a file
#' write_obj(
#'   obj = mtcars,
#'   file = "mtcars_data",
#'   target_dir = obj_dir
#' )
#'
#' # Read the file back into an R session
#' my_mtcars <- dget(file.path(obj_dir, "mtcars_data"))
#'
#' unlink(obj_dir)
#' }
#' @importFrom knitr opts_current
#'
#' @export write_obj
#' @keywords publishing
write_obj <-
  function(
      obj,
      file = NULL,
      target_dir = NULL,
      ...) {
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
    file <- file.path(target_dir, file)
    # Save the object to the specified file
    dput(obj, file, ...)
    # Return the file path, invisibly
    return(invisible(file))
  }
