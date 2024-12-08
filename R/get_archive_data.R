#' Download and Extract Archive Files
#'
#' @description
#' Downloads compressed archive files from a URL and extracts their contents to a
#' specified directory. Supports multiple archive formats and handles permission
#' confirmation.
#'
#' @details
#' Supported archive formats:
#' - ZIP (.zip)
#' - Gzip (.gz)
#' - Tar (.tar)
#' - Compressed tar (.tgz)
#'
#' The function includes safety features:
#' - Permission confirmation for data usage
#' - Directory existence checks
#' - Archive format validation
#' - Automatic file cleanup
#'
#' @param url Character string. Full URL to the compressed archive file.
#' @param target_dir Character string. Directory where the archive contents
#'        should be extracted.
#' @param force Logical. If TRUE, overwrites existing data in target directory.
#'        Default is FALSE.
#' @param confirmed Logical. If TRUE, skips permission confirmation prompt.
#'        Useful for reproducible workflows. Default is FALSE.
#'
#' @return Invisible NULL. Called for side effects:
#' - Downloads archive file
#' - Creates target directory if needed
#' - Extracts archive contents
#' - Cleans up temporary files
#'
#' @importFrom utils download.file unzip untar
#' @importFrom tools file_ext
#'
#' @examples
#' \dontrun{
#' data_dir <- file.path(tempdir(), "data")
#' url <-
#'   "https://raw.githubusercontent.com/qtalr/qtkit/main/inst/extdata/test_data.zip"
#' get_archive_data(
#'   url = url,
#'   target_dir = data_dir,
#'   confirmed = TRUE
#' )
#' }
#' @export
get_archive_data <-
  function(
      url,
      target_dir,
      force = FALSE,
      confirmed = FALSE) {
    confirmed <- confirm_if_needed(confirmed)
    if (!confirmed) {
      return(message("Aborted."))
    }

    ext <- tools::file_ext(url)
    validate_file_extension(ext)

    if (!dir.exists(target_dir) || force) {
      message("Creating target data directory \n")
      dir.create(path = target_dir, recursive = TRUE, showWarnings = FALSE)

      download_and_decompress(url, target_dir, ext)
      clean_filenames(target_dir)
    } else {
      message("Data already exists \n")
    }
  }

#' Check if Permission Confirmation is Needed
#'
#' Helper function that determines whether to prompt the user for permission
#' confirmation based on the confirmed parameter.
#'
#' @param confirmed Logical indicating if permission is pre-confirmed
#'
#' @return Logical indicating if permission is granted
#'
#' @keywords internal
confirm_if_needed <- function(confirmed) {
  if (!confirmed) {
    return(confirm_permission())
  }
  return(TRUE)
}

#' Validate Archive File Extension
#'
#' Helper function that checks if the file extension is supported
#' (zip, gz, tar, or tgz).
#'
#' @param ext Character string of the file extension
#'
#' @return No return value, called for side effects
#'
#' @details Stops execution if extension is not supported
#'
#' @keywords internal
validate_file_extension <- function(ext) {
  if (!ext %in% c("zip", "gz", "tar", "tgz")) {
    stop("Target file given is not supported")
  }
}

#' Download and Decompress Archive File
#'
#' Helper function that downloads an archive file to a temporary location
#' and decompresses it to the target directory.
#'
#' @param url Character string of the archive file URL
#' @param target_dir Character string of the target directory path
#' @param ext Character string of the file extension
#'
#' @return No return value, called for side effects
#'
#' @keywords internal
download_and_decompress <- function(url, target_dir, ext) {
  message("Downloading data... \n")
  temp <- tempfile()
  utils::download.file(url = url, destfile = temp)

  if (ext == "zip") {
    utils::unzip(zipfile = temp, exdir = target_dir, junkpaths = TRUE)
  } else {
    utils::untar(tarfile = temp, exdir = target_dir)
  }

  message("Data downloaded! \n")
}

#' Clean Downloaded File Names
#'
#' Helper function that removes spaces from filenames in the target directory,
#' replacing them with underscores.
#'
#' @param target_dir Character string of the target directory path
#'
#' @return Invisible NULL, called for side effects
#'
#' @keywords internal
clean_filenames <- function(target_dir) {
  files <- list.files(target_dir)
  new_files <- gsub(" ", "_", files)
  invisible(
    file.rename(
      from = file.path(target_dir, files),
      to = file.path(target_dir, new_files)
    )
  )
}
