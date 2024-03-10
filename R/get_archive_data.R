#' Download an Archive File and Unarchives its Contents
#'
#' Possible file types include .zip, .gz, .tar, and .tgz
#'
#' @param url A character vector representing the full url to
#' the compressed file
#' @param target_dir The directory where the archive file should be downloaded
#' @param force An optional argument which forcefully overwrites existing data
#' @param confirmed If `TRUE`, the user has confirmed that they have
#'  permission to use the data.
#' If `FALSE`, the function will prompt the user to confirm permission.
#' Setting this to `TRUE` is useful for reproducible workflows.
#'
#' @returns Download and extract the archive file
#'
#' @importFrom utils download.file unzip untar
#' @importFrom tools file_ext
#'
#' @examples \dontrun{
#' get_archive_data(url = "http://www.test.com/file.zip", target_dir = "./")
#' }
#' @export
get_archive_data <-
  function(url, target_dir, force = FALSE, confirmed = FALSE) {
    confirmed <- confirm_if_needed(confirmed)
    if (!confirmed) {
      return(message("Aborted."))
    }

    ext <- tools::file_ext(url)
    validate_file_extension(ext)

    if (!dir.exists(target_dir) || force) {
      cat("Creating target data directory \n")
      dir.create(path = target_dir, recursive = TRUE, showWarnings = FALSE)

      download_and_decompress(url, target_dir, ext)
      clean_filenames(target_dir)
    } else {
      cat("Data already exists \n")
    }
  }

# ' @keywords internal
# Helper function to confirm permission
confirm_if_needed <- function(confirmed) {
  if (!confirmed) {
    return(confirm_permission())
  }
  return(TRUE)
}

# ' @keywords internal
# Helper function to validate file extension
validate_file_extension <- function(ext) {
  if (!ext %in% c("zip", "gz", "tar", "tgz")) {
    stop("Target file given is not supported")
  }
}

# ' @keywords internal
# Helper function to download and decompress file
download_and_decompress <- function(url, target_dir, ext) {
  cat("Downloading data... \n")
  temp <- tempfile()
  utils::download.file(url = url, destfile = temp)

  if (ext == "zip") {
    utils::unzip(zipfile = temp, exdir = target_dir, junkpaths = TRUE)
  } else {
    utils::untar(tarfile = temp, exdir = target_dir)
  }

  cat("Data downloaded! \n")
}

# ' @keywords internal
# Helper function to clean filenames
clean_filenames <- function(target_dir) {
  files <- list.files(target_dir)
  new_files <- sub(pattern = "^\\.", replacement = "", files)
  invisible(
    file.rename(
      from = file.path(target_dir, files),
      to = file.path(target_dir, new_files)
    )
  )
}
