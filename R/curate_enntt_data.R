#' Curate ENNTT Data
#'
#' This function processes and curates ENNTT (European Parliament) data from a specified directory.
#' It handles both .dat files (containing XML metadata) and .tok files (containing text content).
#'
#' @details
#' The function expects a directory containing paired .dat and .tok files with matching names. As found in the raw ENNTT data \url{https://github.com/senisioi/enntt-release}.
#' The .dat files should contain XML-formatted metadata with attributes:
#' - session_id: Unique identifier for the parliamentary session
#' - mepid: Member of European Parliament ID
#' - state: Country or state representation
#' - seq_speaker_id: Sequential ID within the session
#'
#' The .tok files should contain the corresponding text content, one entry per line.
#'
#' @param dir_path A string. The path to the directory containing the ENNTT data files.
#'
#' @return A tibble containing the curated ENNTT data with columns:
#'   \itemize{
#'     \item session_id: Parliamentary session identifier
#'     \item speaker_id: Speaker's MEP ID
#'     \item state: Representative's state/country
#'     \item session_seq: Sequential position in session
#'     \item text: Speech content
#'     \item type: Corpus type identifier
#'   }
#'
#' @examples
#' \dontrun{
#' curated_data <- curate_enntt_data("/path/to/enntt/data/")
#' }
#'
#' @importFrom xml2 xml_attr xml_find_all read_html
#'
#' @export
curate_enntt_data <- function(dir_path) {
  # Validate input directory
  validate_dir_path(dir_path)
  # Find and process corpus files
  corpus_types <- find_enntt_files(dir_path)
  tryCatch(
    {
      # Process each corpus type and combine results
      data_list <- lapply(corpus_types, function(x) {
        curate_enntt_file(dir_path, x)
      })
      data_df <- do.call(rbind, data_list)
      # Convert row names to sequential numbers
      rownames(data_df) <- NULL
      return(data_df)
    },
    error = function(e) {
      stop("Error processing ENNTT data: ", e$message)
    }
  )
}

#' Validate Directory Path
#'
#' @param dir_path Directory path to validate
#' @keywords internal
validate_dir_path <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    stop("Directory does not exist: ", dir_path)
  }
  if (length(list.files(dir_path, pattern = "(dat|tok)$")) == 0) {
    stop("No .dat or .tok files found in directory")
  }
}

#' Find ENNTT Files
#'
#' @param dir_path Directory to search for ENNTT files
#' @return Vector of unique corpus types
#' @keywords internal
find_enntt_files <- function(dir_path) {
  corpus_files <- basename(list.files(
    path = dir_path,
    pattern = "(dat|tok)$",
    full.names = TRUE,
    recursive = TRUE
  ))
  unique(unlist(lapply(corpus_files, function(x) sub("\\..*$", "", x))))
}

#' Curate Single ENNTT File Pair
#'
#' @param dir_path Directory containing the files
#' @param corpus_type Type identifier for the corpus
#' @return Data frame of curated data
#' @keywords internal
curate_enntt_file <- function(dir_path, corpus_type) {
  # Create file paths
  dat_file <- file.path(dir_path, paste0(corpus_type, ".dat"))
  tok_file <- file.path(dir_path, paste0(corpus_type, ".tok"))
  # Validate file existence
  if (!file.exists(dat_file)) stop("DAT file not found: ", dat_file)
  if (!file.exists(tok_file)) stop("TOK file not found: ", tok_file)
  tryCatch(
    {
      # Read and process files
      dat <- xml2::read_html(dat_file)
      dat <- xml2::xml_find_all(dat, "//line")
      tok <- readLines(tok_file)
      # Validate content alignment
      if (length(dat) != length(tok)) {
        stop("Mismatched lengths between DAT and TOK files")
      }
      # Process data
      dat_attrs <- lapply(dat, extract_dat_attrs)
      dat_attrs <- do.call(rbind, dat_attrs)
      dataset <- cbind(dat_attrs, text = tok)
      dataset$type <- corpus_type
      return(dataset)
    },
    error = function(e) {
      stop("Error processing files for corpus type '", corpus_type, "': ", e$message)
    }
  )
}

#' Extract Attributes from XML Line Node
#'
#' @param line_node XML node containing line attributes
#' @return Data frame of extracted attributes
#' @keywords internal
extract_dat_attrs <- function(line_node) {
  tryCatch(
    {
      session_id <- xml2::xml_attr(line_node, "session_id")
      speaker_id <- xml2::xml_attr(line_node, "mepid")
      state <- xml2::xml_attr(line_node, "state")
      session_seq <- xml2::xml_attr(line_node, "seq_speaker_id")
      if (any(sapply(list(session_id, speaker_id, state, session_seq), is.null))) {
        stop("Error processing files: Missing required attributes in XML node")
      }
      data.frame(
        session_id,
        speaker_id,
        state,
        session_seq,
        stringsAsFactors = FALSE
      )
    },
    error = function(e) {
      stop("Error extracting attributes: ", e$message)
    }
  )
}
