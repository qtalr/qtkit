#' Curate SWDA data
#'
#' Process and curate Switchboard Dialog Act (SWDA) data by reading all .utt
#' files from a specified directory and converting them into a structured
#' format.
#'
#' @param dir_path Character string. Path to the directory containing .utt
#' files.
#'
#' @return A tibble containing the curated SWDA data with columns:
#'   \itemize{
#'     \item doc_id: Document identifier
#'     \item damsl_tag: Dialog act annotation
#'     \item speaker_id: Unique speaker identifier
#'     \item speaker: Speaker designation (A or B)
#'     \item turn_num: Turn number in conversation
#'     \item utterance_num: Utterance number
#'     \item utterance_text: Actual spoken text
#'   }
#'
#' @examples
#' \dontrun{
#' swda_data <- curate_swda_data("/path/to/directory")
#' }
#'
#' @importFrom purrr map_dfr
#' @importFrom tibble as_tibble
#'
#' @export
curate_swda_data <- function(dir_path) {
  # Validate directory path
  if (!dir.exists(dir_path)) {
    stop("Directory does not exist: ", dir_path)
  }

  # Find all .utt files
  utt_files <- list.files(
    path = dir_path,
    pattern = "\\.utt$", # More specific pattern
    full.names = TRUE,
    recursive = TRUE
  )

  # Check if any files were found
  if (length(utt_files) == 0) {
    warning("No .utt files found in directory: ", dir_path)
    return(tibble::tibble())
  }

  # Process all files and combine results
  data_df <- purrr::map_dfr(
    .x = utt_files,
    .f = curate_swda_file,
    .progress = TRUE
  ) |>
    tibble::as_tibble()

  return(data_df)
}

#' Process a single SWDA utterance file
#'
#' @param file_path Character string. Path to the .utt file
#' @return A data frame containing processed data from the file
#' @keywords internal
curate_swda_file <- function(file_path) {
  # Read file contents
  doc_lines <- readLines(file_path)

  # Extract speaker information
  speaker_info <- extract_speaker_info(doc_lines)

  # Extract and process utterances
  utterances_df <- extract_utterances(doc_lines, speaker_info)

  return(utterances_df)
}

#' Extract speaker information from document lines
#'
#' @param doc_lines Character vector of file lines
#' @return Named list of speaker information
#' @keywords internal
extract_speaker_info <- function(doc_lines) {
  speaker_pattern <- "\\d+_\\d+_\\d+"
  speaker_line <- grep(speaker_pattern, doc_lines, value = TRUE)

  if (length(speaker_line) == 0) {
    stop("Could not find speaker information in file")
  }

  speaker_info_parts <- strsplit(
    sub(".*\\t", "", speaker_line),
    "_"
  )[[1]]

  return(list(
    doc_id = speaker_info_parts[1],
    speaker_a_id = speaker_info_parts[2],
    speaker_b_id = speaker_info_parts[3]
  ))
}

#' Extract and process utterances from document lines
#'
#' @param doc_lines Character vector of file lines
#' @param speaker_info List of speaker information
#' @return Data frame of processed utterances
#' @keywords internal
extract_utterances <- function(doc_lines, speaker_info) {
  # Find text section
  text_start <- grep("={3,}", doc_lines)[1] + 1
  text_end <- length(doc_lines)

  # Extract non-empty lines
  text_lines <- grep(
    ".+",
    trimws(doc_lines[text_start:text_end]),
    value = TRUE
  )

  # Create initial data frame
  df <- data.frame(
    doc_id = speaker_info$doc_id,
    text = text_lines,
    stringsAsFactors = FALSE
  )

  # Extract components using regular expressions
  df <- transform(df,
    damsl_tag = trimws(sub("^(.+?)\\s.*$", "\\1", text)),
    speaker_turn = sub("^.*([AB]\\.\\d+).*$", "\\1", text),
    utterance_num = sub("^.*utt(\\d+).*$", "\\1", text),
    utterance_text = trimws(sub("^.*:(.+)$", "\\1", text))
  )

  # Process speaker information
  speaker_info <- process_speaker_info(
    df$speaker_turn,
    speaker_info$speaker_a_id,
    speaker_info$speaker_b_id
  )

  # Combine all information
  result_df <- data.frame(
    doc_id = df$doc_id,
    damsl_tag = df$damsl_tag,
    speaker_id = speaker_info$speaker_id,
    speaker = speaker_info$speaker,
    turn_num = speaker_info$turn_num,
    utterance_num = df$utterance_num,
    utterance_text = df$utterance_text,
    stringsAsFactors = FALSE
  )

  return(result_df)
}

#' Process speaker turn information
#'
#' @param speaker_turn Vector of speaker turns
#' @param speaker_a_id ID for speaker A
#' @param speaker_b_id ID for speaker B
#' @return List with processed speaker information
#' @keywords internal
process_speaker_info <- function(speaker_turn, speaker_a_id, speaker_b_id) {
  # Split speaker turn information
  turn_parts <- strsplit(speaker_turn, "\\.")

  # Extract components
  speaker <- sapply(turn_parts, "[[", 1)
  turn_num <- sapply(turn_parts, "[[", 2)

  # Assign speaker IDs
  speaker_id <- ifelse(speaker == "A", speaker_a_id, speaker_b_id)

  return(list(
    speaker = speaker,
    turn_num = turn_num,
    speaker_id = speaker_id
  ))
}
