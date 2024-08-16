#' Download TalkBank Corpus Data
#'
#' Downloads the utterances, transcripts, participants, tokens, and token
#' types data from the TalkBank database and saves it to disk in the specified
#' target directory.
#'
#' @param corpus_name The name of the TalkBank corpus to download data from.
#' @param corpus_path The path to the TalkBank corpus to download data from.
#' @param target_dir The directory to save the downloaded data to.
#' @param force If `TRUE`, the data will be downloaded even if it already
#' exists on disk.
#' @param confirmed If `TRUE`, the user has confirmed that they have permission
#' to use the data.
#' If `FALSE`, the function will prompt the user to confirm permission.
#' Setting this to `TRUE` is useful for reproducible workflows.
#' @param timeout The maximum time in seconds to wait for the data to be
#' downloaded. Default is 60 seconds.
#'
#' @return A message indicating whether the data was acquired or already
#' existed on disk, writes the data files to disk in the specified target
#' directory.
#'
#' @examples
#' \dontrun{
#' data_dir <- file.path(tempdir(), "data")
#'
#' get_talkbank_data(
#'  corpus_name = "ca",
#'  corpus_path = c("ca", "Nahuatl"),
#'  target_dir = data_dir,
#'  confirmed = TRUE)
#' }
#'
#' @importFrom dplyr everything
#' @importFrom fs dir_exists dir_create file_exists path
#' @importFrom R.utils withTimeout
#' @importFrom readr write_csv
#' @importFrom TBDBr getUtterances getTranscripts getParticipants getTokens
#' @importFrom tidyr unnest
#'
#' @export
get_talkbank_data <-
  function(
    corpus_name,
    corpus_path,
    target_dir,
    force = FALSE,
    confirmed = FALSE,
    timeout = 60) {

  # Confirm permission to use the data
  confirmed <- confirm_if_needed(confirmed)
    if (!confirmed) {
      return(message("Aborted."))
    }

  # Validate input parameters
  validate_inputs_gtd(corpus_name, corpus_path, target_dir, force, confirmed)

  # Check if the target directory exists, if not, create it
  if (!file.exists(target_dir)) dir.create(target_dir, recursive = TRUE)

  # Set up file paths names
  utterances_file <- file.path(target_dir, "utterances.csv")
  transcripts_file <- file.path(target_dir, "transcripts.csv")
  participants_file <- file.path(target_dir, "participants.csv")
  tokens_file <- file.path(target_dir, "tokens.csv")
  token_types_file <- file.path(target_dir, "token_types.csv")

  # Check if the file doesn't exist or force is TRUE
  if (!file.exists(utterances_file) || force) {
    message("Acquiring utterances...")
    write_to_disk(
      TBDBr::getUtterances,
      corpus_name,
      corpus_path,
      utterances_file,
      timeout)
  } else {
    message("Already acquired: ", utterances_file)
  }

  if (!file.exists(transcripts_file) || force) {
    message("Acquiring transcripts...")
    write_to_disk(
      TBDBr::getTranscripts,
      corpus_name,
      corpus_path,
      transcripts_file,
      timeout)
  } else {
    message("Already acquired: ", transcripts_file)
  }

  if (!file.exists(participants_file) || force) {
    message("Acquiring participants...")
    write_to_disk(
      TBDBr::getParticipants,
      corpus_name,
      corpus_path,
      participants_file,
      timeout)
  } else {
    message("Already acquired: ", participants_file)
  }

  if (!file.exists(tokens_file) || force) {
    message("Acquiring tokens...")
    write_to_disk(
      TBDBr::getTokens,
      corpus_name,
      corpus_path,
      tokens_file,
      timeout)
  } else {
    message("Already acquired: ", tokens_file)
  }

  if (!file.exists(token_types_file) || force) {
    message("Acquiring token types...")
    write_to_disk(
      TBDBr::getTokenTypes,
      corpus_name,
      corpus_path,
      token_types_file,
      timeout)
  } else {
    message("Already acquired: ", token_types_file)
  }

  message("Acquisition complete.")
  message("Use `force = TRUE` to re-acquire.")
}

# Helper functions

suppress_output <- function(func, ...) {
  # Create a text connection to capture output
  temp_output <- character()
  tc <- textConnection("temp_output", "w", local = TRUE)

  # Redirect output to the text connection
  sink(tc)
  sink(tc, type = "message")

  # Run the function and capture the result
  result <- tryCatch({
    func(...)
  }, finally = {
    # Restore normal output
    sink(type = "message")
    sink()

    # Close the text connection
    close(tc)
  })

  return(result)
}

write_to_disk <-
  function(
    func,
    corpus_name,
    corpus_path,
    file_path,
    timeout) {
  tryCatch({
    results <- R.utils::withTimeout({
      suppress_output(func, corpus_name, corpus_path)
    }, timeout = timeout, onTimeout = "silent")
    results <- results |> tidyr::unnest(cols = dplyr::everything())
    readr::write_csv(results, file_path)
    message("Acquired: ", file_path)
    return(invisible(NULL))
  }, error = function(e) {
    if (grepl("Operation was aborted by an application callback", e)) {
      msg <- paste(
          "The function",
          deparse(substitute(func)),
          "timed out at",
          timeout,
          "seconds. Change the `timeout` variable for a larger timeout.")
      message(msg)
    } else {
      message("An error occurred while acquiring ", file_path, ".")
    }
    return(invisible(NULL))
  })
}

validate_inputs_gtd <-
  function(
    corpus_name,
    corpus_path,
    target_dir,
    force,
    confirmed) {
  # Check if TBDBr path is valid
  if (!TBDBr::validPath(c(corpus_name, corpus_path))) {
    stop("Invalid path to TalkBank Database.
          Check `corpus_name` and `corpus_path` again.")
  }
  # Check for valid force and confirmed values
  if (!(force %in% c(TRUE, FALSE))) {
    stop("`force` must be a logical value.")
  }
  if (!(confirmed %in% c(TRUE, FALSE))) {
    stop("`confirmed` must be a logical value.")
  }
}
utils::globalVariables(c("confirm_if_needed"))
