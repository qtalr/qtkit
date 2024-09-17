#' Curate SWDA data
#'
#' This function curates the SWDA (Switchboard Dialog Act) data by processing
#' all the .utt files in the specified directory.
#'
#' @param dir_path The path to the directory containing the .utt files.
#'
#' @return A data frame containing the curated SWDA data.
#'
#' @examples
#' \dontrun{
#' curate_swda_data('/path/to/directory')
#' }
#' @importFrom purrr map_dfr
#' @importFrom tibble as_tibble
#'
#' @export
# TODO: curate_swda_data function
curate_swda_data <- function(dir_path) {
    files <- list.files(path = dir_path, pattern = ".utt", full.names = TRUE, recursive = TRUE)

    data_df <- files |>
        purrr::map_dfr(curate_swda_file, .progress = TRUE) |>
        as_tibble()

    return(data_df)
}

curate_swda_file <- function(file) {
    # Read `file` by lines
    doc_chr <- readLines(file)

    # Extract `doc_id`, `speaker_a_id`, and `speaker_b_id`
    speaker_info_chr <- unlist(strsplit(sub(".*\\t", "", grep("\\d+_\\d+_\\d+", doc_chr,
        value = TRUE)), "_"))

    doc_id <- speaker_info_chr[1]
    speaker_a_id <- speaker_info_chr[2]
    speaker_b_id <- speaker_info_chr[3]

    # Extract `text`
    text_start_index <- grep("={3,}", doc_chr) + 1
    text_end_index <- length(doc_chr)

    text <- grep(".+", trimws(doc_chr[text_start_index:text_end_index]), value = TRUE)

    swda_df <- data.frame(doc_id = doc_id, text = text, stringsAsFactors = FALSE)

    # Extract column information from `text`
    swda_df$damsl_tag <- trimws(sub("^(.+?)\\s.*$", "\\1", swda_df$text))
    swda_df$speaker_turn <- sub("^.*([AB]\\.\\d+).*$", "\\1", swda_df$text)
    swda_df$utterance_num <- sub("^.*utt(\\d+).*$", "\\1", swda_df$text)
    swda_df$utterance_text <- trimws(sub("^.*:(.+)$", "\\1", swda_df$text))


    # Separate speaker_turn into distinct columns
    speaker_turn_split <- strsplit(swda_df$speaker_turn, "\\.")
    swda_df$speaker <- sapply(speaker_turn_split, "[[", 1)
    swda_df$turn_num <- sapply(speaker_turn_split, "[[", 2)

    # Link speaker with speaker_id
    swda_df$speaker_id <- ifelse(swda_df$speaker == "A", speaker_a_id, speaker_b_id)

    # Select relevant columns
    swda_df <- swda_df[, c("doc_id", "damsl_tag", "speaker_id", "speaker", "turn_num",
        "utterance_num", "utterance_text")]
    return(swda_df)
}
