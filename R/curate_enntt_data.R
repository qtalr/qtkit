#' Curate ENNTT Data
#'
#' This function curates ENNTT data from a specified directory. It searches
#' for files with .dat or .tok extensions, extracts the unique types from these
#' files, and then curates each type of data using the `curate_enntt_file()`
#' function.
#' The curated data from each type is then combined into a single data frame
#' which is returned.
#'
#' @param dir_path A string. The path to the directory containing the data
#' files.
#'
#' @return A data frame. The curated ENNTT data.
#'
#' @examples
#' \dontrun{
#' curated_data <- curate_enntt_data("/path/to/data_dir/")
#' }
#'
#' @importFrom xml2 xml_attr xml_find_all read_html
#' @importFrom purrr map_dfr
#' @importFrom tibble as_tibble
#'
#' @export
curate_enntt_data <- function(dir_path) {
  files <- basename(list.files(
    path = dir_path, pattern = "(dat|tok)$", full.names = TRUE,
    recursive = TRUE
  ))
  types <- unique(unlist(lapply(files, function(x) sub("\\..*$", "", x))))
  data_df <- purrr::map_dfr(types, function(type) {
    curate_enntt_file(dir_path, type)
  }, .progress = TRUE) |>
    as_tibble()
  return(data_df)
}

curate_enntt_file <- function(dir_path, corpus_type) {
  # Create file paths
  dat_file <- paste0(dir_path, "/", corpus_type, ".dat")
  tok_file <- paste0(dir_path, "/", corpus_type, ".tok")
  # Read in files by lines
  dat <- xml2::read_html(dat_file)
  dat <- xml2::xml_find_all(dat, "//line")
  tok <- readLines(tok_file)
  # Apply function to all line nodes
  dat_attrs <- lapply(dat, extract_dat_attrs)
  # Combine attrs and text into data frame
  dat_attrs <- do.call(rbind, dat_attrs)
  dataset <- cbind(dat_attrs, text = tok)
  # Add type column with value of file name (w/o extension)
  dataset$type <- corpus_type
  # Return data frame
  return(dataset)
}

# Create function to extract attributes
extract_dat_attrs <- function(line_node) {
  session_id <- xml2::xml_attr(line_node, "session_id")
  speaker_id <- xml2::xml_attr(line_node, "mepid")
  state <- xml2::xml_attr(line_node, "state")
  session_seq <- xml2::xml_attr(line_node, "seq_speaker_id")
  data.frame(
    session_id,
    speaker_id,
    state,
    session_seq,
    stringsAsFactors = FALSE
  )
}
