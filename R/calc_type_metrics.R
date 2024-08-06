#' Calculate Type Metrics for Text Data
#'
#' This function calculates type metrics for tokenized text data.
#'
#' @param data A data frame containing the tokenized text data
#' @param type The variable in `data` that contains the type
#'             (e.g., term, lemma) to analyze.
#' @param document The variable in `data` that contains the document IDs.
#' @param frequency A character vector indicating which
#'                  frequency metrics to use. If NULL (default),
#'                  only the `type` and `n` are returned.
#'                  Other options:
#'                  'all',
#'                  'rf' calculates relative frequency,
#'                  'orf' calculates observed relative frequency.
#'                  Can specify multiple options: c("rf", "orf").
#' @param dispersion A character vector indicating which
#'                   dispersion metrics to use. If NULL (default),
#'                   only the `type` and `n` are returned.
#'                   Other options:
#'                  'all',
#'                  'df' calculates Document Frequency.
#'                  'idf' calculates Inverse Document Frequency.
#'                  'dp' calculates Gries' Deviation of Proportions.
#'                  Can specify multiple options: c("df", "idf").
#'
#' @return A data frame with columns:
#'   - `type`: The unique types from the input data.
#'   - `n`: The frequency of each type across all document.
#'   Optionally (based on the `frequency` and `dispersion` arguments):
#'   - `rf`: The relative frequency of each type across all document.
#'   - `orf`: The observed relative frequency (per 100) of each
#'            type across all document.
#'   - `df`: The document frequency of each type.
#'   - `idf`: The inverse document frequency of each type.
#'   - `dp`: Gries' Deviation of Proportions of each type.
#'
#' @references
#' Gries, Stefan Th. (2023). Statistical Methods in Corpus Linguistics.
#' In Readings in Corpus Linguistics: A Teaching and Research Guide
#' for Scholars in Nigeria and Beyond, pp. 78-114.
#'
#' @examples
#' data_path <- system.file("extdata", "types_data.rds", package = "qtkit")
#' data <- readRDS(data_path)
#' calc_type_metrics(
#'   data = data,
#'   type = type,
#'   document = document,
#'   frequency = c("rf", "orf"),
#'   dispersion = c("df", "idf")
#' )
#'
#' @importFrom rlang ensym as_string
#' @importFrom dplyr count
#' @importFrom tidytext cast_sparse
#' @importFrom tibble tibble
#' @export
calc_type_metrics <-
  function(
    data,
    type,
    document,
    frequency = NULL,
    dispersion = NULL) {
  # Validate inputs
  validate_inputs_ctm(data, type, document, frequency, dispersion)

  # Create a Sparse Term-Document Matrix (TDM)
  tdm <- data |>
    dplyr::count({{ type }}, {{ document }}) |>
    tidytext::cast_sparse({{ type }}, {{ document }}, n)

  # Initialize an empty data frame
  row_sums <- Matrix::rowSums(tdm)
  output_df <- tibble::tibble(type = rownames(tdm), n = row_sums)

  # Calculate metrics based on user choice
  metrics <- c("rf", "orf", "df", "idf", "dp")
  for (metric in metrics) {
    if ("all" %in% frequency ||
      metric %in% frequency ||
      "all" %in% dispersion ||
      metric %in% dispersion) {
      output_df[[metric]] <-
        get(paste0("calc_", metric))(tdm)
    }
  }

  return(output_df)
}

validate_inputs_ctm <- function(data, type, document, frequency, dispersion) {
  # Check if data is a data.frame
  if (!is.data.frame(data)) {
    stop("The argument 'data' must be a data frame.")
  }

  # Ensure that type and document are symbols
  type <- rlang::ensym(type)
  document <- rlang::ensym(document)

  # Convert type and document to strings for checking
  type_str <- rlang::as_string(type)
  document_str <- rlang::as_string(document)

  # Check if type and document exist in data
  if (!all(c(type_str, document_str) %in% names(data))) {
    stop("The variables specified in 'type' and 'document' must exist in
      'data'.")
  }

  # If frequency is not NULL,
  # check if it's a character vector and if all of its values are allowed
  if (!is.null(frequency) &&
    (!is.character(frequency) ||
      !all(frequency %in% c("all", "rf", "orf")))) {
    stop("The argument 'frequency' must be a character vector containing any
      combination of: 'rf', 'orf' or 'all' ")
  }

  # If dispersion is not NULL,
  # check if it's a character vector and if all of its values are allowed
  if (!is.null(dispersion) &&
    (!is.character(dispersion) ||
      !all(dispersion %in% c("all", "df", "idf", "dp")))) {
    stop("The argument 'dispersion' must be a character vector containing any
      combination of: 'df', 'idf', 'dp' or 'all'.")
  }
}

utils::globalVariables(c("calc_df", "calc_idf", "calc_dp", "n"))
