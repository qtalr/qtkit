#' Calculate Frequency and Dispersion Metrics for Text Types
#'
#' Calculates various frequency and dispersion metrics for types (terms/tokens)
#' in tokenized text data. Provides a comprehensive analysis of how types are
#' distributed across documents in a corpus.
#'
#' @param data Data frame. Contains the tokenized text data with document IDs
#'        and types/terms.
#' @param type Symbol. Column in `data` containing the types to analyze
#'        (e.g., terms, lemmas).
#' @param document Symbol. Column in `data` containing the document identifiers.
#' @param frequency Character vector. Frequency metrics to calculate:
#'        - NULL (default): Returns only type counts
#'        - 'all': All available metrics
#'        - 'rf': Relative frequency
#'        - 'orf': Observed relative frequency (per 100)
#' @param dispersion Character vector. Dispersion metrics to calculate:
#'        - NULL (default): Returns only type counts
#'        - 'all': All available metrics
#'        - 'df': Document frequency
#'        - 'idf': Inverse document frequency
#'        - 'dp': Gries' deviation of proportions
#'
#' @return Data frame containing requested metrics:
#' - type: Unique types from input data
#' - n: Raw frequency count
#' - rf: Relative frequency (if requested)
#' - orf: Observed relative frequency per 100 (if requested)
#' - df: Document frequency (if requested)
#' - idf: Inverse document frequency (if requested)
#' - dp: Deviation of proportions (if requested)
#'
#' @details
#' The function creates a term-document matrix internally and calculates the
#' requested metrics. Frequency metrics show how often types occur, while
#' dispersion metrics show how evenly they are distributed across documents.
#'
#' The 'dp' metric (Gries' Deviation of Proportions) ranges from 0 (perfectly
#' even distribution) to 1 (completely clumped distribution).
#'
#' @references
#' Gries, Stefan Th. (2023). Statistical Methods in Corpus Linguistics.
#' In Readings in Corpus Linguistics: A Teaching and Research Guide
#' for Scholars in Nigeria and Beyond, pp. 78-114.
#'
#' @examples
#' data_path <- system.file("extdata", "types_data.rds", package = "qtkit")
#' df <- readRDS(data_path)
#' calc_type_metrics(
#'   data = df,
#'   type = letter,
#'   document = doc_id,
#'   frequency = c("rf", "orf"),
#'   dispersion = "dp"
#' )
#' @importFrom rlang as_label enquo
#' @importFrom dplyr count
#' @importFrom tidytext cast_sparse
#' @export
calc_type_metrics <-
  function(data, type, document, frequency = NULL, dispersion = NULL) {
    # Validate inputs
    validate_inputs_ctm(data, {{ type }}, {{ document }}, frequency, dispersion)
    # Create a Sparse Term-Document Matrix (TDM)
    tdm <-
      data |>
      dplyr::count({{ type }}, {{ document }}) |>
      tidytext::cast_sparse({{ type }}, {{ document }}, n)
    # Initialize an empty data frame
    row_sums <- Matrix::rowSums(tdm)
    output_df <- data.frame(type = rownames(tdm), n = row_sums, 
                           stringsAsFactors = FALSE)
    # Calculate frequency metrics based on user choice
    metrics <- c("rf", "orf")
    for (metric in metrics) {
      if ("all" %in% frequency || metric %in% frequency) {
        output_df[[metric]] <- get(paste0("calc_", metric))(tdm)
      }
    }
    # Calculate dispersion metrics based on user choice
    metrics <- c("df", "idf", "dp")
    for (metric in metrics) {
      if ("all" %in% dispersion || metric %in% dispersion) {
        output_df[[metric]] <- get(paste0("calc_", metric))(tdm)
      }
    }
    return(output_df)
  }

#' Validate Inputs for Type Metrics Calculation
#'
#' Helper function that validates the input parameters for the calc_type_metrics
#' function. Checks data frame structure, column existence, and metric
#' specifications.
#'
#' @param data A data frame to validate
#' @param type Column name for the type/term variable
#' @param document Column name for the document ID variable
#' @param frequency Character vector of requested frequency metrics
#' @param dispersion Character vector of requested dispersion metrics
#'
#' @return No return value, called for side effects
#'
#' @details Stops execution with error message if:
#'   - data is not a data frame
#'   - required columns are missing
#'   - frequency contains invalid metric names
#'   - dispersion contains invalid metric names
#'
#' @keywords internal
validate_inputs_ctm <- function(data, type, document, frequency, dispersion) {
  # Check if data is a data.frame
  if (!is.data.frame(data)) {
    stop("The argument 'data' must be a data frame.")
  }
  # Convert type and document to strings
  type <- rlang::as_label(rlang::enquo(type))
  document <- rlang::as_label(rlang::enquo(document))
  # Check if type and document exist in data
  if (!all(c(type, document) %in% names(data))) {
    stop("The variables specified in 'type' and 'document' must exist in
      'data'.")
  }
  # If frequency is not NULL, check if it's a character vector and if all of
  # its values are allowed
  if (!is.null(frequency) && (!is.character(frequency) || !all(frequency %in% c(
    "all",
    "rf", "orf"
  )))) {
    stop("The argument 'frequency' must be a character vector containing any
      combination of: 'rf', 'orf' or 'all' ")
  }
  # If dispersion is not NULL, check if it's a character vector and if all of
  # its values are allowed
  if (!is.null(dispersion) &&
    (!is.character(dispersion) ||
      !all(dispersion %in% c("all", "df", "idf", "dp")))) {
    stop("The argument 'dispersion' must be a character vector containing any
      combination of: 'df', 'idf', 'dp' or 'all'.")
  }
}

utils::globalVariables(c("calc_df", "calc_idf", "calc_dp", "n"))
