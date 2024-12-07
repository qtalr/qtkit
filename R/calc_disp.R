#' Internal Functions for Calculating Dispersion and Frequency Metrics
#'
#' @description
#' A collection of internal helper functions that calculate various dispersion
#' and frequency metrics from term-document matrices. These functions support
#' the main `calc_type_metrics` function by providing specialized calculations
#' for different statistical measures.
#'
#' @details
#' The package implements these metrics:
#'
#' Dispersion measures:
#' - Document Frequency (DF): Count of documents containing each term
#' - Inverse Document Frequency (IDF): Log-scaled inverse of DF, emphasizing rare terms
#' - Deviation of Proportions (DP): Gries' measure of distributional evenness
#'   ranging from 0 (perfectly even) to 1 (completely clumped)
#'
#' Frequency measures:
#' - Relative Frequency (RF): Term frequency normalized by total corpus size
#' - Observed Relative Frequency (ORF): RF expressed as percentage (RF * 100)
#'
#' Implementation notes:
#' - All functions expect a sparse term-document matrix input
#' - Matrix operations are optimized using the Matrix package
#' - NA values are handled appropriately for each metric
#' - Results are returned as numeric vectors
#'
#' @references
#' Gries, S. T. (2008). Dispersions and adjusted frequencies in corpora.
#' International Journal of Corpus Linguistics, 13(4), 403-437.
#'
#' @keywords internal
#'

#' Calculate Relative Frequency
#'
#' @description
#' Computes the relative frequency (RF) for each term in a term-document matrix,
#' representing how often each term occurs relative to the total corpus size.
#'
#' @details
#' The calculation process:
#' 1. Sums occurrences of each term across all documents
#' 2. Divides by total corpus size (sum of all terms)
#' 3. Returns proportions between 0 and 1
#'
#' @param tdm A sparse term-document matrix (Matrix package format)
#'
#' @return A numeric vector where each element represents a term's relative
#' frequency in the corpus (range: 0-1)
#'
#' @importFrom Matrix rowSums
#'
#' @keywords internal
calc_rf <- function(tdm) {
  row_sums <- Matrix::rowSums(tdm)
  rf <- row_sums / sum(tdm)
  return(rf)
}

#' Calculate Observed Relative Frequency
#'
#' Computes the observed relative frequency (ORF) for each term in a term-document matrix.
#' ORF is the relative frequency expressed as a percentage (RF * 100).
#'
#' @param tdm A term-document matrix
#'
#' @return A numeric vector of observed relative frequencies (as percentages) for each term
#'
#' @keywords internal
calc_orf <- function(tdm) {
  orf <- calc_rf(tdm) * 100
  return(orf)
}


#' Calculate Document Frequency
#'
#' Computes the document frequency (DF) for each term in a term-document matrix.
#' DF is the number of documents in which each term appears at least once.
#'
#' @param tdm A term-document matrix
#'
#' @return A numeric vector of document frequencies for each term
#'
#' @keywords internal
calc_df <- function(tdm) {
  df <- Matrix::rowSums(tdm > 0)
  return(df)
}

#' Calculate Inverse Document Frequency
#'
#' Computes the inverse document frequency (IDF) for each term in a term-document matrix.
#' IDF is calculated as log(N/df) where N is the total number of documents and df is
#' the document frequency of the term.
#'
#' @param tdm A term-document matrix
#'
#' @return A numeric vector of inverse document frequencies for each term
#'
#' @keywords internal
calc_idf <- function(tdm) {
  df <- calc_df(tdm)
  idf <- log(ncol(tdm) / df)
  return(idf)
}

#' Calculate Gries' Deviation of Proportions
#'
#' Computes the Deviation of Proportions (DP) measure developed by Stefan Th. Gries.
#' DP measures how evenly distributed a term is across all parts of the corpus.
#' The normalized version (DP_norm) is returned, which ranges from 0 (evenly distributed)
#' to 1 (extremely clumped distribution).
#'
#' @param tdm A term-document matrix
#'
#' @return A numeric vector of normalized DP values for each term
#'
#' @references
#' Gries, S. T. (2008). Dispersions and adjusted frequencies in corpora.
#' International Journal of Corpus Linguistics, 13(4), 403-437.
#'
#' @keywords internal
calc_dp <- function(tdm) {
  row_sums <- Matrix::rowSums(tdm)
  col_sums <- Matrix::colSums(tdm)
  corpus_parts <- col_sums / sum(tdm)
  tdm_normalized <- tdm / row_sums
  diffs <- t(apply(tdm_normalized, 1, function(row) {
    abs(row - corpus_parts)
  }))
  dp <- Matrix::rowSums(diffs) / 2
  dp_norm <- dp / (1 - min(corpus_parts))
  return(dp_norm)
}
