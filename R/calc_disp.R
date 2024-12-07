#' Dispersion and frequency functions

#' These functions are for internal use only.
#'
#' To be used with the `calc_type_metrics` function.
#' Includes dispersion measures: `calc_df()` Document Frequency 'DF',
#' `calc_idf()` Inverse Document Frequency 'IDF', and `calc_dp()` Gries'
#' Deviation of Proportions 'DP' functions. Frequency measures include:
#' `calc_rf()` and `calc_orf()`. Takes a term-document matrix (TDM) as
#' input and returns the respective dispersion metrics for each type in the TDM.
#'
#' @keywords internal
#'

#' Calculate Relative Frequency
#'
#' Computes the relative frequency (RF) for each term in a term-document matrix.
#' RF is calculated as the sum of occurrences divided by total corpus size.
#'
#' @param tdm A term-document matrix
#'
#' @return A numeric vector of relative frequencies for each term
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
