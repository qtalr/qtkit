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
#' @importFrom Matrix rowSums

# Calculate relative frequency (RF)
calc_rf <- function(tdm) {
  row_sums <- Matrix::rowSums(tdm)
  rf <- row_sums / sum(tdm)
  return(rf)
}

# Calculate observed relative frequency (ORF)
calc_orf <- function(tdm) {
  orf <- calc_rf(tdm) * 100
  return(orf)
}


# Calculate document frequency (DF)
calc_df <- function(tdm) {
  df <- Matrix::rowSums(tdm > 0)
  return(df)
}

# Calculate the inverse document frequency (IDF)
calc_idf <- function(tdm) {
  df <- calc_df(tdm)
  idf <- log(ncol(tdm) / df)
  return(idf)
}

# Calculate Gries' Deviation of Proportions (DP)
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
