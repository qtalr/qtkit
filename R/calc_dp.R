#' Calculate Gries' Deviation of Proportions (DP)
#'
#' This function is for internal use only.
#'
#' This function that calculates the Deviation of Proportions (DP)
#' based on Gries' Deviation of Proportions method. It is intended to be used
#' within the package, particularly by the `calc_dispersion_metrics` function.
#'
#' @param tdm_normalized A normalized term-document matrix (TDM) where each 
#' row represents a type and each column represents a document. The values 
#' should be the proportions of each type's frequency to its total frequency 
#' across all documents.
#'
#' @param corpus_parts A numeric vector containing the proportions of each 
#' document in the corpus, which is used to calculate the Deviation of 
#' Proportions (DP).
#'
#' @return A numeric vector containing the Deviation of Proportions (DP) for 
#' each type in the TDM.
#'
#' @keywords internal
#'
#' @importFrom Matrix rowSums
calc_dp <- function(tdm_normalized, corpus_parts) {
  # Check if tdm_normalized is a 'dgCMatrix' object
  if (!inherits(tdm_normalized, "dgCMatrix")) {
    stop("The argument 'tdm_normalized' must be a 'dgCMatrix' object.")
  }
  # Check if the matrix has both rows and columns
  if (nrow(tdm_normalized) == 0 || ncol(tdm_normalized) == 0) {
    stop("The matrix 'tdm_normalized' must have both rows and columns.")
  }

  # Check if corpus_parts is a numeric vector
  if (!is.numeric(corpus_parts)) {
    stop("The argument 'corpus_parts' must be a numeric vector.")
  }

  # Check if length of corpus_parts matches the number of columns in tdm_normalized
  if (length(corpus_parts) != ncol(tdm_normalized)) {
    stop("The length of 'corpus_parts' must match the number of columns in 'tdm_normalized'.")
  }

  diffs <- t(apply(tdm_normalized, 1, function(row) {
    abs(row - corpus_parts)
  }))
  dp <- Matrix::rowSums(diffs) / 2
  dp_norm <- dp / (1 - min(corpus_parts))

  return(dp_norm)
}
