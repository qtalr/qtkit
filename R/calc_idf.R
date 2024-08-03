#' Calculate Inverse Document Frequency (IDF)
#'
#' This function is for internal use only.
#'
#' This function that calculates the Inverse Document Frequency 'IDF' 
#' for each type (e.g., term, lemma) in a term-document matrix (TDM). 
#' It is intended to be used within the package by the 
#' `calc_dispersion_metrics` function.
#'
#' @param tdm A term-document matrix (TDM) where each row represents a 
#' type and each column represents a document.
#'
#' @return A numeric vector containing the Inverse Document Frequency 
#' 'DF' for each type in the TDM.
#'
#' @keywords internal
#'
#' @importFrom Matrix rowSums
# TODO: Implement the 'calc_idf' function 
calc_idf <- function(tdm) {
  # Check if tdm is a 'dgCMatrix' object
  if (!inherits(tdm, "dgCMatrix")) {
    stop("The argument 'tdm' must be a 'dgCMatrix' object.")
  }
  # Check if the matrix has both rows and columns
  if (nrow(tdm) == 0 || ncol(tdm) == 0) {
    stop("The matrix 'tdm' must have both rows and columns.")
  }
  df <- Matrix::rowSums(tdm > 0)
  idf <- log(ncol(tdm) / df)

  return(idf)
}
