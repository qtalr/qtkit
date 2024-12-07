#' Calculate Normalized Entropy for Categorical Variables
#'
#' @description
#' Computes the normalized entropy (uncertainty measure) for categorical variables,
#' providing a standardized measure of dispersion or randomness in the data.
#'
#' @details
#' The function:
#' - Handles both character vectors and factors as input
#' - Treats NA values as a separate category
#' - Normalizes entropy to range [0,1] where:
#'   * 0 indicates complete certainty (one category dominates)
#'   * 1 indicates maximum uncertainty (equal distribution)
#' 
#' The calculation process:
#' 1. Computes category proportions
#' 2. Calculates raw entropy using Shannon's formula
#' 3. Normalizes by dividing by maximum possible entropy
#'
#' @param x A character vector or factor containing categorical data.
#'
#' @return A numeric value between 0 and 1 representing the normalized entropy:
#' - Values closer to 0 indicate less diversity/uncertainty
#' - Values closer to 1 indicate more diversity/uncertainty
#' @examples
#' # Calculate entropy for a simple categorical vector
#' x <- c("A", "B", "B", "C", "C", "C", "D", "D", "D", "D")
#' calc_normalized_entropy(x)
#'
#' # Handle missing values
#' y <- c("A", "B", NA, "C", "C", NA, "D", "D")
#' calc_normalized_entropy(y)
#'
#' # Works with factors too
#' z <- factor(c("Low", "Med", "Med", "High", "High", "High"))
#' calc_normalized_entropy(z)
#'
#' @export
calc_normalized_entropy <- function(x) {
  # Check that x is a character vector or factor
  if (!is.character(x) && !is.factor(x)) {
    stop("Input must be a character vector or factor.")
  }
  # Add an 'NA' level to the factor, if necessary
  x <- addNA(x, ifany = TRUE)
  # Calculate the proportion of each category
  prop <- prop.table(table(x))
  # Calculate the entropy
  entropy <- -sum(prop * log2(prop))
  # Calculate the maximum entropy
  max_entropy <- log2(length(prop))
  # Calculate the normalized entropy
  normalized_entropy <- entropy / max_entropy
  # Return the normalized entropy
  return(normalized_entropy)
}
