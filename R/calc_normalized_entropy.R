#' Calculate Normalized Entropy
#' Calculate the normalized entropy for a categorical variable.
#'
#' This function takes a categorical variable as input and calculates the normalized entropy
#' of the variable. The normalized entropy is a measure of the amount of uncertainty or
#' randomness in the variable, normalized to the maximum possible entropy for the variable.
#'
#' @param x A categorical variable.
#' @return The normalized entropy of the variable.
#' @examples
#' \dontrun{
#' # Calculate the normalized entropy of a vector of categorical data
#' x <- c('A', 'B', 'B', 'C', 'C', 'C', 'D', 'D', 'D', 'D')
#' calc_normalized_entropy(x)
#' }
#'
#' @export
# TODO: calc_normalized_entropy function
calc_normalized_entropy <- function(x) {
    # Add an 'NA' level to the factor, if necessary
    x <- addNA(x, ifany = TRUE)

    # Calculate the proportion of each category
    prop <- prop.table(table(x))

    # Calculate the entropy
    entropy <- -sum(prop * log2(prop))

    # Calculate the maximum entropy
    max_entropy <- log2(length(prop))

    # Calculate the normalized entropy
    normalized_entropy <- entropy/max_entropy

    # Return the normalized entropy
    return(normalized_entropy)
}
