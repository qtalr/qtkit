#' Calculate Association Metrics for Bigrams
#'
#' This function calculates various association metrics
#' (PMI, Dice's Coefficient, G-score) for bigrams in a given corpus.
#'
#' @param data A data frame containing the corpus.
#' @param doc_index Column in 'data' which represents the document index.
#' @param token_index Column in 'data' which represents the token index.
#' @param type Column in 'data' which represents the tokens or terms.
#' @param association A character vector specifying which metrics to calculate.
#' Can be any combination of 'pmi', 'dice_coeff', 'g_score', or 'all'.
#' Default is 'all'.
#' @param verbose A logical value indicating whether to keep the intermediate
#' probability columns. Default is FALSE.
#'
#' @return A data frame with one row per bigram and columns for each
#' calculated metric.
#'
#' @examples
#' data_path <- system.file("extdata", "bigrams_data.rds", package = "qtkit")
#' data <- readRDS(data_path)
#'
#' calc_assoc_metrics(data, doc_index, token_index, type)
#'
#' @importFrom rlang ensym as_string

#' @export
calc_assoc_metrics <-
  function(
      data,
      doc_index,
      token_index,
      type,
      association = "all",
      verbose = FALSE) {
    doc_index <- rlang::ensym(doc_index)
    token_index <- rlang::ensym(token_index)
    type <- rlang::ensym(type)
    validate_inputs_cam(data, doc_index, token_index, type, association)

    bigram_probs <-
      calculate_bigram_probabilities(data, doc_index, token_index, type)

    metrics <- calculate_metrics(bigram_probs, association)

    if (!verbose) {
      metrics <- metrics[, !colnames(metrics) %in% c("p_xy", "p_x", "p_y")]
      return(metrics)
    } else {
      return(metrics)
    }
  }


#' Calculate Probabilities for Bigrams
#'
#' Helper function that calculates joint and marginal probabilities for bigrams
#' in the input data. It processes the data to create bigrams and computes
#' their probabilities along with individual token probabilities.
#'
#' @param data A data frame containing the corpus
#' @param doc_index Column name for document index
#' @param token_index Column name for token position
#' @param type Column name for the actual tokens/terms
#'
#' @return A data frame containing:
#'   \item{x}{First token in bigram}
#'   \item{y}{Second token in bigram}
#'   \item{p_xy}{Joint probability of the bigram}
#'   \item{p_x}{Marginal probability of first token}
#'   \item{p_y}{Marginal probability of second token}
#'
#' @keywords internal
calculate_bigram_probabilities <-
  function(data, doc_index, token_index, type) {
    # Sort data by document and token
    data <- data[order(data[[doc_index]], data[[token_index]]), ]
    # Create bigrams
    x <- data[[type]]
    y <- c(x[-1], NA)
    bigrams <- data.frame(x = x, y = y)
    bigrams <- bigrams[!is.na(bigrams$y), ]
    # Count bigrams
    bigram_counts <- table(bigrams)
    total_bigrams <- sum(bigram_counts)
    # Calculate probabilities
    p_xy <- as.data.frame(bigram_counts / total_bigrams)
    colnames(p_xy) <- c("x", "y", "p_xy")
    # Calculate unigram probabilities
    p_x <- as.data.frame(table(x) / length(x))
    colnames(p_x) <- c("x", "p_x")
    p_y <- as.data.frame(table(y) / length(y))
    colnames(p_y) <- c("y", "p_y")
    # Merge probabilities
    result <- merge(p_xy, p_x, by = "x", all.x = TRUE)
    result <- merge(result, p_y, by = "y", all.x = TRUE)
    # Convert to numeric
    result$p_xy <- as.numeric(result$p_xy)
    result$p_x <- as.numeric(result$p_x)
    result$p_y <- as.numeric(result$p_y)
    return(result)
  }

#' Calculate Association Metrics
#'
#' Helper function that computes various association metrics for bigrams based on
#' their probability distributions. Supports PMI (Pointwise Mutual Information),
#' Dice's Coefficient, and G-score calculations.
#'
#' @param bigram_probs A data frame containing bigram probability data with columns:
#'   \item{p_xy}{Joint probability of bigram}
#'   \item{p_x}{Marginal probability of first token}
#'   \item{p_y}{Marginal probability of second token}
#' @param association Character vector specifying which metrics to calculate
#'
#' @return A data frame containing the original probability columns plus requested
#' association metrics:
#'   \item{pmi}{Pointwise Mutual Information}
#'   \item{dice_coeff}{Dice's Coefficient}
#'   \item{g_score}{G-score}
#'
#' @keywords internal
calculate_metrics <-
  function(bigram_probs, association) {
    metrics <- bigram_probs
    if ("all" %in% association || "pmi" %in% association) {
      metrics$pmi <- log(metrics$p_xy / (metrics$p_x * metrics$p_y))
    }
    if ("all" %in% association || "dice_coeff" %in% association) {
      metrics$dice_coeff <- 2 * metrics$p_xy / (metrics$p_x + metrics$p_y)
    }
    if ("all" %in% association || "g_score" %in% association) {
      metrics$g_score <- 2 * log(metrics$p_xy) -
        log(metrics$p_x) - log(metrics$p_y)
    }
    return(metrics)
  }


#' Validate Inputs for Association Metrics Calculation
#'
#' Helper function that validates the input parameters for the calc_assoc_metrics
#' function. Checks data frame structure, column existence, and association metric
#' specifications.
#'
#' @param data A data frame to validate
#' @param doc_index Column name for document index
#' @param token_index Column name for token position
#' @param type Column name for the tokens/terms
#' @param association Character vector of requested association metrics
#'
#' @return No return value, called for side effects
#'
#' @details Stops execution with error message if:
#'   \item{data is not a data frame}
#'   \item{required columns are missing}
#'   \item{association contains invalid metric names}
#'
#' @keywords internal
validate_inputs_cam <-
  function(data, doc_index, token_index, type, association) {
    if (!is.data.frame(data)) {
      stop("The argument 'data' must be a data frame.")
    }

    doc_index_str <- rlang::as_string(doc_index)
    token_index_str <- rlang::as_string(token_index)
    type_str <- rlang::as_string(type)

    required_cols <- c(doc_index_str, token_index_str, type_str)
    if (!all(required_cols %in% names(data))) {
      stop("All specified columns must exist in 'data'.")
    }

    valid_associations <- c("all", "pmi", "dice_coeff", "g_score")
    if (!is.character(association) ||
      !all(association %in% valid_associations)) { # nolint
      stop("Invalid 'association' argument.")
    }
  }
