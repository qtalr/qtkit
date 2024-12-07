#' Detect Statistical Outliers Using IQR Method
#'
#' @description
#' Identifies statistical outliers in a numeric variable using the Interquartile
#' Range (IQR) method. Provides detailed diagnostics about the outlier detection
#' process.
#'
#' @details
#' The function uses the standard IQR method for outlier detection:
#' - Calculates Q1 (25th percentile) and Q3 (75th percentile)
#' - Computes IQR = Q3 - Q1
#' - Defines outliers as values outside [Q1 - 1.5*IQR, Q3 + 1.5*IQR]
#'
#' @param data Data frame containing the variable to analyze.
#' @param variable_name Unquoted name of the numeric variable to check for outliers.
#'
#' @return
#' If outliers are found:
#' - Data frame containing rows with outlier values
#' - Prints diagnostic information about quartiles and fences
#'
#' If no outliers:
#' - Returns NULL
#' - Prints confirmation message
#'
#' @section Diagnostic Output:
#' - Variable name
#' - Q1 and Q3 values
#' - IQR value
#' - Upper and lower fence values
#' - Number of outliers found
#'
#' @export
#'
#' @examples
#' data(mtcars)
#' find_outliers(mtcars, mpg)
#' find_outliers(mtcars, wt)
#'
#' @importFrom stats quantile
find_outliers <-
  function(
      data,
      variable_name) {
    # Check if `data` is a data.frame
    if (!is.data.frame(data)) {
      stop("The first argument must be a data.frame.")
    }

    # Check if `variable_name` is missing
    if (missing(variable_name)) {
      stop("The second argument must be unquoted.")
    }

    # Check if `variable_name` is a symbol
    if (!is.symbol(substitute(variable_name))) {
      stop("The second argument must be a symbol.")
    }

    # Get the variable name as a string
    var_name <- deparse(substitute(variable_name))

    # Check if `variable_name` exists in `data`
    if (!var_name %in% names(data)) {
      stop("The second argument must be a variable in the data.")
    }

    # Check if `variable_name` is numeric
    if (!is.numeric(data[[var_name]])) {
      stop("The second argument must be a numeric variable.")
    }

    # Calculate the quartiles using base R functions
    quartiles <- stats::quantile(data[[var_name]], probs = c(0.25, 0.75), na.rm = TRUE) # nolint
    q1 <- quartiles[1]
    q3 <- quartiles[2]

    # Calculate the interquartile range (IQR)
    iqr <- q3 - q1

    # Calculate the upper and lower fences
    upper_fence <- q3 + 1.5 * iqr
    lower_fence <- q1 - 1.5 * iqr

    # Filter the dataset by fences to identify outliers
    outlier_rows <- data[[var_name]] > upper_fence | data[[var_name]] < lower_fence # nolint

    # Remove NA values from consideration
    outlier_rows[is.na(outlier_rows)] <- FALSE

    # Diagnostic output
    message("Variable name: ", var_name)
    message("Q1: ", q1, " Q3: ", q3, " IQR: ", iqr)
    message("Upper fence: ", upper_fence, " Lower fence: ", lower_fence)
    message("Number of outliers: ", sum(outlier_rows))

    # Return outliers if any are found
    if (any(outlier_rows)) {
      outliers <- data[outlier_rows, , drop = FALSE]
      return(outliers)
    } else {
      message("No outliers found.")
      return(NULL)
    }
  }
