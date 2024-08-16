#' Identify Outliers in a Numeric Variable
#'
#' This function identifies outliers in a numeric variable of a data.frame
#' using the interquartile range (IQR) method.
#'
#' @param data A data.frame object.
#' @param variable_name A symbol representing a numeric variable in `data`.
#'
#' @return A data.frame containing the outliers in `variable_name`.
#' If no outliers are found, the function returns `NULL`. The
#' function also prints diagnostic information about the
#' variable and the number of outliers found.
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
