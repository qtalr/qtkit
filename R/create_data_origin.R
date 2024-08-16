#' Create data origin file
#'
#' Data frame with attributes about the data origin, written to a CSV file
#' and optionally returned.
#'
#' @param file_path File path where the data origin file should be saved.
#' @param return Logical value indicating whether the data origin should be
#' returned.
#' @param force Logical value indicating whether to overwrite the file if it
#' already exists.
#' @return A data frame containing the data origin information.
#' @export
#'
#' @examples
#' tmp_file <- tempfile(fileext = ".csv")
#' create_data_origin(tmp_file)
#' read.csv(tmp_file)
#' @importFrom utils write.csv
#'
create_data_origin <-
  function(
    file_path,
    return = FALSE,
    force = FALSE) {
  # Check to see if `file_path` is a character string
  if (!is.character(file_path)) stop("`file_path` must be a character string.")

  # Check to see if file exists at `file_path`
  if (file.exists(file_path) && !force) {
    stop("File already exists at `file_path`. Set `force = TRUE` to overwrite.")
  }

  # Create a data frame with the data origin information
  data_origin <- data.frame(
    attribute = c(
      "Resource name",
      "Data source",
      "Data sampling frame",
      "Data collection date(s)",
      "Data format",
      "Data schema",
      "License",
      "Attribution"
    ),
    description = c(
      "The name of the resource.",
      "URL, DOI, etc.",
      "Language, language variety, modality, genre, etc.",
      "The dates the data was collected.",
      ".txt, .csv, .xml, .html, etc.",
      "Relationships between data elements: files, folders, etc.",
      "CC BY, CC BY-SA, etc.",
      "Citation information."
    )
  )

  # Write the data origin to a file
  write.csv(data_origin, file = file_path, row.names = FALSE)

  # Return message to user
  message("Data origin file created at `file_path`.")

  # Return the data origin, if requested
  if (return) {
    return(data_origin)
  }
}
