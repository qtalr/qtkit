#' Create data origin file
#'
#' This function creates a data frame with attributes about the origin of the data,
#' writes it to a CSV file at the specified file path, and returns the data frame, if requested.
#'
#' @param file_path A character string specifying the file path where the data origin file should be saved.
#' @param return A logical value indicating whether the data origin should be returned.
#' @param force A logical value indicating whether to overwrite the file if it already exists.
#' @return A data frame containing the data origin information.
#' @export
#'
#' @examples
#' \dontrun{
#' create_data_origin("data_origin.csv")
#' }
#' @importFrom utils write.csv
#'
create_data_origin <- function(file_path, return = FALSE, force = FALSE) {
  # Check to see if `file_path` is a character string
  if (!is.character(file_path)) stop("`file_path` must be a character string.")

  # Check to see if file exists at `file_path`
  if (file.exists(file_path) && !force) {
    stop("File already exists at `file_path`. Set `force = TRUE` to overwrite.")
  }

  # Create a data frame with the data origin information
  data_origin <- data.frame(
    attribute = c("Resource name", "Data source", "Data sampling frame", "Data collection date(s)", "Data format", "Data schema", "License", "Attribution"),
    description = c("The name of the resource.", "URL, DOI, etc.", "Language, language variety, modality, genre, etc.", "The dates the data was collected.", ".txt, .csv, .xml, .html, etc.", "Relationships between data elements: files, folders, etc.", "CC BY, CC BY-SA, etc.", "Citation information.")
  )

  # Write the data origin to a file
  write.csv(data_origin, file = file_path, row.names = FALSE)

  # Return message
  message("Data origin file created at `file_path`.")

  # Return the data origin, if requested
  if (return) {
    return(data_origin)
  }
}
