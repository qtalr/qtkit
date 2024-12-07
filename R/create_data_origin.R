#' Create Data Origin Documentation
#'
#' @description
#' Creates a standardized data origin documentation file in CSV format,
#' containing essential metadata about a dataset's source, format, and usage
#' rights.
#'
#' @details
#' Generates a template with the following metadata fields:
#' - Resource name
#' - Data source (URL/DOI)
#' - Sampling frame (language, modality, genre)
#' - Collection dates
#' - Data format
#' - Schema description
#' - License information
#' - Attribution requirements
#'
#' @param file_path Character string. Path where the CSV file should be saved.
#' @param return Logical. If TRUE, returns the data frame in addition to saving.
#'        Default is FALSE.
#' @param force Logical. If TRUE, overwrites existing file at path.
#'        Default is FALSE.
#'
#' @return If return=TRUE, returns a data frame containing the data origin
#'         template. Otherwise returns invisible(NULL).
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
