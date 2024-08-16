#' Get Works from Project Gutenberg
#'
#' Retrieves works from Project Gutenberg based on specified criteria
#' and saves the data to a CSV file. This function is a wrapper for the
#' gutenbergr package.
#'
#' @param target_dir The directory where the CSV file will be saved.
#' @param lcc_subject A character vector specifying the Library of Congress
#' Classification (LCC) subjects to filter the works.
#' @param birth_year An optional integer specifying the minimum birth year of
#' authors to include.
#' @param death_year An optional integer specifying the maximum death year of
#' authors to include.
#' @param n_works An integer specifying the number of works to retrieve.
#' Default is 100.
#' @param force A logical value indicating whether to overwrite existing data
#' if it already exists.
#' @param confirmed If `TRUE`, the user has confirmed that they have
#' permission to use the data.
#' If `FALSE`, the function will prompt the user to confirm permission.
#' Setting this to `TRUE` is useful for reproducible workflows.
#'
#' @return A message indicating whether the data was acquired or already
#' existed on disk, writes the data files to disk in the specified target
#' directory.
#'
#' @details This function retrieves Gutenberg works based on the specified LCC
#' subjects and optional author birth and death years.
#' It checks if the data already exists in the target directory and provides
#' an option to overwrite it.
#' The function also creates the target directory if it doesn't exist.
#' If the number of works is greater than 1000 and the 'confirmed' parameter
#' is not set to TRUE, it prompts the user for confirmation.
#' The retrieved works are filtered based on public domain rights in the USA
#' and availability of text.
#' The resulting works are downloaded and saved as a CSV file in the target
#' directory.
#'
#' For more information on Library of Congress Classification (LCC) subjects,
#' refer to the \href{https://www.loc.gov/catdir/cpso/lcco/}{Library of
#' Congress Classification Guide}.
#'
#' @examples
#' \dontrun{
#' data_dir <- file.path(tempdir(), "data")
#'
#' get_gutenberg_data(
#'  target_dir = data_dir,
#'  lcc_subject = "JC"
#'  n_works = 5,
#'  confirmed = TRUE)
#' }
#'
#' @import gutenbergr
#' @importFrom dplyr select mutate filter
#' @importFrom readr write_csv
#'
#' @export
get_gutenberg_data <-
  function(
    target_dir,
    lcc_subject,
    birth_year = NULL,
    death_year = NULL,
    n_works = 100,
    force = FALSE,
    confirmed = FALSE) {

  # Confirm permission to use the data
  confirmed <- confirm_if_needed(confirmed)
    if (!confirmed) {
      return(message("Aborted."))
    }

  # Parameter validation
  if (is.null(lcc_subject) || length(lcc_subject) == 0) {
    stop("lcc_subject must be provided and non-empty.")
  }

  # Create path to target_file from target_dir and lcc_subject
  file_name <- paste0("works_",
                      tolower(paste0(lcc_subject, collapse = "_")),
                      ".csv")
  target_file <- file.path(target_dir, file_name)

  # Check to see if the data already exists
  if (file.exists(target_file) && !force) {
    message("Data already exists at ", target_file,
            "\nUse 'force = TRUE' to overwrite existing data.")
    return(invisible())
  }

  # Ensure the directory exists
  dir.create(path = target_dir, recursive = TRUE, showWarnings = FALSE)

  # Get authors within years
  authors <- gutenbergr::gutenberg_authors

  if (!is.null(birth_year)) {
    authors <- authors |> dplyr::filter(birthdate > birth_year)
  }

  if (!is.null(death_year)) {
    authors <- authors |> dplyr::filter(deathdate < death_year)
  }

  # Get LCC subjects
  subjects <- gutenbergr::gutenberg_subjects |>
    dplyr::filter(subject_type == "lcc", subject %in% lcc_subject)

  # Get works based on authors and subjects
  works <- gutenbergr::gutenberg_metadata |>
    dplyr::filter(
      gutenberg_author_id %in% authors$gutenberg_author_id,
      gutenberg_id %in% subjects$gutenberg_id
    )

  # Download works
  results <- works |>
    dplyr::filter(rights == "Public domain in the USA.", has_text == TRUE) |>
    dplyr::slice_sample(n = n_works) |>
    gutenbergr::gutenberg_download(
      mirror = "https://gutenberg.pglaf.org/",
      meta_fields = c(
        "title",
        "author",
        "gutenberg_author_id",
        "gutenberg_bookshelf"),
      verbose = FALSE
    )

  # Organize works
  results <-
    results |>
    dplyr::mutate(
      lcc = lcc_subject
    ) |>
    dplyr::select(
      gutenberg_id,
      lcc,
      gutenberg_bookshelf,
      gutenberg_author_id,
      author,
      title,
      text
    )

  # Write works to disk
  readr::write_csv(results, file = target_file)
  message("Data saved to ", target_file)
}

utils::globalVariables(c(
  "birthdate",
  "deathdate",
  "subject_type",
  "subject",
  "gutenberg_author_id",
  "gutenberg_id",
  "rights",
  "has_text",
  "lcc",
  "gutenberg_bookshelf",
  "author",
  "title",
  "text"
))
