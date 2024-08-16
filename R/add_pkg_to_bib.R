#' Add package to BibTeX file
#'
#' This function adds a package to a BibTeX file. It uses the
#' `knitr::write_bib` function to write the package name to the file.
#'
#' @param pkg_name The name of the package to add to the BibTeX file.
#' @param bib_file The name of the BibTeX file to write to.
#'
#' @return NULL
#'
#' @importFrom knitr write_bib
#'
#' @examples
#' my_bib_file <- tempfile(fileext = ".bib")
#' add_pkg_to_bib("dplyr", my_bib_file)
#' readLines(my_bib_file) |> cat(sep = "\n")
#'
#' @export
add_pkg_to_bib <- function(pkg_name, bib_file = "packages.bib") {
  knitr::write_bib(c(.packages(), pkg_name), file = bib_file)
}
