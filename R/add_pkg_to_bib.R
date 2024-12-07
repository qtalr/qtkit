#' Add Package Citations to BibTeX File
#'
#' Adds citation information for R packages to a BibTeX file. Uses the
#' `knitr::write_bib` function to generate and append package citations
#' in BibTeX format.
#'
#' @param pkg_name Character string. The name of the R package to add to
#'        the BibTeX file.
#' @param bib_file Character string. The path and name of the BibTeX file
#'        to write to. Default is "packages.bib".
#'
#' @return Invisible NULL. The function is called for its side effect of
#'         writing to the BibTeX file.
#'
#' @details The function will create the BibTeX file if it doesn't exist,
#'          or append to it if it does. It includes citations for both the
#'          specified package and all currently loaded packages.
#'
#' @examples
#' # Create a temporary BibTeX file
#' my_bib_file <- tempfile(fileext = ".bib")
#'
#' # Add citations for dplyr package
#' add_pkg_to_bib("dplyr", my_bib_file)
#'
#' # View the contents of the BibTeX file
#' readLines(my_bib_file) |> cat(sep = "\n")
#'
#' @importFrom knitr write_bib
#'
#' @export
add_pkg_to_bib <- function(pkg_name, bib_file = "packages.bib") {
  knitr::write_bib(c(.packages(), pkg_name), file = bib_file)
}
