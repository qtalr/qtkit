# qtkit 1.1.1

Bug fixes:

- Bug fix (#9): `create_data_dictionary()` now catches errors when the \
  AI model fails to produce a CSV output that will parse correctly. A warning \
  is issued and the function returns a data dictionary without the AI model. \
  _Note: In testing, gpt-3.5-turbo and gpt-4 have been found to be the most \
  reliable models for this task._

# qtkit 1.1.0

New features:

- Adds `curate_enntt_data()` to curate the ENNTT data downloaded from \
  GitHub [here](https://github.com/senisioi/enntt-release).
- Adds `curate_swda_data()` to curate the SWDA data downloaded from the \
  LDC [here](https://catalog.ldc.upenn.edu/docs/LDC97S62/swb1_dialogact_annot.tar.gz).

Bug fixes:

- Bug fix (#7): `calc_type_metrics()` now correctly allows for the `type` \
  and `document` arguments to be specified as symbols that can take values \
  other than type and document.

Enhancements:

- Adds vignettes for documenting data and using the publishing functions.
- Removes many external dependencies from the package (glue, purrr, readr, stringr, tibble, and tidytext) to reduce the number of dependencies and make the package more lightweight.

# qtkit 1.0.0

- Adds `calc_assoc_metrics()` to calculate association metrics between two \
  variables.
- Adds `calc_type_metrics()` to calculate dispersion and frequency metrics \
  for a variable.
- Adds `create_data_dictionary()` to create a data dictionary for a dataset.
- Adds `find_outliers()` to identify observations in a data frame that have \
  outliers for a given variable.
- Adds `get_gutenberg_data()` to download a dataset from Project Gutenberg.
- Adds `add_pkg_to_bib()` to add a package to the bibliography.
- Adds `write_*()` functions to aid in publishing results to a variety of \
  formats.
  - `write_gg()` writes a ggplot object to a file.
  - `write_kbl()` writes a knitr::kable object to a file.
  - `write_obj()` writes an R object to a file.
- Adds `find_outliers()` to identify observations in a data frame that has \
  outliers for a given variable.

Notes: `get_talkbank_data()` is postponed for a future release.

# qtkit 0.10.0

- Initial CRAN submission.
