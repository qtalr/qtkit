# qtkit 1.1.0

- Bug fix (#7): `calc_type_metrics()` now correctly allows for the `type` and `document` arguments to be specified as symbols that can take values other than type and document.
- Adds `curate_enntt_data()` to curate the ENNTT data downloaded from GitHub [here](https://github.com/senisioi/enntt-release).
- Adds `curate_swda_data()` to curate the SWDA data downloaded from the LDC [here](https://catalog.ldc.upenn.edu/docs/LDC97S62/swb1_dialogact_annot.tar.gz).

# qtkit 1.0.0

- Adds `calc_assoc_metrics()` to calculate association metrics between two variables.
- Adds `calc_type_metrics()` to calculate dispersion and frequency metrics for a variable.
- Adds `create_data_dictionary()` to create a data dictionary for a dataset.
- Adds `find_outliers()` to identify observations in a data frame that have outliers for a given variable.
- Adds `get_gutenberg_data()` to download a dataset from Project Gutenberg.
- Adds `get_talkbank_data()` to download a dataset from TalkBank.
- Adds `add_pkg_to_bib()` to add a package to the bibliography.
- Adds `write_*()` functions to aid in publishing results to a variety of formats.
  - `write_gg()` writes a ggplot object to a file.
  - `write_kbl()` writes a knitr::kable object to a file.
  - `write_obj()` writes an R object to a file.
- Adds `find_outliers()` to identify observations in a data frame that has outliers for a given variable.

# qtkit 0.10.0

- Initial CRAN submission.
