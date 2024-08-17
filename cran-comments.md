## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

- This is an update from version 0.10.0 to 1.0.0.

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Changes since last version

- Adds `calc_assoc_metrics()` to calculate association metrics between two variables.
- Adds `calc_type_metrics()` to calculate dispersion and frequency metrics for a variable.
- Adds `create_data_dictionary()` to create a data dictionary for a dataset.
- Adds `find_outliers()` to identify observations in a data frame that have outliers for a given variable.
- Adds `get_gutenberg_data()` to download a dataset from Project Gutenberg.
- Adds `add_pkg_to_bib()` to add a package to the bibliography.
- Adds `write_*()` functions to aid in publishing results to a variety of formats.
  - `write_gg()` writes a ggplot object to a file.
  - `write_kbl()` writes a knitr::kable object to a file.
  - `write_obj()` writes an R object to a file.
- Adds `find_outliers()` to identify observations in a data frame that has outliers for a given variable.

## Test environments

- local macOS 14.5 (aarch64), R 4.1.1
- GitHub Actions (ubuntu-latest): devel, release, oldrel-1 (windows-latest, macOS-latest)
- win-builder (devel and release)

## Additional comments

- This package requires a Chromium-based browser (e.g., Chrome, Chromium, or Brave) for full functionality. This requirement is now stated in the SystemRequirements field of the DESCRIPTION file.
- The package includes checks for the presence of a suitable browser and provides informative messages if one is not found.
- Users can set the CHROMOTE_CHROME environment variable to specify the location of their Chrome-based browser executable if it's not automatically detected.
