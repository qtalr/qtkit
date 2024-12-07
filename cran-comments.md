## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

- This is an update from version 1.0.0 to 1.1.0.

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Changes since last version

- Adds `curate_enntt_data()` to curate the ENNTT data downloaded from GitHub [here](https://github.com/senisioi/enntt-release).
- Adds `curate_swda_data()` to curate the SWDA data downloaded from the LDC [here](https://catalog.ldc.upenn.edu/docs/LDC97S62/swb1_dialogact_annot.tar.gz).
- Bug fix (#7): `calc_type_metrics()` now correctly allows for the `type` and `document` arguments to be specified as symbols that can take values other than type and document.
- Adds vignettes for documenting data and using the publishing functions.
- Removes many external dependencies from the package (glue, purrr, readr, stringr, tibble, and tidytext) to reduce the number of dependencies and make the package more lightweight.

## Test environments

- Local macOS 15.2 (aarch64), R 4.1.1
- GitHub Actions (ubuntu-latest): devel, release, oldrel-1 (windows-latest, macOS-latest)
- win-builder (devel and release)

## Additional comments

- This package requires a Chromium-based browser (e.g., Chrome, Chromium, or Brave) for full functionality. This requirement is now stated in the SystemRequirements field of the DESCRIPTION file.
- The package includes checks for the presence of a suitable browser and provides informative messages if one is not found.
- Users can set the CHROMOTE_CHROME environment variable to specify the location of their Chrome-based browser executable if it's not automatically detected.
