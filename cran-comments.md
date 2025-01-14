## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

- This is an update from version 1.1.0 to 1.1.1.

## Downstream dependencies

There are currently no downstream dependencies for this package.

## Changes since last version

- Bug fix (#9): `create_data_dictionary()` now catches errors when the \
  AI model fails to produce a CSV output that will parse correctly. A warning \
  is issued and the function returns a data dictionary without the AI model. \
  _Note: In testing, gpt-3.5-turbo and gpt-4 have been found to be the most \
  reliable models for this task._
- Bug fix: `calc_assoc_metrics()` now uses {dplyr} functions to perform more \
efficient calculations. Previously, the function was using base R functions \
to calculate the metrics, and memory usage was high for large datasets.

## Test environments

- Local macOS 15.3 (aarch64), R 4.1.1
- GitHub Actions (ubuntu-latest): devel, release, oldrel-1 (windows-latest, macOS-latest)
- win-builder (devel and release)

## Additional comments

- This package requires a Chromium-based browser (e.g., Chrome, Chromium, or Brave) for full functionality. This requirement is now stated in the SystemRequirements field of the DESCRIPTION file.
- The package includes checks for the presence of a suitable browser and provides informative messages if one is not found.
- Users can set the CHROMOTE_CHROME environment variable to specify the location of their Chrome-based browser executable if it's not automatically detected.
