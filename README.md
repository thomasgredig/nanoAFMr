# nanoAFMr — Load and Analyze Atomic Force Microscopy Data Files

[![DOI](https://zenodo.org/badge/580499036.svg)](https://zenodo.org/badge/latestdoi/580499036) [![CRAN status](https://www.r-pkg.org/badges/version/nanoAFMr)](https://CRAN.R-project.org/package=nanoAFMr)

A compact R package for importing, inspecting, and analyzing Atomic Force Microscopy (AFM) image data.

## Installation

Install from CRAN:

```r
install.packages("nanoAFMr")
```

Or install the development version from GitHub using remotes:

```r
install.packages("remotes")
remotes::install_github("thomasgredig/nanoAFMr")
```

## Basic example

```r
library(nanoAFMr)
# load a packaged sample image, import and plot
file <- AFM.getSampleImages("ibw")[1]
img <- AFM.import(file)
plot(img)
```

For more examples and longer workflows, see the package vignettes (if installed) or the man pages.

## CRAN submission notes

We ran unit tests and package checks during release preparation:

- testthat::test_local() (local test suite) — all tests passed in development (177 tests at time of writing).
- R CMD build && R CMD check --as-cran on a clean environment — please consult cran-comments.md for notes.

Known check items: some NOTEs were triggered by Suggests that are optional (e.g. latex2exp) in certain environments; these were documented for the CRAN submission.

## Contributing & Support

Please open issues or pull requests at: https://github.com/thomasgredig/nanoAFMr
For bug reports use: https://github.com/thomasgredig/nanoAFMr/issues

## License

This package is distributed under the AGPL (>= 3). See the DESCRIPTION file for details.

## Citation

To cite this package in publications, use:

```r
citation("nanoAFMr")
```

For further help, consult the man pages (help(package = "nanoAFMr")) or contact the maintainer listed in DESCRIPTION.

