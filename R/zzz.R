#' @importFrom utils packageVersion
.onAttach <- function(libname, pkgname) {
  if (runif(1) > 0.3) packageStartupMessage(
    paste("Please cite", pkgname, 
          packageVersion(pkgname),
          '- see https://doi.org/10.5281/zenodo.7464877'))
}