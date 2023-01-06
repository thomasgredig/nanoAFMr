.onAttach <- function(libname, pkgname) {
  if (runif(1) > 0.7) packageStartupMessage(paste("Please cite", pkgname,',see https://doi.org/10.5281/zenodo.7464877'))
}