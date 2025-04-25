#' Sample AFM image file names
#'
#' @description
#' returns sample AFM image files included with this library
#' 
#' @param type could be "ibw" for Asylum Research, "nid" for NanoSurf, 
#' "veeco" for Bruker/Veeco image,
#' "tiff" for Park, "force" for force spectroscopy, "resonance" for resonance curve,
#' if empty or "*", all files will be returned
#' 
#' @return vector with path/filename to AFM sample images
#' @author Thomas Gredig
#' 
#' @examples
#' file.list = AFM.getSampleImages()
#' print(paste("Found",length(file.list),"sample files."))
#' @export
AFM.getSampleImages <- function(type='*') {
  pfad = system.file("extdata",package="nanoAFMr")
  searchFile = switch(type,
                      force = "ForceCurve",
                      ibw = "ibw$",
                      nid = "20160301",
                      tiff = "^Park",
                      gredig = "^Park",
                      veeco= "0\\d\\d$",
                      resonance = "resonance",
                      "*")
  file.list = dir(pfad,pattern=searchFile)
  file.path(pfad, file.list)
}
