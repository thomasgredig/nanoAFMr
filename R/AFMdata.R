#' AFM image class
#'
#' A S4 class to store and manipulate images from Atomic Force Microscopes.
#' It contains the names of channels (exact naming depends on the type/instrument)
#' The class supports multiple images; it can also contain additional information
#' such as lines, etc.
#'
#' @slot data list with objects ($z is a list with images, $freq is a resonance curve)
#' @slot x.conv conversion factor from pixels to nm
#' @slot y.conv conversion factor from pixels to nm
#' @slot x.pixels number of pixels in x-direction
#' @slot y.pixels number of pixels in y-direction
#' @slot x.nm length of image
#' @slot y.nm height of image
#' @slot z.conv (not used)
#' @slot z.units vector with units for $z (deg, m)
#' @slot channel vector with names of channels
#' @slot instrument name of instrument (Park, Cypher, NanoSurf, Veeco)
#' @slot history history of file changes
#' @slot date date when file was created
#' @slot description AFM image description or note
#' @slot fullFilename name of file
AFMdata<-setClass("AFMdata",
                  slots = c(data="list",
                            x.conv="numeric",
                            y.conv="numeric",
                            x.pixels="numeric",
                            y.pixels="numeric",
                            x.nm = "numeric",
                            y.nm = "numeric",
                            z.conv = "numeric",
                            z.units = "character",
                            channel="character",
                            instrument="character",
                            history="character",
                            date = "character",
                            description="character",
                            fullFilename="character"
                  ),
                  validity =
                    function(object) {
                      errors <- character()

                      if (!(object@instrument %in% c('Cypher','Park','NanoSurf','Veeco','artificial'))) {
                        msg <- paste('Object has invalid instrument:',object@instrument)
                        errors <- c(errors,msg)
                      }
                      if (length(errors) == 0) TRUE else errors
                    }
                  )


#' Constructor method of AFMImage Class.
#'
#' @param .Object an AFMdata object
#' @param data list with objects ($z is a list with images, $freq is a resonance curve)
#' @param x.conv conversion factor from pixels to nm
#' @param y.conv conversion factor from pixels to nm
#' @param x.pixels number of pixels in x-direction
#' @param y.pixels number of pixels in y-direction
#' @param z.conv (not used)
#' @param z.units vector with units for z (deg, m)
#' @param channel vector with names of channels
#' @param instrument name of instrument (Park, Cypher, NanoSurf, Veeco)
#' @param history history of file changes
#' @param date date when file was created
#' @param description AFM image description or note
#' @param fullFilename name of file
#' @return initialized AFMdata object
#' @export
#' @importFrom methods setMethod initialize new validObject
setMethod(f="initialize",
          signature="AFMdata",
          definition= function(.Object,
                               data,
                               x.conv,
                               y.conv,
                               x.pixels,
                               y.pixels,
                               z.conv,
                               z.units ,
                               channel,
                               instrument,
                               history,
                               date,
                               description,
                               fullFilename)
          {
            if (!missing(data)) .Object@data<-data
            if (!missing(x.conv)) {.Object@x.conv<-x.conv; .Object@x.nm=round(x.conv*(x.pixels-1)); }
            if (!missing(y.conv)) {.Object@y.conv<-y.conv; .Object@y.nm=round(y.conv*(y.pixels-1)); }
            if (!missing(x.pixels)) .Object@x.pixels<-x.pixels
            if (!missing(y.pixels)) .Object@y.pixels<-y.pixels
            if (!missing(z.conv)) .Object@z.conv<-z.conv
            if (!missing(z.units)) .Object@z.units<-z.units
            if (!missing(channel)) .Object@channel <-channel
            if (!missing(instrument)) .Object@instrument <-instrument
            if (!missing(history)) .Object@history <-history
            if (!missing(date)) .Object@date <-date
            if (!missing(description)) .Object@description <-description
            if (!missing(fullFilename)) .Object@fullFilename<-fullFilename
            validObject(.Object)
            return(.Object)
          })




#' Initialize the AFMdata object
#'
#' @param data list with objects ($z is a list with images, $freq is a resonance curve)
#' @param x.conv conversion factor from pixels to nm
#' @param y.conv conversion factor from pixels to nm
#' @param x.pixels number of pixels in x-direction
#' @param y.pixels number of pixels in y-direction
#' @param z.conv (not used)
#' @param z.units vector with units for $z (deg, m)
#' @param channel vector with names of channels
#' @param instrument name of instrument (Park, Cypher, NanoSurf, Veeco)
#' @param history history of file changes
#' @param date date when file was created
#' @param description AFM image description or note
#' @param fullFilename name of file
#' @export
AFMdata <- function(data,
                    x.conv,
                    y.conv,
                    x.pixels,
                    y.pixels,
                    z.conv,
                    z.units ,
                    channel,
                    instrument,
                    history,
                    date="",
                    description="",
                    fullFilename) {
  return(new("AFMdata",
             data,
             x.conv,
             y.conv,
             x.pixels,
             y.pixels,
             z.conv,
             z.units ,
             channel,
             instrument,
             history,
             date,
             description,
             fullFilename))
}

# cat print
cpf <- function(...) cat(paste0(sprintf(...), "\n"))


#' Imports AFM file
#'
#' Use this function to create an AFMdata object from the filename;
#' four AFM formats (TIFF, NID, IBW, and 000) are supported. Use
#' \code{AFM.raster()} to create a data.frame from this object, or
#' use \code{plot()} to generate an image.
#'
#' @param filename name of AFM filename
#' @param verbose if \code{TRUE}, output additional information during loading of file
#' @return AFMdata object
#' @author Thomas Gredig
#' @importFrom ggplot2 ggplot aes
#' @examples
#' d = AFM.import(AFM.getSampleImages(type='ibw'))
#' summary(d)
#' plot(d)
#' 
#' @export
AFM.import <- function(filename, verbose=FALSE) {
  if (grepl('ibw$',filename)) obj = read.AR_file.v2(filename)
  else if (grepl('tiff$',filename)) obj = read.Park_file.v2(filename)
  else if (grepl('nid$',filename)) obj = read.NanoSurf_file.v2(filename)
  else if (grepl("\\d\\d\\d$", filename)) obj = read.Nanoscope.v2(filename)
  else {
    if (verbose) cat("Not a recognized AFM file.\n")
    return(NULL)
  }
  
  if (verbose) print(paste("Instrument:", obj@instrument))
  obj
}


#' Print AFMdata object
#'
#' prints essential information about the AFMdata object, which includes
#' the description, channel, image size, history, and filename
#'
#' @param x AFMdata object
#' @param ... other arguments
#' @return text with object information
#' @author Thomas Gredig
#' @examples
#' d = AFM.import(AFM.getSampleImages(type='ibw'))
#' print(d)
#' @export
print.AFMdata <- function(x, ...) {
  dataType = AFM.dataType(x)
  if(dataType=="image" || dataType == "spectroscopy") imageRes = paste(x@x.nm,"nm  x ",x@y.nm,'nm')
  if(dataType=="frequency") imageRes = paste(x@z.conv,x@z.units," - ",(x@z.conv + x@x.nm),x@z.units)

  cpf("Object      : %s AFM %s", x@instrument, dataType)
  cpf("Date Created: %s",        x@date)
  cpf("Description : %s",        x@description)
  cpf("Channel     : %s",        x@channel)
  cpf("Resolution  : %s",        imageRes)
  cpf("Pixels      : %s x %s",   x@x.pixels, x@y.pixels)
  cpf("History     : %s",        x@history)
  cpf("Filename    : %s",        x@fullFilename)
}

#' summary of AFMdata object
#'
#' @param object AFMdata object
#' @param ... other summary parameters
#' @return summary of AFMdata object
#' @author Thomas Gredig
#' 
#' @importFrom rlang is_empty
#' @examples
#' d = AFM.import(AFM.getSampleImages(type='ibw'))
#' summary(d)
#' @export
summary.AFMdata <- function(object,...) {
  if (is_empty(object@description)) object@description=""

  dataType = AFM.dataType(object)
  if(dataType == 'image' || dataType == 'spectroscopy') {
    r = data.frame(
      objectect = paste(object@instrument,dataType),
      description = paste(object@description),
      resolution = paste(object@x.pixels,"x",object@y.pixels),
      size = paste(object@x.nm,"x",round(object@y.nm),'nm'),
      channel = paste(object@channel),
      history = paste(object@history),
      date= paste(object@date)
    )
    for(i in seq_len(length(r$channel))) {
      d = AFM.raster(object,i)
      r$z.min[i] = min(d$z)
      r$z.max[i] = max(d$z)
    }
    r$z.units = paste(object@z.units)
  } else if (dataType == 'frequency') {
    r = data.frame(
      objectect = paste(object@instrument,dataType),
      description = paste(object@description),
      resolution = paste(object@x.pixels),
      size = paste(object@z.conv,"-",(object@z.conv+object@x.nm)),
      channel = paste(object@channel),
      history = paste(object@history),
      date= paste(object@date),
      z.min = which.max(object@data$freq),
      z.max = (which.max(object@data$freq)-1)*object@x.conv + object@z.conv,
      z.units = object@z.units
    )
  } else {
    # probably dataType is "noImage"
    if (dataType != "noImage") warning("AFM data type is not noImage.")
    r = data.frame(
      objectect = paste(object@instrument,dataType),
      description = paste(object@description),
      resolution = paste(object@x.pixels),
      size = paste(object@z.conv,"-",(object@x.pixels)),
      channel = paste(object@channel),
      history = paste(object@history),
      date= paste(object@date),
      z.min = 0,
      z.max = 0,
      z.units = object@z.units
    )
  }
  r$dataType = dataType
  r
}



#' Raster data frame
#'
#' data frame has  ($x, $y, $z) in units for particular channel, ($x, $y) are
#' always in units of nanometer
#'
#' for frequency data sweep, it will return ($freq, $z) instead
#'
#' @param obj AFMdata object
#' @param no channel number
#' @return data.frame with ($x, $y, $z) raster image; ($x,$y) in units of nm, or ($freq, $z) for frequency sweep
#' @author Thomas Gredig
#' @examples
#' afmd = AFM.import(AFM.getSampleImages(type='ibw'))
#' d = AFM.raster(afmd, 1)
#' head(d)
#' @export
AFM.raster <- function(obj,no=1) {
  if(!isS4(obj)) { stop("Not an S4 object, AFMdata object expected.") }
  if (AFM.isImage(obj) || (AFM.dataType(obj)=='spectroscopy')) {
    dr = data.frame(
      x = rep(0:(obj@x.pixels-1),obj@y.pixels)*obj@x.conv,
      y = rep(0:(obj@y.pixels-1),each=obj@x.pixels)*obj@y.conv,
      z = obj@data$z[[no]]
    )
  } else if (AFM.dataType(obj)=='frequency') {
    nLen = length(obj@data$freq)
    dr = data.frame(
      freq.Hz = seq(from=obj@z.conv, to=(obj@z.conv + obj@x.nm), length.out = nLen),
      z.V = obj@data$freq
    )
  } else {  # could be a spectrum
    dr = data.frame(
      x = (0:(obj@x.pixels-1))*obj@x.conv,
      z = obj@data$z[[no]]
    )
  }
  dr
}





#' checks if the object is an AFM image
#'
#' @param obj AFMdata object
#' @return \code{TRUE} if object is an AFM image, \code{FALSE} for frequency image
#' @author Thomas Gredig
#' @examples
#' d = AFM.import(AFM.getSampleImages(type='ibw'))
#' AFM.isImage(d)
#' @export
AFM.isImage <- function(obj) {
  (AFM.dataType(obj)=="image")
}

#' Get AFM Data Type as String
#' 
#' @description Determine the type of the AFM file; this could be
#'     an `image` or `frequency` for resonance curve, or `spectroscopy` for
#'     a force vs distance spectroscopy file, `noImage` for a file 
#'     with data and no image
#'
#' @param obj AFMdata object
#' @return string with AFM type, "image", "noImage", "frequency", "spectroscopy"
#'
#' @author Thomas Gredig
#' @examples
#' # show the data type for each AFM file
#' fList = AFM.getSampleImages()
#' dTypes = sapply(fList, function(x) { AFM.dataType(AFM.import(x)) })
#' data.frame(basename(fList), unname(dTypes))
#' 
#' @export
AFM.dataType <- function(obj) {
  if (names(obj@data)[1]=="freq") return("frequency")
  if (((obj@x.pixels <= 1) || (obj@y.pixels <= 1))) return("noImage")
  if ((length(grep("specHead", names(obj@data)))>0) && (length(obj@data$specHead)>0)) return("spectroscopy")
  return("image")
}



#' Valid AFM file
#'
#' Checks that filename is an AFM data file either Asylum Research
#' Igor Wavefile, Nanoscope Veeco file, Park AFM file, or Nanosurf file
#'
#' @param filename filename with full path
#' @return \code{TRUE} if filename is a supported AFM image
#' @author Thomas Gredig
#' @examples
#' AFM.isFileValid(AFM.getSampleImages()[1])
#' @export
AFM.isFileValid <- function(filename) {
  validFile = FALSE
  if (file.exists(filename)) {
    validFile =  grepl('\\.ibw$',filename, ignore.case = TRUE) |
      grepl('\\.tiff$',filename, ignore.case = TRUE) |
      grepl('\\.nid$',filename, ignore.case = TRUE) |
      grepl('\\.\\d{3}$',filename, ignore.case = TRUE)
  }
  validFile
}

