#' AFMinfo object
#'
#' header information about the AFM image, includes scan rate, widths, etc.
#'
#' @param filename full file name of instrument AFM file
#' @return AFMinfo class object
#' @author Thomas Gredig
#' @examples
#' filename = AFM.getSampleImages(type='gredig')
#' h = AFMinfo(filename)
#' summary(h)
#' @export
AFMinfo <- function(filename) {
  fext = tolower(tools::file_ext(filename))
  if (fext=='ibw') {
    data = read.AR_header.v2(filename)
    type = 'Cypher'
    widthPixel = data$value[grep('PointsLines',data$name)[1]]
    heightPixel = data$value[grep('ScanLines',data$name)[1]]
    scanAngle = data$value[grep('ScanAngle',data$name)[1]]
    scanRate.Hz = data$value[grep('ScanRate',data$name)[1]]
    imagingMode = data$value[grep('ImagingMode',data$name)[1]]
    cantilever = data$value[grep('ThermalLever',data$name)[1]]
    resFrequency = data$value[grep('ResFreq1',data$name)[1]]
    note = paste0(data$value[grep('ImageNote',data$name)[1]],",",data$value[grep('BaseName',data$name)[1]])
    noChannels = 8 - sum(data$value[grep('^Channel\\d',data$name)]==" None")
  } else if (fext=='tiff') {
    data = read.Park_header.v2(filename)
    type = 'Park'
    widthPixel = data$value[grep('nWidth',data$name)[1]]
    heightPixel = data$value[grep('nHeight',data$name)[1]]
    scanAngle = data$value[grep('dfAngle',data$name)[1]]
    scanRate.Hz = data$value[grep('dfScanRateHz',data$name)[1]]
    imagingMode = data$value[grep('imageMode',data$name)[1]]
    cantilever = data$value[grep('Cantilever',data$name)[1]]
    resFrequency = data$value[grep('dfNCMFrequency',data$name)[1]]
    note = data$value[grep('sourceName',data$name)[1]]
    noChannels = 1
  } else if (fext=='nid') {
    data = read.NanoSurf_header.v2(filename)
    type = 'NanoSurf'
    widthPixel = data$value[grep('Points',data$name)[1]]
    heightPixel = data$value[grep('Lines',data$name)[1]]
    scanAngle = data$value[grep('dfAngle',data$name)[1]]
    scanRate.Hz = 1/as.numeric(gsub('(\\d+).*','\\1',data$value[grep('Time/Line',data$name)[1]]))
    imagingMode = data$value[grep('Op. mode',data$name)[1]]
    cantilever = data$value[grep('Cantilever type',data$name)[1]]
    resFrequency = as.numeric(gsub('kHz','',data$value[grep('Vibration freq',data$name)[1]]))*1000
    note = ""
    noChannels = length(grep('Gr\\d-Ch\\d',data$name))
  } else {
    data = read.Nanoscope_file(filename, headerOnly=TRUE)
    type = 'Veeco'
    widthPixel = as.numeric(data$value[grep('Valid data len X',data$name)[1]])
    heightPixel = as.numeric(data$value[grep('Valid data len Y',data$name)[1]])
    scanAngle = data$value[grep('Feature scan angle',data$name)[1]]
    scanRate.Hz = data$value[grep('Scan rate',data$name)[1]]
    imagingMode = ''
    cantilever = data$value[grep('Cantilever type',data$name)[1]]
    resFrequency = 0
    note = data$value[grep('Note',data$name)[1]]
    noChannels = 0
  }
  structure(
    list(
      data = data,
      type = type,
      filename = filename,
      widthPixel = as.numeric(widthPixel),
      heightPixel = as.numeric(heightPixel),
      scanRate.Hz = as.numeric(scanRate.Hz),
      imagingMode = imagingMode,
      cantilever = cantilever,
      resFrequency = as.numeric(resFrequency),
      noChannels = noChannels,
      note = note
    ),
    class = 'AFMinfo'
  )
}

#' Specific information about AFM image
#'
#' Retrieve a specific information piece from the AFM image,
#' such as "ScanRate", "Description", etc. The item names depend
#' on the instrument; to find possible item names, leave
#' \code{itemName} empty.
#'
#' @param obj AFMinfo object
#' @param itemName name to retrieve (ScanRate, ScanAngle, ...)
#' @return if \code{itemName} is empty, returns list of names, otherwise the value for the specific data item
#' @author Thomas Gredig
#' @examples
#' filename = AFM.getSampleImages(type='gredig')
#' h = AFMinfo(filename)
#' allNames = AFMinfo.item(h)
#' AFMinfo.item(h,"Description")
#' AFMinfo.item(h,"Operating mode")
#' print(allNames)
#' @export
AFMinfo.item <- function(obj, itemName="") {
  q = grep(itemName,obj$data$name)
  if(itemName=="") { # return item names
    itemValue = obj$data$name
  } else { # return value of item
    itemValue = ''
    if (length(q)>0) {
      itemValue = obj$data$value[q[1]]
    }
  }
  itemValue
}


#' summary for AFMinfo object
#'
#' @param object AFMinfo object
#' @param ... other parameters
#' @return quick summary
#' @author Thomas Gredig
#' @examples
#' h1 = AFMinfo(AFM.getSampleImages(type='ibw'))
#' summary(h1)
#' @export
summary.AFMinfo <- function(object, ...) {
  cat('AFM Image Type:       ', object$type, "\n")
  cat('Resolution:           ', object$widthPixel, "x", object$heightPixel,'\n')
  cat('Channels:             ', object$noChannels,'\n')
  cat('Resonance Freq (Hz):  ', object$resFrequency,'\n')
  cat('Scan Rate (Hz):       ', object$scanRate.Hz,'\n')
  cat('Scan Angle (deg):     ', object$scanAngle,'\n')
  cat('Notes:                ', object$note,'\n')
  cat('Data Items:           ', length(object$data),'\n')
}
