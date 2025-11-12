#' AFMinfo
#'
#' Read and normalize header/metadata from AFM image files produced by several
#' instrument vendors (Asylum/Cypher `.ibw`, Park `.tiff`, Nanosurf `.nid`,
#' and Bruker/Veeco Nanoscope). The function extracts commonly used fields such
#' as scan size, pixel dimensions, scan angle, scan rate, imaging mode,
#' cantilever information, and resonance frequency, and returns them in a
#' consistent structure with standardized units.
#'
#' @description
#' Parses instrument-specific headers and returns a compact summary of AFM scan
#' settings. Numeric outputs are coerced into consistent base units:
#' 
#' - `imageSize.um` in micrometers (µm)
#' 
#' - `scanRate.Hz` in hertz (Hz)
#' 
#' - `resFrequency` in hertz (Hz)
#' 
#' - `scanAngle` in degrees
#'
#' @details
#' **Supported formats and field mapping (high level):**
#' - **Asylum/Cypher (`.ibw`)**: Reads fields such as `ScanSize`, `PointsLines`,
#'   `ScanLines`, `ScanAngle`, `ScanRate`, `ImagingMode`, `ThermalLever`,
#'   `ResFreq1`, `ImageNote`, `BaseName`. The image size is converted to µm,
#'   resonance frequency to Hz.
#' - **Park (`.tiff`)**: Reads `dfXScanSizeum`, `nWidth`, `nHeight`, `dfAngle`,
#'   `dfScanRateHz`, `imageMode`, `Cantilever`, `dfNCMFrequency`, `sourceName`.
#'   Image size is provided directly in µm; frequencies are normalized to Hz.
#' - **Nanosurf (`.nid`)**: Uses `get_NID_imageSize()` to derive scan size (µm),
#'   reads `Points`, `Lines`, `Rotation` (parsed as degrees), `Time/Line`
#'   (converted to `scanRate.Hz = 1 / (time per line)`), `Op. mode`,
#'   `Cantilever type`, `Vibration freq` (kHz → Hz). Channel count is inferred
#'   from `Gr\\d-Ch\\d` entries.
#' - **Bruker/Veeco Nanoscope (other)**: Reads `Scan size`, valid data lengths,
#'   `Feature scan angle`, `Scan rate`, and misc. notes. Image size is expressed
#'   in µm. Some fields may be instrument/firmware dependent.
#'
#' @param filename Character scalar. Full path to the instrument AFM file.
#'
#' @return An object of class **`AFMinfo`** (list) with the following elements:
#' \itemize{
#'   \item `data` — instrument-specific header key/value table.
#'   \item `type` — character; inferred vendor type (`"Cypher"`, `"Park"`,
#'                 `"NanoSurf"`, `"Veeco"`).
#'   \item `filename` — character; original file path.
#'   \item `imageSize.um` — numeric; scan size in micrometers (µm).
#'   \item `widthPixel` — numeric; number of pixels/samples in X.
#'   \item `heightPixel` — numeric; number of pixels/samples in Y.
#'   \item `scanRate.Hz` — numeric; line (or effective) scan rate in Hz.
#'   \item `scanAngle` — numeric; scan rotation angle in degrees.
#'   \item `imagingMode` — character; instrument imaging/operation mode.
#'   \item `cantilever` — character; cantilever or probe descriptor.
#'   \item `resFrequency` — numeric; resonance frequency in Hz (0 if unavailable).
#'   \item `noChannels` — integer; number of data channels detected.
#'   \item `note` — character; free-form note (instrument dependent).
#' }
#'
#' @examples
#' filename <- AFM.getSampleImages(type = "gredig")
#' h <- AFMinfo(filename)
#' summary(h)
#'
#' @seealso \code{read.AR_header.v2}, \code{read.Park_header.v2},
#'   \code{read.NanoSurf_header.v2}, \code{read.Nanoscope_file},
#'   \code{get_NID_imageSize}
#'
#' @author Thomas Gredig
#' @export
AFMinfo <- function(filename) {
  fext = tolower(tools::file_ext(filename))
  if (fext=='ibw') {
    data = read.AR_header.v2(filename)
    type = 'Cypher'
    imageSize.um = as.numeric(data$value[grep('ScanSize',data$name)[1]])/1e-6
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
    imageSize.um = data$value[grep('dfXScanSizeum',data$name)[1]]
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
    # cat(data$value[grep("Image size",data$name)[1]])
    imageSize.um = get_NID_imageSize(data$value[grep("Image size",data$name)[1]])
    widthPixel = data$value[grep('Points',data$name)[1]]
    heightPixel = data$value[grep('Lines',data$name)[1]]
    scanAngle = as.numeric(gsub("[^0-9eE+\\.-]", "", data$value[grepl("Rotation", data$name)][1]))
    scanRate.Hz = 1/as.numeric(gsub('(\\d+).*','\\1',data$value[grep('Time/Line',data$name)[1]]))
    imagingMode = data$value[grep('Op. mode',data$name)[1]]
    cantilever = data$value[grep('Cantilever type',data$name)[1]]
    resFrequency = as.numeric(gsub('kHz','',data$value[grep('Vibration freq',data$name)[1]]))*1000
    note = ""
    noChannels = length(grep('Gr\\d-Ch\\d',data$name))
  } else {
    data = read.Nanoscope_file(filename, headerOnly=TRUE)
    type = 'Veeco'

    imageSize.um = get_NID_imageSize(data$value[grep("Scan size",data$name)][2])
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
      imageSize.um = as.numeric(imageSize.um),
      widthPixel = as.numeric(widthPixel),
      heightPixel = as.numeric(heightPixel),
      scanRate.Hz = as.numeric(scanRate.Hz),
      scanAngle = as.numeric(scanAngle),
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



# returns the image size for a NID AFM file
# s = string, such as "200nm" etc.
get_NID_imageSize <- function(s) {
  # Normalize the string (remove spaces and lowercase)
  s <- trimws(tolower(s))

  # Extract numeric part
  value <- as.numeric(gsub("[^0-9eE+\\.-]", "", s))
  
  # Determine scale based on unit
  if (grepl("nm", s)) {
    return(value * 1e-3)      # nm → µm
  } else if (grepl("µm|um", s)) {
    return(value)             # already in µm
  } else if (grepl("mm", s)) {
    return(value * 1e3)       # mm → µm
  } else if (grepl("m", s)) {
    return(value * 1e6)       # m → µm
  } else {
    return(NA)
    # warning("Unknown or missing unit in NID image size:", s)
  }
}
