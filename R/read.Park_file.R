read.String <- function(to.read, nLen) {
  a = readBin(to.read, character(), size=1, n=nLen , endian = "little")
  paste(a, collapse='')
}

# Loading header information for spectroscopy
loadBinaryAFMspectrumHead <- function(filename, dataStart, nLen) {
  MaxSpectraChannels = 8

  to.read = file(filename, 'rb')
  seek(to.read, where=dataStart)
  d = readBin(to.read, integer(), size=2, n=nLen/2 , endian = "little")
  close(to.read)
  d
}

read.Park_file.v2 <- function(filename) {
  # read TIFF tags
  tiffTags = tagReader(filename)
  afm.params = as.numeric(strsplit(tiffTags[16,'valueStr'],',')[[1]])
  if (length(afm.params)<2) {
    warning(paste("Park AFM file ",filename,"has XML header instead of parameter list."))
    return(NULL) # XML header in num 17
  }
  params = get.ParkAFM.header(afm.params)

  # check if it contains spectroscopy data
  if (params$imageType==2) {
    # warning("Contains spectroscopy data.")
    # Load Spectroscopy Header
    dataStart = tiffTags[which(tiffTags$tag==50438),]$value
    dataLen = tiffTags[which(tiffTags$tag==50438),]$count
    # load with data type Int32
    spec.head = loadBinaryAFMspectrumHead(filename, dataStart, dataLen)
    # warning(paste("Head Length for Spectroscopy Header:", dataLen))

    # loadBinaryAFMspectrumHead(filename, dataStart, dataLen)
    # Load Spectroscopy Data
    dataStart = tiffTags[which(tiffTags$tag==50439),]$value
    dataLen = tiffTags[which(tiffTags$tag==50439),]$count
    spec.data = loadBinaryAFMDatafromTIFF(filename, dataStart, dataLen, params$nDataType)
    # warning(paste("Data Length for Spectroscopy Data:", dataLen))
    # print(dataLen)
  } else {
    spec.data = data.frame()
    spec.head = data.frame()
  }

  # check that the file can be displayed
  if (!tiff.isPaletteColorImage(tiffTags)) stop("Not a palette color image.")
  if (!tiff.getValue(tiffTags,'BitsPerSample') == 8) stop("Not an 3 x 8-bit image.")

  # read data
  dataStart = tiffTags[which(tiffTags$tag==50434),]$value
  dataLen = tiffTags[which(tiffTags$tag==50434),]$count
  # warning(paste("length:",dataLen))
  df = loadBinaryAFMDatafromTIFF(filename, dataStart, dataLen, params$nDataType)
  if (is.null(df)) {return(NULL) }

  # create image
  imWidth = params$nWidth # tiff.getValue(tiffTags, 'ImageWidth')
  imHeight = params$nHeight # tiff.getValue(tiffTags, 'ImageLength')
  
  if (length(df) != params$nWidth * params$nHeight) {
    warning("AFM Image ",filename," is not square ==> improvising.")
    imHeight = sqrt(length(df))
    imWidth = imHeight
  }
  channels = params$sourceName
  units = units2nanometer(params$UnitZ)$unit
  description = params$imageMode

  # Park TIFF images only have 1 channel
  z.data = list()
  z.data[[1]] = (df * params$dfDataGain) *  units2nanometer(params$UnitZ)$power
  # return AFMdata object
  AFMdata(
    data = list(z=z.data, specData=spec.data, specHead = spec.head),
    channel = channels,
    x.conv = params$dfXScanSizeum / (imWidth-1) * 1000,
    y.conv = params$dfYScanSizeum / (imHeight-1) * 1000,
    x.pixels = imWidth,
    y.pixels = imHeight,
    z.conv = 1,
    z.units = units,
    instrument = 'Park',
    history = '',
    date = read.Park_date(tiffTags),
    description = description,
    fullFilename = filename
  )
}


# loads Park Image header
read.Park_header.v2 <- function(filename) {

  tiffTags = tagReader(filename)
  afm.params = as.numeric(strsplit(tiffTags[16,'valueStr'],',')[[1]])
  params = get.ParkAFM.header(afm.params)

  r1 = data.frame(
    name = colnames(params),
    value = as.character(params[1,])
  )
  r2 = data.frame(
    name = tiffTags[1:14,'tagName'],
    value = tiffTags[1:14,'value']
  )
  r2[c(9:12),'value']=tiffTags[c(9:12),'valueStr']
  rbind(r1,r2)
}

# returns the date string, when the image was recorded:
# example: "2024:06:04 12:16:16"
read.Park_date <- function(tiffTags) {
  tiffTags$valueStr[which(tiffTags$tag==306)]
}

