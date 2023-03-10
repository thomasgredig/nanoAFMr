# read PARK AFM images
read.Park_file <- function(filename) {
  warning("OUTDATED: should not be used any longer.")
  # read TIFF tags
  tiffTags = tagReader(filename)
  afm.params = as.numeric(strsplit(tiffTags[16,'valueStr'],',')[[1]])
  params = get.ParkAFM.header(afm.params)

  # check that the file can be displayed
  if (!tiff.isPaletteColorImage(tiffTags)) stop("Not a palette color image.")
  if (!tiff.getValue(tiffTags,'BitsPerSample') ==  8) stop("Not an 3 x 8-bit image.")

  # read data
  dataStart = tiffTags[which(tiffTags$tag==50434),]$value
  dataLen = tiffTags[which(tiffTags$tag==50434),]$count
  # warning(paste("length:",dataLen))
  df = loadBinaryAFMDatafromTIFF(filename, dataStart, dataLen, params$nDataType)

  # create image
  imWidth = tiff.getValue(tiffTags, 'ImageWidth')
  imHeight = tiff.getValue(tiffTags, 'ImageLength')
  if (imHeight != imWidth) {
    warning("Image is not square.")
    imHeight=imWidth
  }
  if (length(df) != imHeight*imWidth) {
    imHeight = sqrt(length(df))
    imWidth = imHeight
  }
  x=rep(1:imWidth,imHeight)
  y=rep(seq(from=1, to=imHeight),each=imWidth)

  d1 = data.frame(
    x,
    y,
    z = df
  )
  d1$x.nm = params$dfXScanSizeum * d1$x / max(d1$x)*1000
  d1$y.nm = params$dfYScanSizeum * d1$y / max(d1$y)*1000
  d1$z.nm = (d1$z * params$dfDataGain) *  units2nanometer(params$UnitZ)

  d1
}

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
  # readBin(to.read, character(), size=2, n=46 , endian = "little")
  # specName <- read.String(to.read, 32)
  # specUnits <- read.String(to.read, 8)
  # specGain <- readBin(to.read, integer(), size=4, n=1 , endian = "little")
  # specAxes <- readBin(to.read, integer(), size=2, n=1 , endian = "little")
  close(to.read)
  d
}

read.Park_file.v2 <- function(filename) {
  # read TIFF tags
  tiffTags = tagReader(filename)
  afm.params = as.numeric(strsplit(tiffTags[16,'valueStr'],',')[[1]])
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


  # create image
  imWidth = tiff.getValue(tiffTags, 'ImageWidth')
  imHeight = tiff.getValue(tiffTags, 'ImageLength')
  if (imHeight != imWidth) {
    warning("Image is not square.")
    imHeight=imWidth
  }
  if (length(df) != imHeight*imWidth) {
    imHeight = sqrt(length(df))
    imWidth = imHeight
  }
  channels = params$sourceName
  units = params$UnitZ
  description = params$imageMode

  # Park TIFF images only have 1 channel
  z.data = list()
  z.data[[1]] = (df * params$dfDataGain) *  units2nanometer(params$UnitZ)
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

