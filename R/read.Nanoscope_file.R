# Returns Veeco NanoScope AFM image with Scaling
read.Nanoscope_file <- function(filename, no=1, headerOnly = FALSE, verbose=FALSE) {
  err.msg = c()
  # load header
  header = c()
  con <- file(filename,"rb")
  first_line = ''
  while(first_line != "\\*File list end" ) {
    first_line <- readLines(con,n=1)
    if (length(first_line)==0) { err.msg = c(err.msg,'Header error'); break; }
    if (first_line == "Frame=Frequency sweep") { err.msg = c(err.msg,"Frequency sweep file."); break;}
    header=c(header, first_line)
  }
  if (length(err.msg)>0) { warning(paste("Nanoscope read error:", err.msg)); return(data.frame()) }



  # analyze header
  header = gsub('\\\\','',header,  useBytes = TRUE)
  header[grep('^\\*',header, useBytes = TRUE)] = 
    paste0(header[grep('^\\*',header, useBytes = TRUE)],":NEW SECTION")
  HEADER.INFO = data.frame(
    name = gsub("(.*?):.*","\\1",header, useBytes = TRUE),
    value = gsub(".*?:(.*)","\\1",header, useBytes = TRUE),
    stringsAsFactors = FALSE
  )
  if (verbose) print(paste("Header has", nrow(HEADER.INFO),"items."))


  # get version
  version = HEADER.INFO$value[grep('^Version',HEADER.INFO$name)]
  version = as.numeric(gsub("0x",'',version, useBytes = TRUE))/1e6
  if (verbose) print(paste("Header version:",version))

  # could be Sens. Zscan or Sens. Zsens (version 8+)
  Volt2nanometer = HEADER.INFO$value[grep('Sens. Zs', HEADER.INFO$name)]
  Volt2nanometer = as.numeric(gsub(' V(.*)nm/V','\\1',Volt2nanometer, useBytes = TRUE))[1] # just use first
  #write.csv(HEADER.INFO, file='VEECO-Header.csv')

  # parse out parameters that are relevant to image number no
  sections = grep('NEW SECTION', HEADER.INFO$value, useBytes = TRUE)
  fileNo = grep('File list$', HEADER.INFO$name[sections], useBytes = TRUE)
  scanNo = grep('Scanner list$', HEADER.INFO$name[sections], useBytes = TRUE)
  ciaoNo = grep('Ciao scan list$', HEADER.INFO$name[sections], useBytes = TRUE)
  eqipNo = grep('Equipment list', HEADER.INFO$name[sections], useBytes = TRUE)
  totalNumberImages = length(grep('Ciao image list$',HEADER.INFO$name[sections], useBytes = TRUE))
  imNo = grep('Ciao image list$',HEADER.INFO$name[sections], useBytes = TRUE)

  # Image Header Online
  imageHeader = HEADER.INFO[sections[imNo[no]]:(sections[imNo[no]+1]-1),]
  getHeaderNumeric <- function(name) {as.numeric(imageHeader$value[grep(name,imageHeader$name)])  }
  getHeaderStr <- function(name) {imageHeader$value[grep(name,imageHeader$name)]  }
  getHeaderStrVal <- function(name) {imageHeader$value[grep(name,imageHeader$value)]  }

  im.channelName = gsub('.*\\[(.*)\\].*','\\1',imageHeader$value[grep('Image Data:',imageHeader$value)])
  im.line.num = getHeaderNumeric('Number of lines')
  im.line.sam = getHeaderNumeric('Samps/line')
  im.zMagnify = getHeaderStr('magnify')
  im.zMagnifyFac = as.numeric(gsub('.*\\](.*)','\\1',im.zMagnify))
  im.image.size = getHeaderStr('Scan size')
  if (length(im.image.size)==0) im.image.size = getHeaderStr('Scan Size')

  im.bytes.pixel = getHeaderNumeric('Bytes/pixel')
  im.scanDirection = getHeaderStr('Line direction')
  im.Zscale = getHeaderStrVal('Z scale:')
  im.ZscaleFac = as.numeric(gsub('.*?\\)(.*)V','\\1',im.Zscale))
  im.Zoffset = getHeaderStrVal('Z offset:')
  im.dataOffset = getHeaderNumeric('Data offset')
  im.dataLength = getHeaderNumeric('Data length')

  im.units='nm'  # default units
  if (im.channelName=='Amplitude') im.units='V'
  if (im.units=='V') Volt2nanometer=1
  zConversion =  im.ZscaleFac / (2^(im.bytes.pixel*8)) * Volt2nanometer

  wh = strsplit(gsub('\\s*(.*)\\s.*','\\1', im.image.size)," ")[[1]]
  im.width.nm = as.numeric(wh[1])
  im.height.nm = as.numeric(wh[2])
  # convert um to nm
  if (length(grep('~m',im.image.size))) {
    im.width.nm = im.width.nm*1000
    im.height.nm = im.height.nm *1000
  }

  step.nm = im.height.nm / im.line.num

  # load image
  x.nm = rep(seq(from=0, to=im.width.nm, length=im.line.sam), im.line.num)
  y.nm = rep(seq(from=0, to=im.height.nm, length=im.line.num), each=im.line.sam)

  #print(paste("Loading at offset",im.dataOffset," (",im.dataLength,"bytes)"))
  seek(con, where  = im.dataOffset) # skip header
  z   <- readBin(con, integer(),  n = im.dataLength/im.bytes.pixel, size=im.bytes.pixel, endian = "little")
  close(con)

  if (length(err.msg)>0) { warning(paste("NID read error:", err.msg)); return(data.frame()) }

  df = data.frame(x = rep(1:im.line.sam, im.line.num),
                 y = rep(1:im.line.num, each=im.line.sam),
                 z,
                 x.nm,
                 y.nm,
                 z.nm = z*zConversion)

  attr(df,"channelDirection") = getHeaderStrVal('Line direction')
  attr(df,"note")       = getHeaderStrVal('Note')
  attr(df,"channel")    = im.channelName
  attr(df,"units")      = im.units
  attr(df,"date")       = HEADER.INFO$value[grep('^Date', HEADER.INFO$name)]
  if (headerOnly) {
    fileHeader = HEADER.INFO[sections[fileNo[no]]:(sections[fileNo[no]+1]-1),]
    ciaoHeader = HEADER.INFO[sections[ciaoNo[no]]:(sections[ciaoNo[no]+1]-1),]
    scanHeader = HEADER.INFO[sections[scanNo[no]]:(sections[scanNo[no]+1]-1),]
    equipHeader = HEADER.INFO[sections[eqipNo[no]]:(sections[eqipNo[no]+1]-1),]
    df = rbind(imageHeader,equipHeader,scanHeader,ciaoHeader,fileHeader)
  }
  df
}

# reads a nanoscope file with file extension .001 or another number
# and returns an AFMdata object
read.Nanoscope.v2 <- function(filename) {
  d = AFM.read(filename)
  z.conv = 1
  if (d$z[1] != 0) z.conv = d$z.nm[1] / d$z[1]
  d1 = list(d$z.nm)
  if (is.null(attr(d,"note"))) attr(d,"note")="none"
  obj = AFMdata(
    data = list(z=d1),
    channel = attr(d,"channel"),
    x.conv = max(d$x.nm)/(max(d$x)-1),
    y.conv = max(d$y.nm)/(max(d$y)-1),
    x.pixels = max(d$x),
    y.pixels = max(d$y),
    z.conv = z.conv,
    z.units = .getChannelUnits(attr(d,"channel")),
    instrument = attr(d,"instrument"),
    history = '',
    date = '',
    description = attr(d,"note"),
    fullFilename = filename
  )
}
