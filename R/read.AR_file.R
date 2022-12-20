# read Igor AFM image files
read.AR_header.v2 <- function(filename) {
  suppressWarnings({
    d = read.ibw.image(filename)
    # to get notes, need to read all, not just "header"
  })
  qNote = attr(d, "Note")
  notes = strsplit(qNote,'\r')[[1]]
  df = data.frame(
    name = gsub('(.*?):.*','\\1',notes),
    value = gsub('(.*?):(.*)','\\2',notes)
  )
}

read.AR_file.v2 <- function(filename) {
  h1 = read.AR_eofHeader.V2(filename)
  if (length(h1$DataTypeList)>0) { channels = strsplit(h1$DataTypeList,',')[[1]] }
  else { channels = strsplit(h1$DataTypes,',')[[1]] }
  units = rep('nm', length(channels))
  units[grep('Phase',channels)] = 'deg'    # both Phase and NapPhase channels
  suppressWarnings({
    d = read.ibw.image(filename)
  })
  q2 = attr(d, "WaveHeader")
  noChannels <- h1$NumberOfFiles
  if (as.numeric(h1$IsImage)==1) {
    # loading an image file
    imageDim <- q2$nDim[1:2]
    dr = data.frame()
    x.conv = q2$sfA[1] * 1e9
    y.conv = q2$sfA[2] * 1e9
    y.pixels = imageDim[2]
    im.size = imageDim[1]*imageDim[2]
    z.data=list()
    for(i in 1:noChannels) {
      z.conv = 1; if (units[i]=='nm') z.conv=1e9
      z.data[[i]] = d[(im.size*(i-1)+1):(im.size*i)]*z.conv
    }
    description = gsub('.*ImageNote:(.*?)\r.*','\\1',attr(d,'Note'))
  } else if (as.numeric(h1$IsForce)==1) {
    # loading a force curve
    units[grep('^Freq',channels)] = 'Hz'
    units[grep('^Time',channels)] = 's'
    imageDim = q2$nDim[1]
    x.conv = q2$sfA[1]
    y.conv = 0
    y.pixels = 1
    z.data=list()
    dr = data.frame()
    for(i in 1:noChannels) {
      z.data[[i]] = d[(imageDim*(i-1)+1):(imageDim*i)]
    }
    description = 'Force'
  } else { warning("Neither image nor force data in AFM image.") }

  # return AFMdata object
  AFMdata(
    data = list(z=z.data),
    channel = channels,
    x.conv = x.conv,
    y.conv = y.conv,
    x.pixels = imageDim[1],
    y.pixels = y.pixels,
    z.conv = 1,
    z.units = units,
    instrument = 'Cypher',
    history = '',
    description = description,
    fullFilename = filename
  )
}


read.AR_eofHeader.V2 <- function(wavefile, Verbose = FALSE) {
  con <- file(wavefile, "rb",encoding="macintosh")

  # check Igor Wavefile is version 5
  bytes=readBin(con,"integer",2,size=1)
  if(bytes[1]==0){ endian="big"; version=bytes[2] } else { endian="little"; version=bytes[1] }
  if(Verbose) cat("version = ",version,"endian = ",endian,"\n")
  if(version != 5) { warning("Not sure how to read IBW")}

  # find the end of the file
  fsize = file.info(wavefile)$size
  seek(con, where=fsize-10)
  # read header size
  rawc = readBin(con, what="raw", 10)    # "0466 MFP3D"
  s=readBin(rawc, what="character")
  headerSize = as.numeric(gsub('^(\\d+).*','\\1',s))
  if(Verbose) cat("headerSize = ",headerSize,"\n")

  # read complete header version 2
  s3=list()
  if (headerSize>10 & headerSize<1200) {
    seek(con, where=fsize-headerSize)
    rawc = readBin(con, what="raw", headerSize)
    s=readBin(rawc, what="character")
    p2= strsplit(strsplit(s,';')[[1]],":")
    p3 = p2[which(sapply(p2, length)==2)]

    s3[sapply(p3,'[[',1)] =  sapply(p3,'[[',2)
  } else { warning("Header size incorrect.") }
  close(con)

  s3
}


