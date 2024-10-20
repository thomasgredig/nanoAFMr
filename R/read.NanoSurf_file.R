# ######################################
# read NID file, AFM file
#
# Date: 2019-02-10
# Author: Thomas Gredig
#
# ######################################
#
# # Nanosurf image data format (NID format)
# # from easyscan AFM
#
#
# # read the header of the NID AFM files
# # seems header file ends with "" (empty) or
# # with #!F
# ######################################

# loads header of NanoSurf AFM NID file and returns
read.NanoSurf_header.v2 <- function(filename) {
  # read the NID header
  k1 = read.NID_header(filename)[[2]]

  q = grep("=",k1, useBytes = TRUE)
  data.frame(
    name = gsub('(.*?)=.*','\\1',k1[q], useBytes = TRUE),
    value = gsub('(.*?)=(.*)','\\2',k1[q], useBytes = TRUE)
  )
}

# loads NanoSurf NID file, all channels and then returns an AFMdata file.
read.NanoSurf_file.v2 <- function(filename) {
  # file must be in NID format
  if (!file.exists(filename)) stop(paste("File",filename,"does NOT exist."))
  if (!(NID.checkFile(filename)==0)) stop("NID file is corrupted.")

  # read the header
  hItems = read.NID_headerItems(filename)
  h = read.NID_header(filename)

  # get the size for each channel
  q = get.NID_imageInfo(h[[2]])
  noImages = length(q)

  # read header and all channels
  header.length = h[[1]]
  con <- file(filename,"rb")
  bin.header <- readBin(con, integer(),  n = header.length, size=1, endian = "little")
  bin.ID = readBin(con, integer(),  n = 2, size=1, endian = "little")

  r = list()

  if (sum(bin.ID) == sum(c(35,33))) {
    if(length(q)>0) {
      for(i in seq_len(length(q))) {
        bin.data <- readBin(con, integer(),  n = q[i], size=2, endian = "little")
        r[[i]] = bin.data
      }
    }
  }
  close(con)

  # check if file is a resonance / frequency curve
  if (get.NIDitem(hItems[[2]],'Gr0-Name') == "Frequency sweep") {
    # FREQUENCY file
    # ==============
    afmNote = paste(get.NIDitem(hItems[[3]], "Cantilever type"),
          get.NIDitem(hItems[[3]], "Freq. peak"),
          get.NIDitem(hItems[[3]], "Excitation ampl."),
          get.NIDitem(hItems[[3]], "Date"),
          get.NIDitem(hItems[[3]], "Time"),
          sep="; ")
    h = NID.getHeaderSet(hItems,2)
    if (length(r) != 1) warning("Frequency sweep has additional data sets !?!")
    freqStep = get.NIDitem.numeric(h,'Dim0Range') / (get.NIDitem.numeric(h,'Points') - 1)

    obj = AFMdata(
      data = list(freq=r[[1]]/(2^16)*get.NIDitem.numeric(h, "Dim2Range")),
      channel = "Frequency sweep",
      x.conv = freqStep,  # frequency Step
      y.conv = 0,
      x.pixels = get.NIDitem.numeric(h,'Points'),  # number of data points
      y.pixels = 0,
      z.conv = get.NIDitem.numeric(h, "Dim0Min"),  # starting frequency in set
      z.units = get.NIDitem(h,'Dim0Unit'),
      instrument = "NanoSurf",
      history = '',
      date = '',
      description = afmNote,
      fullFilename = filename
    )

  } else {
    # IMAGE file
    # =========

    # get scaling for image
    units=c()
    channels=c()
    dz = list()
    for(J in seq_len(noImages)) {
      s1 = NID.getChannelScale(hItems,J)
      unit_conversion_factor = 1
      new_units = s1$units[3]
      if (new_units=="m") {
        new_units = "nm"
        unit_conversion_factor = 1E9
      }
      units = c(units, new_units)
      channels = c(channels, s1$channelName[3])

      # create the rastering sequences for x-, y-axes
      # and convert pixels from z-axis into scale (m or V)
      seq_x = seq(from=s1$from[1], to=s1$to[1], length.out = s1$length[1])
      seq_y = seq(from=s1$from[2], to=s1$to[2], length.out = s1$length[2])
      range.z = s1$to[3] - s1$from[3]

      dz[[J]] = r[[J]]* (range.z/s1$length[3]) * unit_conversion_factor
    }

    afmNote =paste(get.NIDitem(hItems[[3]],'Date'),get.NIDitem(hItems[[3]],'Time'))
    obj = AFMdata(
      data = list(z=dz),
      channel = channels,
      x.conv = ((s1$to[1] - s1$from[1]) / (s1$length[1]-1)) * 1e9,  # assume it is in [m]
      y.conv = ((s1$to[2] - s1$from[2]) / (s1$length[2]-1)) * 1e9,
      x.pixels = s1$length[1],
      y.pixels = s1$length[2],
      z.conv = 1,
      z.units = units,
      instrument = "NanoSurf",
      history = '',
      description = afmNote,
      fullFilename = filename
    )
  }

  obj
}


# laods channel 1 image, deprecated, use read.NanoSurf_file.v2() instead
read.NanoSurf_file <- function(filename, imageNo=1) {
  if (!file.exists(filename)) warning(paste("File",filename,"does NOT exist."))
  # read header information
  hItems = read.NID_headerItems(filename)
  # read all images
  if (NID.checkFile(filename) == 0) {
    h = read.NID_header(filename)
    q = get.NID_imageInfo(h[[2]])

    header.length = h[[1]]
    con <- file(filename,"rb")
    bin.header <- readBin(con, integer(),  n = header.length, size=1, endian = "little")
    bin.ID = readBin(con, integer(),  n = 2, size=1, endian = "little")
    #r = list(header = bin.header, ID = bin.ID)
    r = list()

    # Loading all channels
    if (sum(bin.ID) == sum(c(35,33))) {
      if(length(q)>0) {
        for(i in seq_len(length(q))) {
          bin.data <- readBin(con, integer(),  n = q[i], size=2, endian = "little")
          r[[i]] = bin.data
        }
      }
    }
    close(con)
  }
  d = r

  # get scaling for image
  s1 = NID.getChannelScale(hItems,imageNo)

  # create the rastering sequences for x-, y-axes
  # and convert pixels from z-axis into scale (m or V)
  seq1 = seq(from=s1$from[1], to=s1$to[1], length.out = s1$length[1])
  seq2 = seq(from=s1$from[2], to=s1$to[2], length.out = s1$length[2])
  range.z = s1$to[3]-s1$from[3]

  # create a data frame with the AFM image
  data.frame(x =rep(1:s1$length[1], each = s1$length[1]),
             y = rep(1:s1$length[1], times = s1$length[1]),
             z = d[[imageNo]],
             x.nm=rep(seq1,each=s1$length[2])*1e9,
             y.nm=rep(seq2,times=s1$length[1])*1e9,
             z.nm=(d[[imageNo]]* (range.z/65536) )*1e9)
}

# additional helper functions to read NID files
NULL


get.NIDitem <- function(item, name) {
  n0 = grep(paste0(name,'='),item, useBytes = TRUE)
  gsub(paste0(name,'='),'',item[n0])
}

get.NIDitem.numeric <- function(item, name) {
  n0 = grep(paste0(name,'='),item, useBytes = TRUE)
  as.numeric(gsub(paste0(name,'='),'',item[n0]))
}

NID.getHeaderSet <- function(headerList, imageNo = 1) {
  c1 = switch(imageNo, "Gr0-Ch1","Gr0-Ch2","Gr1-Ch1","Gr1-Ch2",
              "Gr2-Ch1","Gr2-Ch2","Gr3-Ch1","Gr3-Ch2")
  d.set = get.NIDitem(headerList[[2]],c1)
  k.set = grep(d.set,headerList[[1]], useBytes = TRUE)
  headerList[[k.set]]
}

# returns the scales of a particular channel / image
# headerList header list as obtained from read.NID_headerItems
# imageNo 1,2,3,4 denoting the number of the image
NID.getChannelScale <- function(headerList, imageNo = 1) {
  h = NID.getHeaderSet(headerList, imageNo)

  ax=data.frame(axis='x',units = get.NIDitem(h,'Dim0Unit'),
                from=get.NIDitem.numeric(h,'Dim0Min'),
                to=get.NIDitem.numeric(h,'Dim0Min')+get.NIDitem.numeric(h,'Dim0Range'),
                length=get.NIDitem.numeric(h,'Points'),
                channelName = ""
  )
  ay=data.frame(axis='y',units = get.NIDitem(h,'Dim1Unit'),
                from=get.NIDitem.numeric(h,'Dim1Min'),
                to=get.NIDitem.numeric(h,'Dim1Min')+get.NIDitem.numeric(h,'Dim1Range'),
                length=get.NIDitem.numeric(h,'Lines'),
                channelName = ""
  )
  az=data.frame(axis='z',units = get.NIDitem(h,'Dim2Unit'),
                from=get.NIDitem.numeric(h,'Dim2Min'),
                to=get.NIDitem.numeric(h,'Dim2Min')+get.NIDitem.numeric(h,'Dim2Range'),
                length=2**get.NIDitem.numeric(h,'SaveBits'),
                channelName = paste(get.NIDitem(h,'Dim2Name'),get.NIDitem(h,'Frame'))
  )
  rbind(ax,ay,az)
}




# > k1[from]
# [1] "[DataSet]"                           "[DataSet-Info]"                      "[DataSet\\DataSetInfos]"
# [4] "[DataSet\\DataSetInfos\\Scan]"       "[DataSet\\DataSetInfos\\Feedback]"   "[DataSet\\DataSetInfos\\Global]"
# [7] "[DataSet\\DataSetInfos\\Module]"     "[DataSet\\Calibration]"              "[DataSet\\Calibration\\Scanhead]"
# [10] "[DataSet\\Calibration\\Cantilever]"  "[DataSet\\Parameters]"               "[DataSet\\Parameters\\Approach]"
# [13] "[DataSet\\Parameters\\ZFeedback]"    "[DataSet\\Parameters\\Lithography]"  "[DataSet\\Parameters\\Imaging]"
# [16] "[DataSet\\Parameters\\SignalIO]"     "[DataSet\\Parameters\\Spectroscopy]" "[DataSet\\Parameters\\SPMSystem]"
# [19] "[DataSet-0:1]"                       "[DataSet-0:2]"                       "[DataSet-1:1]"
# [22] "[DataSet-1:2]"                       "[SetView]"                           "[SetView-View0]"
# [25] "[SetView-View1]"                     "[SetView-View2]"                     "[SetView-View3]"

# ######################################
# read NID file, AFM file
#
# Date: 2019-02-10
# Author: Thomas Gredig
#
# ######################################

# loads header of NanoSurf NID file as text vector
#
# str(t)
# List of 2
# $ header.len: num 12917
# $ header    : chr [1:626] "[DataSet]" "Version=2" "GroupCount=2" "Gr0-Name=Scan forward" ...
# > head(t[[2]])
# [1] "[DataSet]"             "Version=2"             "GroupCount=2"          "Gr0-Name=Scan forward"
# [5] "Gr0-ID=0"              "Gr0-Count=21"
read.NID_header <- function(filename) {
  if (!file.exists(filename)) { return(NULL) }

  con <- file(filename,"rb")
  rline = ''
  i=0
  dlen.header = 0
  while( TRUE ) {
    rline = readLines(con,n=1, skipNul=TRUE)
    if (substr(rline,1,2) == "#!" ) break
    i = i + 1
    dlen.header = dlen.header + nchar(rline, type="bytes") + 2
  }
  close(con)

  con <- file(filename,"rb")
  header = readLines(con, n=(i-1))
  close(con)

  list(header.len = dlen.header, header = header)
}



read.NID_headerItems <- function(filename) {
  # read the NID header
  k1 = read.NID_header(filename)[[2]]
  # separate groups
  from = grep("^\\[.*\\]$",k1, useBytes = TRUE)
  to = c(from[-1]-1, length(k1))
  itemslist <- mapply(
    function(x, y) return(k1[x:y]),
    x = from, y = to - 1,
    SIMPLIFY = FALSE
  )
  # add list with titles
  c(list(c('HEADERS',k1[from])),itemslist)
}


NID.checkFile <- function(filename) {
  # does file exist?
  if (!file.exists(filename)) return(-1)
  # length of file in bytes
  file.len = file.info(filename)$size

  # read header
  h = read.NID_header(filename)
  # get header length in bytes
  header.length = h[[1]]

  # get number of images and size of images
  q = get.NID_imageInfo(h[[2]])

  # compare file length with images + header +
  # 2 bytes for #! character, which is the beginning
  # of the images
  file.len - sum(q)*2 - header.length - 2
}



# returns a vector with the pixels per image, for example for an image
# with 4 channels, each 128x128, it would return
# [1] 16384 16384 16384 16384
#
get.NID_imageInfo <- function(header.string) {
  # split data sets
  from = grep('\\[DataSet-',header.string, useBytes = TRUE)
  to = c(from[-1]-1, length(header.string))

  itemslist <- mapply(
    function(x, y) return(header.string[x:y]),
    x = from, y = to,
    SIMPLIFY = FALSE
  )
  itemslist[[1]] <- NULL

  image.Lines <- lapply(itemslist,
                        function(x) {
                          x[grep('Lines',x)]
                        }
  )

  image.Points <- lapply(itemslist,
                        function(x) {
                          x[grep('Points',x)]
                        }
  )

  as.numeric(
    unlist(lapply(image.Lines, function(x) { sapply(strsplit(x,"="),'[[',2) }))
  ) -> nLines

  as.numeric(
    unlist(lapply(image.Points, function(x) { sapply(strsplit(x,"="),'[[',2) }))
  ) -> nPoints

  nLines*nPoints
}





