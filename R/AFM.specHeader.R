#' read spectroscopy channel header info
#'
#' @param obj AFMdata object
#' @returns channels from spectroscopy header
#'
#' @export
AFM.specHeader <- function(obj) {
  if (!is(obj,'AFMdata')) stop("Input must be an AFMdata object.")

  if (AFM.dataType(obj) != 'spectroscopy') stop("AFMdata object must have spectroscopy data.")
  specHead = obj@data$specHead

  r = data.frame()
  specHeadLen = 48
  maxChannels = 8
  for(j in 1:maxChannels) {
    ss = 1+(j-1)*specHeadLen
    r = rbind(r, data.frame(
      channelName = intToUtf8(specHead[ss:(ss+31)]),
      channelUnit = intToUtf8(specHead[(ss+32):(ss+39)]),
      channelGain = int2double(specHead[(ss+40):(ss+43)]),
      channelXaxis = specHead[ss+44],
      channelYaxis = specHead[ss+46],
      offset = int2double(specHead[(409+j*4):(412+j*4)]),
      logScale = asInt(specHead[(443+j*2):(444+j*2)]),
      square = asInt(specHead[(459+j*2):(460+j*2)])
    )
    )
  }
  # channelNumber = asInt(specHead[385:386])
  # channelAvg = asInt(specHead[387:388])
  # channelRes = asInt(specHead[389:390])
  # channelPoints = asInt(specHead[391:392])
  # channelDrvingSourceIndex = asInt(specHead[393:394])
  # channelForward = int2double(specHead[395:398])
  # channelBackward = int2double(specHead[399:402])
  # channelForwardSpeed = int2double(specHead[403:406])
  # channelBackwardSpeed = int2double(specHead[407:410])
  # Volume.Image = asInt(specHead[411:412])

  r
}


# takes 2 small integers (16-bit) and returns a "gint" (32-bit integer)
asInt <- function(v) { v[2]*2^16 + v[1] }
