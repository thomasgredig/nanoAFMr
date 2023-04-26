#' read spectroscopy channel data
#'
#' @param obj AFMdata object
#' @returns channels from spectroscopy header
#'
#' @importFrom methods is
#'
#' @export
AFM.specData <- function(obj) {
  if (!is(obj,'AFMdata')) stop("Input must be an AFMdata object.")
  
  if (AFM.dataType(obj) != 'spectroscopy') stop("AFMdata object must have spectroscopy data.")
  specHead = obj@data$specHead
  rChannels = AFM.specHeader(obj)

  channelNumber = asInt(specHead[385:386])
  channelAvg = asInt(specHead[387:388])
  channelRes = asInt(specHead[389:390])
  channelPoints = asInt(specHead[391:392])
  channelDrvingSourceIndex = asInt(specHead[393:394])
  channelForward = int2double(specHead[395:398])
  channelBackward = int2double(specHead[399:402])
  channelForwardSpeed = int2double(specHead[403:406])
  channelBackwardSpeed = int2double(specHead[407:410])
  Volume.Image = asInt(specHead[411:412])

  specData = obj@data$specData
  if (!(length(specData) == channelPoints*channelRes*channelNumber)) warning("Spec Data does not have the correct length.")
  
  r = data.frame()
  s=1
  for(pt in 1:channelPoints) {
    c1 = data.frame(pt = rep(pt, channelRes))
    for(no in 1:channelNumber) {
      c2 = specData[s:(s+channelRes-1)] * rChannels$channelGain[no]
      c1 = cbind(c1,c2)
      s=s+channelRes
    } 
    names(c1) = c("Point", rChannels$channelName[1:channelNumber])
    r = rbind(r, c1)
  }
  
  r
}


# takes 2 small integers (16-bit) and returns a "gint" (32-bit integer)
asInt <- function(v) { v[2]*2^16 + v[1] }
