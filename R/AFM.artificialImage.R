#' Create an artificial AFMdata object
#'
#' @description
#' generates an AFMdata object, which is sometimes
#' useful for testing algorithms or models or to
#' explore AFM data numerically.
#'
#' @param width width in pixels
#' @param height height in pixels
#' @param minZ minimum z height
#' @param maxZ maximum z height
#' @param imageWidth width of image in nm
#' @param imageHeight height of image in nm
#' @param type can be random gradient calibration
#' @param addNoise if \code{TRUE}, add a bit of noise to the data
#' @param invert logical, if \code{TRUE}, top and bottom are inverted
#' @param verbose if \code{TRUE}, output additional information
#' 
#' @author Thomas Gredig
#' @return AFMdata object
#' 
#' @importFrom stats runif
#' 
#' @examples
#' a = AFM.artificialImage(type='calibration')
#' plot(a)
#' @export
AFM.artificialImage <- function(width = 10,
                                height = 10,
                                minZ = 0,
                                maxZ = 100,
                                imageWidth = 1000,
                                imageHeight = 1000,
                                type = c('random','gradient','calibration'),
                                addNoise = TRUE,
                                invert = FALSE,
                                verbose = TRUE) {

  if (missing(type)) type='random'
  # generate height data
  z = switch(type,
         'random' = rep(runif(width*height, min=minZ, max=maxZ)),
         'gradient' = rep(seq(from=minZ, to=maxZ, length=width), each=height),
         'calibration' = .artificialCalibration(width, height, minZ, maxZ)
           )
  if(length(z)==0) stop("Invalid type.")
  if (addNoise) z = z + rep(runif(width*height, min=0, max=(maxZ-minZ)*0.05))
  if (invert) z = -z

  # create AFMdata S4 class
  obj = AFMdata(
    data = list(z=list(z)),
    channel = "HeightTrace",
    x.conv = imageWidth / width,  # pixel per nm
    y.conv = imageHeight / height,
    x.pixels = width,
    y.pixels = height,
    z.conv = 1,
    z.units = "nm",
    instrument = "artificial",
    history = '',
    description = paste("Artificial image with width=",width, "and height=",height),
    fullFilename = "artificial.000"
  )

  if (verbose) print(paste("Instrument:", obj@instrument))
  obj
}

# creates the height of an artificial calibration grid
.artificialCalibration <- function(width, height, minZ, maxZ) {
  # create foundation
  z = rep(minZ, height*width)

  # add one pillar
  w2 = seq(from = round(width / 4), to = round(width * 0.75), by = 1)
  h2 = seq(from = round(height / 4), to = round(height * 0.75), by = 1)
  p = rep(h2*width, length(w2)) + rep(w2, each = length(h2))
  z[p] = maxZ

  z
}
