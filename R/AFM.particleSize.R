#' Find Particle Size in AFM image
#' 
#' @description
#' The image has to have particles that clearly contrast with the smooth
#' background.
#' 
#' @param obj AFMdata object
#' @param floorHeight pixels with height below this threshold will be removed as background
#' @param minParticleSize minimum pixel size of a particle
#' 
#' @examples
#' a <- AFM.artificialImage(21,21, type="calibration", verbose=FALSE)
#' q <- AFM.particleSize(a, floorHeight = 10)
#' print(q$ps.table)
#' 
#' 
#' @returns list with AFM image showing particles and also table with particle size
#' 
#' @export
AFM.particleSize <- function(obj, floorHeight = 0.8, minParticleSize = 6) {
  # mark all the particles, line by line
  # create a new image based on obj with the new data
  for(j in 1:obj@y.pixels) {
    AFM.getLine(obj, yPixel = j, dataOnly = TRUE) -> r
    r$z[which(r$z < floorHeight)] = 0
    r$z[which(r$z >= floorHeight)] = 1
    
    if (j==1)  {
      obj.particles <- AFM.setLine(obj, r$z, yPixel = j)
    } else {
      obj.particles <- AFM.setLine(obj.particles, r$z, yPixel = j)
    }
  }
  
  # fill in one height for each particle
  particle.no = 2
  afm.raster = AFM.raster(obj.particles)
  z = afm.raster$z
  maxPixels = obj.particles@x.pixels
  
  fillNeighbors <- function(pt, pNo, maxSize) {
    if (pt<1 | pt>(maxSize*maxSize)) return()
    if (z[pt]==1) {
      z[pt] <<-  pNo
      fillNeighbors(pt+1, pNo, maxSize)
      fillNeighbors(pt-1, pNo, maxSize)
      fillNeighbors(pt+maxSize, pNo, maxSize)
      fillNeighbors(pt-maxSize, pNo, maxSize)
      

      fillNeighbors(pt-maxSize-1, pNo, maxSize)
      fillNeighbors(pt-maxSize+1, pNo, maxSize)
      fillNeighbors(pt+maxSize-1, pNo, maxSize)
      fillNeighbors(pt+maxSize+1, pNo, maxSize)
    }
  }
  
  while(TRUE) {
    fPoint <- which(z==1)[1]
    if (is.na(fPoint)) break
    fillNeighbors(fPoint, particle.no, maxPixels)
    particle.no = particle.no+ 1
  }
  
  # particle sizes
  result = data.frame()
  pMax = max(z)
  for(j in 2:pMax) {
    z.particle <- which(z==j)
    if (length(z.particle) >= minParticleSize) {
      r = data.frame(no = j-1,
                     area.pixel = length(z.particle),
                     radius.units = signif(sqrt(length(z.particle) / pi) * obj.particles@x.conv, 3),
                     units = obj.particles@z.units,
                     x.min = min(z.particle %% obj.particles@x.pixels), # afm.raster$x[min(z.particle)],
                     y.min = ceiling(min(z.particle / obj.particles@y.pixels)),
                     x.max = max(z.particle %% obj.particles@x.pixels),
                     y.max = ceiling(max(z.particle / obj.particles@y.pixels))
      )
      result = rbind(result, r)
    }
  }
  
  # return the image and also the results
  list(afmData = obj.particles,
       ps.table = result)
}