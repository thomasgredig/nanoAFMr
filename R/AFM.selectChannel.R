#' AFM select channels
#'
#' @description
#' Allows to select a specific channel in order to reduce the size of the image
#'
#' @param obj AFMdata object
#' @param no channel number to select
#' @returns AFMdata object with only the selected channel
#'
#' @author Thomas Gredig
#'
#' @seealso \code{\link{AFM.crop}}
#'
#' @export
AFM.selectChannel <- function(obj, no) {
  if ((no<1) | (no > length(obj@channel))) return(obj)

  zData = obj@data$z[[no]]
  obj@data$z = list()
  obj@data$z[[1]] = zData

  obj@z.units = c(obj@z.units[no])
  obj@channel = c(obj@channel[no])

  obj
}
