#' Converts AFM data to a 2D matrix
#' 
#' @param obj AFMdata object
#' @param no channel number
#' @return Numeric matrix of `z` values with `y` rows and `x` columns.
#' @author Thomas Gredig
#' @importFrom ggplot2 ggplot geom_path scale_color_discrete xlab theme_bw theme
#'
#' @examples
#' \dontrun{
#' filename = AFM.getSampleImages(type='ibw')
#' d = AFM.import(filename)
#' m <- AFM.to_CSV(d)
#' write.csv(m, "data.csv")
#' }
#' @export
AFM.to_CSV <- function(obj, no=1) {
  df <- AFM.raster(obj, no=no)
  num_x = obj@x.pixels
  num_y = obj@y.pixels

  matrix(df$z, nrow = num_y, ncol = num_x)
}
