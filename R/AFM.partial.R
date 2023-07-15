#' Identifies Partial AFM Image
#'
#' @description
#' Sometimes an AFM image is recorded partially, this function
#' returns \code{TRUE}, if the image is a partial AFM image.
#'
#' @param afmd AFMdata object
#' 
#' @importFrom dplyr "%>%"
#'
#' @export
AFM.partial <- function(afmd) {
  afmd %>% AFM.raster() %>% tail(n=100) %>% (`[`)("z") -> df
  sd(df$z)==0
}
