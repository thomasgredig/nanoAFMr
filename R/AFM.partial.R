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
  afmd %>% AFM.raster() %>% tail(n=25) %>% (`[`)("z") -> df.bottom
  afmd %>% AFM.raster() %>% head(n=25) %>% (`[`)("z") -> df.top
  sd(df.bottom$z)==0 | sd(df.top$z)==0 
}
