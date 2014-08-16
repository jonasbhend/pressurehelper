#' gravity
#' 
#' Compute gravity at latitude phi
#' 
#' @param phi latitude (in degrees)
#' @param elevation elevation (in m)
#' 
#' @references
#' http://library.wmo.int/pmb_ged/wmo_8_en-2012.pdf
#' 
#' @examples
#' ## gravity at equator
#' gravity(0)
#' ## gravity at pole
#' gravity(90)
#' 
#' @keywords util
#' @export
gravity <- function(phi, elevation=0){
  ## gravity by latitude
  gphi <- 9.80620*(1 - 0.0026442 * cos(2*phi/180*pi) - 0.0000058 * cos(2*phi/180*pi)**2)
  ## correct for elevation
  gphih <- gphi - 0.000003086 * elevation ## assuming flat surface
  ## Helmert's formula
  ## g <- 9.780327 * (1 + 0.0053024 * sin(phi/180*pi)**2 + 0.0000058 * sin(phi/180*pi)**2)
  return(gphih)
}