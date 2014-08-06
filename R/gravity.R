#' gravity
#' 
#' Compute gravity at latitude phi
#' 
#' @param phi latitude (in degrees)
#' 
#' @examples
#' ## gravity at equator
#' gravity(0)
#' ## gravity at pole
#' gravity(90)
#' 
#' @keywords util
#' @export
gravity <- function(phi){
  g <- 9.780327 * (1 + 0.0053024 * sin(phi/180*pi)**2 + 0.0000058 * sin(phi/180*pi)**2)
  return(g)
}