#' pressure2SI
#' 
#' Converts the absolute pressure in mmHg to hPa given the local temperature
#' 
#' @param pressure Local absolute pressure in mmHg
#' @param temperature Local temperature in deg. C
#' @param latitude latitude of location in degrees for gravity
#' 
#' @examples
#' ## should be 1000 hPa
#' pressure2SI(750, temperature=10, latitude=68.61)
#' 
#' @keywords util
#' @export
pressure2SI <- function(pressure, temperature, latitude){
  gamma <- 1.82e-4 ## thermal expansion of mercury at 0 deg. C
  rho <- 13.5951e3 ## density of mercury at 0 deg. C
  pressure.corr <- (1 - gamma * temperature) * pressure
  
  ## convert to hPa
  QFE <- rho * gravity(latitude) * pressure.corr * 1e-5
  
  return(QFE)
}