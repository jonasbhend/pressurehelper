#' QFE2QFF
#' 
#' Reduce absolute atmospheric pressure to sea level
#' 
#' @param QFE local atmospheric pressure (in hPa)
#' @param elevation height AMSL of barometer (in m)
#' @param latitude latitude of station (in degree)
#' @param temperature air temperature at station (in Kelvin)
#' 
#' @details
#' The mean temperature of the fictituous air column between station
#' elevation and sea level is estimated using a lapse rate of 0.0065 K/m.
#' 
#' @examples
#' ## should be a 1000 hPa at sea level
#' QFE2QFF(990, elevation=88, latitude=15, temperature=298.15)
#' QFE2QFF(990, elevation=88, latitude=15, temperature=25)
#' 
#' @keywords util
#' @export
QFE2QFF <- function(QFE, elevation, latitude, temperature){
  Rd <- 287.04 ## gas constant for dry air in J K-1 kg-1
  lapse <- 0.0065 ## assumed lapse rate for mean temperature

  ## check if temperature is in Kelvin 
  if (median(temperature, na.rm=T) < 200){
    temperature <- temperature + 273.15
  }
  
  ## compute virtual mean temperature below station
  Tm <- temperature + elevation * lapse / 2
  
  
  QFF <- QFE * exp(gravity(latitude, elevation)*elevation / Rd / Tm)
  return(QFF)
}