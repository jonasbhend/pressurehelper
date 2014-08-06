#' read_Turin
#' 
#' Reads in Swedish air pressure station data and transforms units
#' 
#' @param infile file path of air pressure data
#' 
#' @keywords util
#' @export
read_Turin <- function(infile){
  rawdata <- readWorksheetFromFile(infile, sheet='1787-1865.06', startRow=4)
  names(rawdata) <- c('Day', 'Month', 'Year', 'Location', 'Temperature1', 'Hour1', 'Temperature2', 'Hour2', 'Temperature3', 'Hour3', 'Tmin', 'Tmax', 'Tmin.hom', 'Tmax.hom', 'Precipitation', 'Fresh.snow', 'QFF')
  
  
  ## Use the homogenised Tmin and Tmax only
  rawmelt <- rawdata[,c('Day', 'Month', 'Year', 'Tmin.hom', 'Tmax.hom', 'QFF')]
  rawmelt$Hour <- 12
  datestring <- paste(apply(rawmelt[,c('Year', 'Month', 'Day')], 1, paste, collapse='-'), rawmelt$Hour)
  rawmelt$Local.date <- as.POSIXct(datestring, format='%F %H', tz='UCT')
  
  rawmelt$QFF.Tcorrect <- TRUE
  
  return(rawmelt)
}