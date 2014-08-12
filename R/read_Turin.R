#' read_Turin
#' 
#' Reads in Swedish air pressure station data and transforms units
#' 
#' @param infile file path of air pressure data
#' 
#' @keywords util
#' @export
read_Turin <- function(infile){
  ## read infile
  wb <- loadWorkbook(infile)
  ## set missing values
  setMissingValue(wb, missvals)
  ## read in data
  rawdata <- readWorksheet(wb, sheet='1787-1865.06', startRow=4)
  names(rawdata) <- c('Day', 'Month', 'Year', 'Location', 'TA1', 'Time1', 'TA2', 'Time2', 'TA3', 'Time3', 'Tmin', 'Tmax', 'Tmin.hom', 'Tmax.hom', 'Precipitation', 'Fresh.snow', 'QFF')
  
  
  ## Use the homogenised Tmin and Tmax only
  rawmelt <- rawdata[,c('Day', 'Month', 'Year', 'Tmin.hom', 'Tmax.hom', 'QFF')]
  rawmelt$Time <- '12:00'

  # specify whether pressure is reduced to temperature
  rawmelt$Tcorr <- 1
  rawmelt$P.units <- 'hPa'
  rawmelt$Station <- 'Turin'
  
  return(rawmelt)
}