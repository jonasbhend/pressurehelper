#' read_Prague
#' 
#' Reads in Prague air pressure station data
#' 
#' @param infile file path of air pressure data
#' 
#' @keywords util
#' @export
read_Prague <- function(infile){
  ## read infile
  rawdata <- readWorksheetFromFile(infile, sheet=1, startRow=1)
  names(rawdata) <- c('Year', 'Month', 'Day', 'P')
  rawdata$P.units <- 'mm'
  rawdata$Tcorr <- 1
  rawdata$Time <- '14:00'
  rawdata$Station <- 'Prague'

  return(rawdata)
  
}