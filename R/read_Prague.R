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
  names(rawdata) <- c('Year', 'Month', 'Day', 'Orig.pressure')
  rawdata$Pressure.units <- 'mm'
  rawdata$Tcorrect <- TRUE
  rawdata$Local.time <- '14:00'
  
  ## convert to local date
  ## convert back asPOSIXct to text as in original
  datestring <- paste(apply(rawdata[,c('Year', 'Month', 'Day')], 1, paste, collapse='-'), rawdata$Local.time)
  rawdata$Local.date <- as.POSIXct(datestring, format='%F %H:%M', tz='UTC')  
  
  ## reorder the time
  rawdata <- rawdata[order(rawdata$Local.date),]
  
  return(rawdata)
  
}