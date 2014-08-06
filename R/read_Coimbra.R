#' read_Coimbra
#' 
#' Reads in Coimbra air pressure station data
#' 
#' @param infile file path of air pressure data
#' 
#' @keywords util
#' @export
read_Coimbra <- function(infile){
  ## read infile
  rawdata <- readWorksheetFromFile(infile, sheet=1, startRow=2, endCol=9)
  
  ## convert to local date
  ## convert back asPOSIXct to text as in original
  rawdata$Local.time <- format(rawdata$Local.time, '%H:%M')
  datestring <- paste(apply(rawdata[,c('Year', 'Month', 'Day')], 1, paste, collapse='-'), rawdata$Local.time)
  rawdata$Local.date <- as.POSIXct(datestring, format='%F %H:%M', tz='UTC')

  ## convert Portuguese inches to mm  
  rawdata$Orig.pressure <- length2SI(rawdata[,5:7], base=c(27.5, 27.5/12, 27.5/12/4))
  rawdata$Pressure.units <- 'mm'
  
  ## convert Reaumur to Celsius
  rawdata$Orig.temperature <- temperature2SI(rawdata[['Temp..R']] + 0.25*rawdata[['X...R']], 'Re') - 273.15
  rawdata$Temperature.units <- 'C'

  ## reorder the time
  rawdata <- rawdata[order(rawdata$Local.date),]
  
  return(rawdata)
  
}