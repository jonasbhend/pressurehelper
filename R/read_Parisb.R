#' read_Parisb
#' 
#' Reads in Paris series b air pressure station data
#' 
#' @param infile file path of air pressure data
#' 
#' @keywords util
#' @export
read_Parisb <- function(infile){
  ## read infile
  rawdata <- readWorksheetFromFile(infile, sheet=1, startRow=2)
  
  ## melt data frame
  rawmelt <- melt(rawdata, c('Year', 'Month', 'Day'), value.name='Orig.pressure')
  rawmelt$Pressure.units <- 'mmHg'
  ## convert variable name to hour
  rawmelt$Hour <- toupper(gsub('mmHg.', '', as.character(rawmelt$variable)))
  ## remove variable
  rawmelt <- rawmelt[,-grep('variable', names(rawmelt))]
  
  ## convert to local date
  ## convert back asPOSIXct to text as in original
  datestring <- paste(apply(rawmelt[,c('Year', 'Month', 'Day')], 1, paste, collapse='-'), rawmelt$Hour)
  rawmelt$Local.date <- as.POSIXct(datestring, format='%F %I%p', tz='UTC')
  
  ## reorder the time
  rawmelt <- rawmelt[order(rawmelt$Local.date),]
  
  return(rawmelt)
  
}