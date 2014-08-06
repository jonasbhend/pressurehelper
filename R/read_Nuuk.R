#' read_Nuuk
#' 
#' Reads in Nuuk air pressure station data
#' 
#' @param infile file path of air pressure data
#' 
#' @keywords util
#' @export
read_Nuuk <- function(infile){
  ## read infile
  rawdata <- readWorksheetFromFile(infile, sheet=1)
  names(rawdata) <- c('Year', 'Month', 'Day', 'P.1', 'l.1', 'P.2', 'l.2', 'P.3', 'l.3')
  
  ## melt and merge
  timestring <- c('morning', 'midday', 'evening')
  time.hour <- c('07:00', '14:00', '20:00')
  dms <- list()
  for (nn in c('P', 'l')){
    dms[[nn]] <- melt(rawdata[c(1:3, grep(nn, names(rawdata)))], c('Year', 'Day', 'Month'), value.name=nn)
    dms[[nn]]$Local.time <- time.hour[as.numeric(gsub(paste0(nn, '.'), '', dms[[nn]]$variable))]
    dms[[nn]]$Time.txt <- timestring[as.numeric(gsub(paste0(nn, '.'), '', dms[[nn]]$variable))]
    dms[[nn]] <- dms[[nn]][,-grep('variable', names(dms[[nn]]))]
  }
  
  ## merge to output data
  rawmelt <- Reduce(merge, dms)
  
  ## convert to local date
  ## convert back asPOSIXct to text as in original
  datestring <- paste(apply(rawmelt[,c('Year', 'Month', 'Day')], 1, paste, collapse='-'), rawmelt$Local.time)
  rawmelt$Local.date <- as.POSIXct(datestring, format='%F %H:%M', tz='UTC')
  
  ## convert Danish inches to mm  
  rawmelt$Orig.pressure <- length2SI(rawmelt[,c('P', 'l')], base=c(313.85/12, 313.85/12**2))
  rawmelt$Pressure.units <- 'mm'
  
  ## reorder the time
  rawmelt <- rawmelt[order(rawmelt$Local.date),]
  
  return(rawdata)
  
}