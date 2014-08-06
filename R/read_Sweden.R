#' read_Sweden
#' 
#' Reads in Swedish air pressure station data and transforms units
#' 
#' @param infile file path of air pressure data
#' 
#' @keywords util
#' @export
read_Sweden <- function(infile){
  rawdata <- readWorksheetFromFile(infile, sheet=1, startRow=2)
  
  ## reorganise data frame
  ## only melt pressure readings first
  rawmelt <- melt(rawdata[, c(1:3, grep('inHg', names(rawdata)))], 1:3, value.name='Orig.pressure')
  rawmelt$Pressure.units <- 'inHg'
  ## also add in the temperature data
  if (length(grep('C', names(rawdata))) > 1){
    rawtmp <- melt(rawdata[, c(1:3, grep('X.C.', names(rawdata)))], 1:3, value.name='Orig.temperature')
    rawtmp$Temperature.units <- 'C'
    names(rawtmp)[grep('variable', names(rawtmp))] <- 'variable2'
    ## merge the two data frames 
    rawmelt <- merge(rawmelt, rawtmp)    
  }
  
  ## extract observing hours
  hour <- toupper(gsub('\\.', '', gsub('inHg.', '', rawmelt$variable)))

  ## correct hours if observing times differ throughout the year
  print(names(rawdata)[ncol(rawdata)])
  if ('X.from.May.to.October..6am.2pm.10pm' %in% names(rawdata)){
    ## set early hour to 6AM
    hour[rawmelt$Month %in% 5:10 & hour == '7AM'] <- '6AM'
    hour[rawmelt$Month %in% 5:10 & hour == '9PM'] <- '10PM'
  } else if ('X.6am.from.March.to.October' %in% names(rawdata)){
    hour[rawmelt$Month %in% 3:10 & hour == '7AM'] <- '6AM'
  }
    
  ## convert to local date
  datestring <- paste(apply(rawmelt[,c('Year', 'Month', 'Day')], 1, paste, collapse='-'), hour)
  rawmelt$Hour <- hour
  rawmelt$Local.date <- as.POSIXct(datestring, format='%F %I%p', tz='UCT')
  
  ## order by time
  rawmelt <- rawmelt[order(rawmelt$Local.date),]
  
  ## convert units of pressure reading
  rawmelt$mmHg <- length2SI(rawmelt$Orig.pressure, units='tum') * 1000
  rawmelt$hPa <- 13595.1 * 9.81 * rawmelt$mmHg * 10**(-5)
  rawmelt$hPa.Tcorrect <- FALSE
  
  ## remove the variable names
  rawmelt <- rawmelt[,-grep('variable', names(rawmelt))]
  
  return(rawmelt)
}