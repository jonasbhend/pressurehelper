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
  rawmelt <- melt(rawdata[, c(1:3, grep('inHg', names(rawdata)))], 1:3, value.name='P')
  rawmelt$P.units <- 'Swedish inches (dec tum)'
  ## also add in the temperature data
  if (length(grep('C', names(rawdata))) > 1){
    rawtmp <- melt(rawdata[, c(1:3, grep('X.C.', names(rawdata)))], 1:3, value.name='TP')
    rawtmp$TP.units <- 'C'
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

  ## add in time
  rawmelt$Time <- hour
  
  ## convert units of pressure reading
  rawmelt$Tcorr <- 0
  
  ## remove the variable names
  rawmelt <- rawmelt[,-grep('variable', names(rawmelt))]
  
  ## add in the station name
  rawmelt$Station <- gsub('.*\\/', '', gsub('\\..*', '', infile))
  
  return(rawmelt)
}