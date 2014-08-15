#' read_Sweden
#' 
#' Reads in Swedish air pressure station data and transforms units
#' 
#' @param infile file path of air pressure data
#' 
#' @keywords util
#' @export
read_Sweden <- function(infile){
  ## read infile
  wb <- loadWorkbook(infile)
  ## set missing values
  setMissingValue(wb, missvals)
  ## read in data
  rawdata <- readWorksheet(wb, sheet=1, startRow=2)
  
  ## reorganise data frame
  ## only melt pressure readings first
  rawmelt <- melt(rawdata[, c(1:3, grep('inHg', names(rawdata)))], 1:3, value.name='P')
  ## extract observing hour
  rawmelt$Time <- toupper(gsub('.*\\.', '', rawmelt$variable))
  rawmelt <- rawmelt[, -grep('variable', names(rawmelt))]
  ## also add in the temperature data
  if (length(grep('C', names(rawdata))) > 1){
    rawtmp <- melt(rawdata[, c(1:3, grep('X.C.', names(rawdata)))], 1:3, value.name='TP')
    ## extract observing hour
    rawtmp$Time <- toupper(gsub('.*\\.', '', rawtmp$variable))
    rawtmp <- rawtmp[, -grep('variable', names(rawtmp))]
    ## merge the two data frames 
    rawmelt <- merge(rawmelt, rawtmp)    
  }
  
  ## correct hours if observing times differ throughout the year
  print(names(rawdata)[ncol(rawdata)])
  if ('X.from.May.to.October..6am.2pm.10pm' %in% names(rawdata)){
    ## set early hour to 6AM
    rawmelt$Time[rawmelt$Month %in% 5:10 & rawmelt$Time == '7AM'] <- '6AM'
    rawmelt$Time[rawmelt$Month %in% 5:10 & rawmelt$Time == '9PM'] <- '10PM'
  } else if ('X.6am.from.March.to.October' %in% names(rawdata)){
    rawmelt$Time[rawmelt$Month %in% 3:10 & rawmelt$Time == '7AM'] <- '6AM'
  }

  ## convert units of pressure reading
  rawmelt$Tcorr <- 0
  rawmelt$TP.units <- 'C'
    
  ## add in the station name
  rawmelt$Station <- gsub('.*\\/', '', gsub('\\..*', '', infile))
  rawmelt$P.units <- 'Swedish inches (dec tum)'
  
  return(rawmelt)
}