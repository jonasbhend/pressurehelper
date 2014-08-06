#' read_Avignon
#' 
#' Reads in Avignon air pressure station data
#' 
#' @param infile file path of air pressure data
#' 
#' @keywords util
#' @export
read_Avignon <- function(infile){
  ## read infile
  rawdata <- readWorksheetFromFile(infile, sheet='Table 1816', startRow=5, endCol=12)
  names(rawdata) <- c('Montext', 'Day', 'Temp.6h', 'Temp.14h', 'noval', 'Temp.10h', 'Temp.12h', 'noval', 'mmHg.6h', 'mmHg.10h', 'mmHg.12h', 'mmHg.14h')
  ## remove empty columns
  rawdata <- rawdata[,-grep('noval', names(rawdata))]
  ## convert month text to Months
  rawdata$Month <- cumsum(!is.na(rawdata$Montext))
  rawdata$Year <- 1816
  
  ## melt the data frame for Temperature and Pressure
  df1 <- melt(rawdata[, -grep('Temp', names(rawdata))], c('Year', 'Month', 'Day', 'Montext'), value.name='Orig.pressure')
  df1$hour <- gsub('mmHg.', '', df1$variable)
  df1$Pressure.units <- 'mm'
  df2 <- melt(rawdata[,-grep('mmHg', names(rawdata))], c('Year', 'Month', 'Day', 'Montext'), value.name='Orig.temperature')
  df2$hour <- gsub('Temp.', '', df2$variable)
  df2$Temperature.units <- 'C'
  
  ## combine the two data frames
  rawmelt <- merge(df1[, -grep('variable', names(df1))], df2[, -grep('variable', names(df2))])
  
  ## compute local date
  datestring <- paste(apply(rawmelt[,c('Year', 'Month', 'Day')], 1, paste, collapse='-'), rawmelt$hour)
  rawmelt$Local.date <- as.POSIXct(datestring, format='%F %Hh', tz='UCT')
  
  ## reorder data frame
  rawmelt <- rawmelt[order(rawmelt$Local.date), ]
  
  ## convert units to numeric for temperature and pressure (missing data, etc.)
  rawmelt$Orig.temperature <- as.numeric(rawmelt$Orig.temperature)
  rawmelt$Orig.pressure <- as.numeric(rawmelt$Orig.pressure)
  
  return(rawmelt)
  
}