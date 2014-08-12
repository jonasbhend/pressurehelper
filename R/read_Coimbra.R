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
  rawdata$Time <- format(rawdata$Local.time, '%H:%M')
  rawdata <- rawdata[, -grep('Local.time', names(rawdata))]
  
  ## rename pressure
  names(rawdata)[grep('Inches', names(rawdata))] <- 'P.1'
  names(rawdata)[grep('Lines', names(rawdata))] <- 'P.2'
  names(rawdata)[grep('X..of.Lines', names(rawdata))] <- 'P.3'
  rawdata$P.units <- 'Portuguese inches-lines-quartsoflines'
  rawdata$Tcorr <- 0
  ## rename temperature
  rawdata$TP <- as.numeric(rawdata[['Temp..R']]) + 0.25*as.numeric(rawdata[['X...R']])
  rawdata$TP.units <- 'R'

  ## add in additional variables
  rawdata$Station <- 'Coimbra'
  
  ## exclude rows with values for June 31st
  rawdata <- rawdata[!(rawdata$Month == 6 & rawdata$Day == 31),]
  print('Exclude data for June 31, 1816')
  rawdata$Comments <- 'The source (Jornal de Coimbra) lists observations for June 31, 1816 (?!?). Not clear whether the data afterward (or before?) are shifted by one day with respect to the Gregorian calendar.'
  
  
  return(rawdata)
  
}