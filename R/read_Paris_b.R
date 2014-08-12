#' read_Paris_b
#' 
#' Reads in Paris series b air pressure station data
#' 
#' @param infile file path of air pressure data
#' 
#' @keywords util
#' @export
read_Paris_b <- function(infile){
  ## read infile
  wb <- loadWorkbook(infile)
  ## set missing values
  setMissingValue(wb, missvals)
  ## read in data
  rawdata <- readWorksheet(wb, sheet=1, startRow=2)
  
  ## melt data frame
  rawmelt <- melt(rawdata, c('Year', 'Month', 'Day'), value.name='P')
  rawmelt$P.units <- 'mm'
  ## convert variable name to hour
  rawmelt$Time <- toupper(gsub('mmHg.', '', as.character(rawmelt$variable)))
  ## remove variable
  rawmelt <- rawmelt[,-grep('variable', names(rawmelt))]
  ## convert pressure to numeric
  print('Convert from cm to mm (wrong metadata in .xls)')
  rawmelt$P <- as.numeric(rawmelt$P)*10
  ## add in station name
  rawmelt$Station <- 'Paris_b'
  ## add in flag for temperature reduction
  rawmelt$Tcorr <- 1
  
  return(rawmelt)
  
}