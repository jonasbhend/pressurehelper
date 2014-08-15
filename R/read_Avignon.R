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
  wb <- loadWorkbook(infile)
  ## set missing values
  setMissingValue(wb, missvals)
  ## read in data
  rawdata <- readWorksheet(wb, sheet='Table 1816', startRow=5, endCol=12, endRow=371)
  names(rawdata) <- c('Montext', 'Day', 'TP1', 'TP4', 'noval', 'TP2', 'TP3', 'noval', 'P1', 'P2', 'P3', 'P4')
  ## remove empty columns
  rawdata <- rawdata[,-grep('noval', names(rawdata))]
  ## convert month text to Months
  rawdata$Month <- cumsum(!is.na(rawdata$Montext))
  rawdata$Year <- 1816
  rawdata$Time1 <- '06:00'
  rawdata$Time2 <- '10:00'
  rawdata$Time3 <- '12:00'
  rawdata$Time4 <- '14:00'
  rawdata <- rawdata[,-grep('Montext', names(rawdata))]
  
  ## extract variable names to melt
  rawnames <- names(rawdata)
  meltnames <- setdiff(rawnames, c('Year', 'Month', 'Day'))
  ## only replace first number (time indicator, less than nine observing times)
  meltnames <- unique(sub('[0-9]', '', meltnames))
  
  ## run the melt process
  rmelt <- list()
  for (mn in meltnames){
    mnames <- c('Year', 'Month', 'Day', rawnames[grep(paste0('^', gsub('\\.', '.\\.', mn)), rawnames)])    
    ## melt the dataframe
    mtmp <- melt(rawdata[,mnames], mnames[1:3], value.name=mn)
    ## extract time index (remove everything after the dot and all characters before)
    vartmp <- gsub('\\..$', '', mtmp$variable)
    vartxt <- unique(sub('[0-9]$', '', vartmp))
    if (length(vartxt) > 1) print(vartxt)
    mtmp$Time.i <- as.numeric(gsub(vartxt, '', vartmp))
    rmelt[[mn]] <- mtmp[,-grep('variable', names(mtmp))]
    rm(mtmp, vartxt, vartmp, mnames)
  }
  
  ## merge to dataframe
  out <- Reduce(merge, rmelt)
  print("Converting pressure and temperature to numeric")
  out$TP <- as.numeric(out$TP)
  out$P <- as.numeric(out$P)
  out$Station <- 'Avignon'
  out$TP.units <- 'C'
  out$P.units <- 'mm'
  out$Tcorr <- 0
  
  return(out)
}