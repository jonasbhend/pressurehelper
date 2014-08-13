#' read_Cadiz
#' 
#' Reads in Cadiz air pressure station data
#' 
#' @param infile file path of air pressure data
#' 
#' @keywords util
#' @export
read_Cadiz <- function(infile){
  
  rawdata <- read.table(infile, sep='\t', stringsAsFactors=FALSE, na.strings=-999)
  names(rawdata) <- c('Date', 'Time1', 'P1', 'Time2', 'P2', 'Time3', 'P3')
  
  ## convert dates
  rawdata$Date <- as.Date(rawdata$Date, format='%d-%m-%Y')
  rawdata$Year <- as.numeric(format(rawdata$Date, '%Y'))  
  rawdata$Month <- as.numeric(format(rawdata$Date, '%m'))  
  rawdata$Day <- as.numeric(format(rawdata$Date, '%d'))  
  rawdata <- rawdata[, -grep('Date', names(rawdata))]
  
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

  ## Units etc.
  out$P.units <- 'hPa'
  out$Tcorr <- 0
  out$Station <- 'Cadiz'
  
  return(out)
  
}