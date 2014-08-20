#' read_Stockholm
#' 
#' Reads in Stockholm air pressure station data
#' 
#' @param infile file path of air pressure data
#' 
#' @keywords util
#' @export
read_Stockholm <- function(infile){
  ## read infile (converted to csv for size)
  rawdata <- read.table(infile, header=FALSE, na.strings=missvals, stringsAsFactors=FALSE)
  ## throw out empty columns
  rawdata <- rawdata[,apply(rawdata, 2, function(x) any(!is.na(x)))]
  
  ## column names (according to README)
  names(rawdata) <- c('Year', 'Month', 'Day', 'P1', 'TP1', 'P2', 'TP2', 'P3', 'TP3')
  
  ## get observation times
  infile2 <- gsub('Stockholm', 'Stockholm_obs_hours', infile)
  rawdata2 <- read.table(infile2, header=FALSE, na.strings=missvals, stringsAsFactors=FALSE)
  names(rawdata2) <- c('Year', 'Month', 'Day', 'Time1', 'Time2', 'Time3')
  
  ## merge times and data
  rawdata <- merge(rawdata, rawdata2[1:nrow(rawdata),])
  rm(rawdata2)
  
  ## find out whether data is in compact or long already
  rawnames <- names(rawdata)
  
  ## extract variable names to melt
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
  rawmelt <- Reduce(merge, rmelt)
  
  ## convert back time from CET to local (assuming CET leads UTC by +1 hour)
  rawmelt$Time <- rawmelt$Time + 18.05 * 24/360 - 1
  
  rawmelt$Tcorr <- 0
  rawmelt$Station <- 'Stockholm'
  rawmelt$TP.units <- 'C'
  rawmelt$P.units <- 'Swedish inches (dec tum)'
  
  return(rawmelt) 
}