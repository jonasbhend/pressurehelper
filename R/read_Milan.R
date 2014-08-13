#' read_Milan
#' 
#' read station pressure at Milan station
#' 
#' @param infile input file name
#' 
#' @keywords util
#' @export
read_Milan <- function(infile){
  
  # read in data
  rawdata <- read.table(infile, skip=0, header=FALSE, stringsAsFactors=FALSE, na.strings=-999.9)
  names(rawdata) <- c('Day', 'Month', 'Year', 'Time1', 'P1', 'Time2', 'P2')
  
  rawdata$Time1 <- 'sunrise'
  rawdata$Time2 <- 'sunrise+12'
  
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
  
  out$P.units <- 'hPa'
  out$Tcorr <- 0
  out$Station <- 'Milan'
  
  return(out)
  
}