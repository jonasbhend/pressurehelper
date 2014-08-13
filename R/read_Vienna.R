#' read_Vienna
#' 
#' read station pressure from Vienna
#' 
#' @param infile input file path
#' 
#' @keywords util
#' @export
read_Vienna <- function(infile){

  ## read in data
  rawdata <- read.table(infile, stringsAsFactors=F, header=TRUE, sep=';')
  
  ## convert missing data
  rawdata <- rawdata[-1,]
  rawdata <- as.data.frame(lapply(rawdata, function(x) {
    xout <- as.character(x)
    xout[xout == substr('999999999999999999999', 1, median(nchar(xout)))] <- NA
    return(xout)}), stringsAsFactors=FALSE)
  
  ## retain interesting columns
  rawdata <- rawdata[c('datum', 't7', 't14', 't19', 'druck07', 'druck14', 'druck19')]
  
  ## convert to numeric
  rawdata <- as.data.frame(lapply(rawdata, as.numeric), stringsAsFactors=FALSE)
  
  ## rename the columns
  names(rawdata) <- c('datum', 'TA1', 'TA2', 'TA3', 'P1', 'P2', 'P3')
  
  ## change the units
  rawdata[,2:7] <- rawdata[,2:7] / 10
  
  ## convert dates
  rawdata$Year <- floor(rawdata$datum / 10000)
  rawdata$Month <- floor((rawdata$datum%%10000)/100)
  rawdata$Day <- rawdata$datum %% 100
  rawdata <- rawdata[-grep('datum', names(rawdata))]
  
  ## exclude rows with all missing
  rawdata <- rawdata[apply(!is.na(rawdata), 1, any),]
  
  ## according to metadata, observation times are 08:00, 15:00, and 22:00
  rawdata$Time1 <- '08:00'
  rawdata$Time2 <- '15:00'
  rawdata$Time3 <- '22:00'
  
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
  
  
  out$Station <- 'Vienna'
  out$Tcorr <- 0
  out$P.units <- 'hPa'
  out$TA.units <- 'C'
  
  return(out)
  
}