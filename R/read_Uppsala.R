#' read_Uppsala
#' 
#' Reads in Uppsala air pressure station data
#' 
#' @param infile file path of air pressure data
#' 
#' @keywords util
#' @export
read_Uppsala <- function(infile){
  ## read infile (converted to csv for size)
  rawdata <- read.fwf(infile, width=c(10, rep(6,20)))
  names(rawdata) <- c('Date', outer(c('Time', 'P'), 1:10, paste0))
  
  ## convert datestring to year month and day
  rawdata$Year <- as.numeric(sapply(strsplit(as.character(rawdata$Date), '-'), function(x) x[3]))
  rawdata$Month <- as.numeric(sapply(strsplit(as.character(rawdata$Date), '-'), function(x) x[2]))
  rawdata$Day <- as.numeric(sapply(strsplit(as.character(rawdata$Date), '-'), function(x) x[1]))
  ## remove date
  rawdata <- rawdata[-grep('Date', names(rawdata))]
  
  ## convert time to character and pressure to numeric
  for (i in grep('Time', names(rawdata))) rawdata[[i]] <- gsub(' ', '', as.character(rawdata[[i]]))
  for (i in grep('P', names(rawdata))) rawdata[[i]] <- as.numeric(rawdata[[i]])
  
  ## exclude last few years when mmHg were measured
  rawdata <- rawdata[-which(!is.na(rawdata$P1) & rawdata$P1 > 500), ]
  
  ## extract variable names to melt
  rawnames <- names(rawdata)
  meltnames <- c('Time', 'P')
  
  ## run the melt process
  rmelt <- list()
  for (mn in meltnames){
    mnames <- c('Year', 'Month', 'Day', rawnames[grep(paste0('^', gsub('\\.', '.\\.', mn)), rawnames)])    
    ## melt the dataframe
    mtmp <- melt(rawdata[,mnames], mnames[1:3], value.name=mn)
    ## extract time index (remove everything after the dot and all characters before)
    mtmp$Time.i <- as.numeric(gsub(mn, '', as.character(mtmp$variable)))
    rmelt[[mn]] <- mtmp[,-grep('variable', names(mtmp))]
    rm(mtmp, mnames)
  }
  
  ## merge to dataframe
  rawmelt <- Reduce(merge, rmelt)
  
  ## remove columns without time nor pressure
  rawmelt <- rawmelt[-which(apply(is.na(rawmelt[,c('Time', 'P')]), 1, all)), ]
  ## remove everything before 1760 and after 1839 as units have changed
  rawmelt <- rawmelt[-which(rawmelt$Year < 1760 | rawmelt$Year > 1838),]
  
  ## add in missing bits
  rawmelt$Tcorr <- 0
  rawmelt$Station <- 'Uppsala'
  rawmelt$P.units <- 'Swedish inches (dec tum)'
  rawmelt$Time[rawmelt$Time == 'am'] <- 'morning'
  rawmelt$Time[rawmelt$Time == 'pm'] <- 'evening'
  
  return(rawmelt) 
}