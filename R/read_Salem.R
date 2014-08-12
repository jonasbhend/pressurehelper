#' read_Salem
#' 
#' Reads in Salem air pressure station data
#' 
#' @param infile file path of air pressure data
#' 
#' @keywords util
#' @export
read_Salem <- function(infile){
  ## read infile
  ff <- list()
  monnames <- months(as.Date('1990-01-15') + seq(0,330,30))
  for (i in 1:140){
    mon.i <- ((i - 1) %% 4) * 3 + 1:3
    year <- read.table(infile, header=F, nrows=1, skip=33 * (i-1))
    ytmp <- read.table(infile, header=FALSE, skip=2 + 33 * (i-1), nrows=31, na.string='9999.9', row.names=NULL)
    names(ytmp) <- c('Day', outer(c('8AM', '10PM'), monnames[mon.i], function(x,y) paste(y, x, sep='.')))
    ytmp$Year <- as.numeric(year)
    if (i %% 4 == 1){
      ytmplist <- ytmp
    } else if (i %% 4 == 0) {
      ytmplist <- merge(ytmplist, ytmp)
      ff[[paste(year)]] <- ytmplist
    } else {
      ytmplist <- merge(ytmplist, ytmp)
    }
  }
  rm(ytmplist, ytmp)
  
  ## melt the individual series
  ff.melt <- lapply(ff, function(x){
    xmelt <- melt(x, c('Year', 'Day'), value.name='P')
    xmelt$Montext <- gsub('\\..*', '', xmelt$variable)
    xmelt$Time <- gsub('.*\\.', '', xmelt$variable)
    xmelt <- xmelt[,-grep('variable', names(xmelt))]
    return(xmelt)
  })
  
  ## reassemble to one data frame
  rawdata <- Reduce(rbind, ff.melt)
  
  ## convert to local date
  ## convert back asPOSIXct to text as in original
  rawdata$Month <- match(rawdata$Montext, monnames)
  datestring <- paste(apply(rawdata[,c('Year', 'Month', 'Day')], 1, paste, collapse='-'), rawdata$Time)
  rawdata$Local.date <- as.POSIXct(datestring, format='%F %I%p', tz='UTC')
  ## remove inexistent dates (e.g. February 31)
  rawdata <- rawdata[!is.na(rawdata$Local.date),]
  ## remove local date
  rawdata <- rawdata[, -grep('Local.date', names(rawdata))]
  
  ## Units etc.
  rawdata$P.units <- 'hPa'
  rawdata$Tcorr <- TRUE
  rawdata$Station <- 'Salem'
  rawdata$Comments <- 'reduced to 0°C'
  
  return(rawdata)
  
}