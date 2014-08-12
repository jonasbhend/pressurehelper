#' read_Karlsruhe
#' 
#' Reads in Karlsruhe air pressure station data
#' 
#' @param infile file path of air pressure data
#' 
#' @keywords util
#' @export
read_Karlsruhe <- function(infile){
  ## read infile (converted to csv for size)
  rawdata <- read.csv(infile, sep=';', header=TRUE, na.strings=missvals)
  ## throw out empty columns
  rawdata <- rawdata[,apply(rawdata, 2, function(x) any(!is.na(x)))]
  
  ## only retain relevant bits
  rnames <- names(rawdata)
  rawdata <- rawdata[,c(1:3, grep('hr_z.h', rnames), grep('hr_pa', rnames), grep('hr_t', rnames), grep('hr_qf', rnames))]
  ## replace names with english
  names(rawdata)[1:3] <- c('Day', 'Month', 'Year')
  
  ## change times to character
  for (nn in paste0('hr_z.h', 1:3)) rawdata[[nn]] <- as.character(rawdata[[nn]])
  
  ## check for missing times and replace with prominent observation time
  for (ii in 1:3){
    rawdata[[paste0('hr_miss.h', ii)]] <- FALSE
    t.i <- which(rawdata[[paste0('hr_z.h', ii)]] == '')
    if (length(t.i) > 0){
      dd <- rawdata[[paste0('hr_z.h', ii)]]
      rawdata[[paste0('hr_z.h', ii)]][t.i] <- names(table(dd))[which.max(table(dd))]
      rawdata[[paste0('hr_miss.h', ii)]][t.i] <- TRUE
    }
  }
  
  
  ## melt and merge
  rnames <- names(rawdata)
  dms <- list()
  for (nn in c('hr_z.h', 'hr_miss.h', 'hr_pa.m', 'hr_tp.m', 'hr_t.m', 'hr_qf.m')){
    mind <- c(1:3, grep(nn, rnames))
    dms[[nn]] <- melt(rawdata[,mind], c('Day', 'Month', 'Year'), value.name=nn)
    dms[[nn]]$hi <- gsub(nn, '', dms[[nn]]$variable)
    dms[[nn]] <- dms[[nn]][,-grep('variable', names(dms[[nn]]))]
  }
  
  rawmelt <- Reduce(merge, dms)
  ## remove ancillary variable
  rawmelt <- rawmelt[,-grep('hi', names(rawmelt))]
  
  ## change name to descriptive and convert units
  names(rawmelt)[names(rawmelt) == 'hr_z.h'] <- 'Time'
  names(rawmelt)[names(rawmelt) == 'hr_miss.h'] <- 'Time.missing'
  names(rawmelt)[names(rawmelt) == 'hr_pa.m'] <- 'P'
  rawmelt$P <- rawmelt$P/10
  rawmelt$P.units <- 'hPa'
  names(rawmelt)[names(rawmelt) == 'hr_tp.m'] <- 'TP'
  rawmelt$TP <- rawmelt$TP/10
  rawmelt$TP.units <- 'C'
  names(rawmelt)[names(rawmelt) == 'hr_t.m'] <- 'TA'
  rawmelt$TA <- rawmelt$TA / 10
  rawmelt$TA.units <- 'C'
  names(rawmelt)[names(rawmelt) == 'hr_qf.m'] <- 'QFF'
  rawmelt$QFF <- rawmelt$QFF/100
  rawmelt$Tcorr <- 1
  rawmelt$Station <- 'Karlsruhe'
  
  return(rawmelt)
  
}