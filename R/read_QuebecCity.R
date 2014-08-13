#' read_QuebecCity
#' 
#' read station pressure from Quebec City
#' 
#' @param infile input file path
#' 
#' @keywords util
#' @export
read_QuebecCity <- function(infile){
  
  ## read in data (do it the hard way as the leap years are malformatted, i.e. julday 366)
  fwidths <- list(c(4, -2, 3, rep(c(-1, 7), 10)), c(4,-2,3,rep(c(-1,7), 8)))
  fwidths <- fwidths[c(rep(1,365),2)]
  rfwf <- as.matrix(read.fwf(infile, width=fwidths, skip=1))
  
  ## expand matrix (add in missing columns for days 366 with missing values)
  rind <- c(seq(1, 365*12), 365*12 + c(1:2, NA, 3:6, NA, 7:10))
  rfwf <- rfwf[,rind]
  rfwf[rfwf == -999.9] <- NA
  
  ## convert back to data frame with one date per line
  rawdata <- data.frame(t(array(t(rfwf), c(12, length(rfwf)/12))))  
  ## add in original names
  names(rawdata) <- read.table(infile, header=FALSE, stringsAsFactors=FALSE, nrow=1)
  
  ## convert date
  rdate <- as.Date(paste(rawdata$year, formatC(rawdata$day, width=3, flag='0')), format='%Y %j')
  ## exclude non-sensical days
  rawdata <- rawdata[!is.na(rdate),]
  ## convert date again
  rdate <- as.Date(paste(rawdata$year, formatC(rawdata$day, width=3, flag='0')), format='%Y %j')
  
  out <- data.frame(Year=as.numeric(format(rdate, '%Y')),
                    Month=as.numeric(format(rdate, '%m')),
                    Day=as.numeric(format(rdate, '%d')),
                    Time1='8AM',
                    Time2='3PM',
                    P1=rawdata[['P8am_org_obs']],
                    QFE1=rawdata[['P8am_tadj']],
                    QFF1=rawdata[['P8am_tadj_slp']],
                    P2=rawdata[['P3pm_org_obs']],
                    QFE2=rawdata[['P3pm_tadj']],
                    QFF2=rawdata[['P3pm_tadj_slp']],
                    stringsAsFactors=FALSE)

  ## swap the names for melting (copy-paste)
  rawdata <- out
  
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
  
  ## add in missing variables
  out$P.units <- 'English inches'
  out$Tcorr <- 1
  out$Station <- 'Quebec City'
  
  return(out)
  
}