#' @name compact2long
#' @aliases missvals
#' 
#' @title compact2long
#' 
#' @description
#' Converts tables in compact format to long format
#' 
#' @param infile file path to input file (xls[x])
#' 
#' @details
#' The following standard names in the data field of the compact form are reserved
#' \itemize{
#'   \item{Year, Month, Day, Time:}{date indicators}
#'   \item{P:}{pressure reading (in mm, in, or other)}
#'   \item{QFE:}{station pressure (in hPa)}
#'   \item{QFF:}{pressure at sea level}
#'   \item{TA:}{outside temperature}
#'   \item{TP:}{temperature at barometer}
#' }
#' 
#' @keywords util
#' @export
#' 
compact2long <- function(infile){
  ## load the input file
  wb <- loadWorkbook(infile)
  ## set empty cells to missing
  setMissingValue(wb, value=missvals)
  
  ## read the header
  header <- readWorksheet(wb, sheet=1, endRow=10, endCol=2, header=FALSE)
  
  ## read in the data
  rawdata <- readWorksheet(wb, sheet=1, startRow=11)
  
  ## remove columns that are blank (all missing values)
  rawdata <- rawdata[,apply(rawdata, 2, function(x) any(!is.na(x)))]
  
  ## convert time back to character (from date)
  time.i <- grep('Time', names(rawdata))
  if (length(time.i) > 0){
    for (t.i in time.i){
      if (any(class(rawdata[[t.i]]) %in% c('Date', 'POSIXt', 'POSIXct', 'POSIXlt'))){
        rawdata[[t.i]] <- as.character(format(rawdata[[t.i]], '%H:%M'))        
      }
    }
  }
  
  ## find out whether data is in compact or long already
  rawnames <- names(rawdata)
  nameend <- substr(rawnames, nchar(rawnames), nchar(rawnames))
  
  ## check if multiple instances of the same name exist 
  ## (denoted by trailing 1, 2, 3)
  if (any(nameend == '1')){
    
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
    out <- Reduce(merge, rmelt)
    
  } else {
    out <- rawdata
  }
  
  ## append information in header
  if (any(!is.na(header[,2]))){
    for (nn in header[!is.na(header[,2]),1]) {
      out[[nn]] <- header[header[,1] == nn,2]
    }
  } else {
    warning('No header information in file')
  }
  
  ## check on format of Temperature correction flag
  if (!is.null(out$Tcorr)){
    out$Tcorr <- as.numeric(out$Tcorr)
  }
  
  ## clear garbage
  gc()
  
  ## return data frame
  invisible(out)
  
}

#' @rdname compact2long
#' @export
missvals <- c(-999, -9999, -99999, -999.9, -9999.9, -99999.9, -999.99, -9999.99)
