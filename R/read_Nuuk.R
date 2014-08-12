#' read_Nuuk
#' 
#' Reads in Nuuk air pressure station data
#' 
#' @param infile file path of air pressure data
#' 
#' @keywords util
#' @export
read_Nuuk <- function(infile){
  ## load the input file
  wb <- loadWorkbook(infile)
  ## set empty cells to missing
  setMissingValue(wb, value=missvals)

  ## read the data
  rawdata <- readWorksheet(wb, sheet=1)
  names(rawdata) <- c('Year', 'Month', 'Day', 'P1.1', 'P1.2', 'P2.1', 'P2.2', 'P3.1', 'P3.2')
  
  rawdata$Time1 <- 'morning'
  rawdata$Time2 <- 'noon'
  rawdata$Time3 <- 'evening'

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
  out$P.units <- 'Danish inches'
  out$Station <- 'Nuuk'
  
  return(out)
  
}