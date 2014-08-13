#' read_Zwanenburg
#' 
#' read station pressure at Zwanenburg station
#' 
#' @param infile input file name
#' 
#' @keywords util
#' @export
read_Zwanenburg <- function(infile){
  
  # read in data
  rawdata <- read.table(infile, sep=',', skip=51, header=TRUE, stringsAsFactors=FALSE)
  
  ## convert to output format
  out <- data.frame(Station='Zwanenburg',
                    Year=floor(rawdata$YYYYMMDD/10000),
                    Month=floor((rawdata$YYYYMMDD%%10000)/100),
                    Day=rawdata$YYYYMMDD%%100,
                    Time=c('07:00', '13:00', '22:00')[as.numeric(rawdata$M)],
                    P.1=as.numeric(substr(as.character(rawdata$P), 1, 2)),
                    P.2=as.numeric(substr(as.character(rawdata$P), 3, 4)),
                    P.3=as.numeric(substr(paste0(as.character(rawdata$P), '-'), 5, 5)),
                    P.units='Rijnlandse inches-lines-quartsoflines',
                    TA=floor(rawdata$T/10),
                    TA.units='F',
                    stringsAsFactors=FALSE)
  
  ## change first observing time in December and January
  out$Time[out$Time == '07:00' & out$Month %in% c(1,12)] <- '08:00'

  print("Only use integer temperatures as digit after decimal point is unclear")
  
  return(out)
  
}