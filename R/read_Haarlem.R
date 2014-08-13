#' read_Haarlem
#' 
#' read station pressure at Haarlem station
#' 
#' @param infile input file name
#' 
#' @keywords util
#' @export
read_Haarlem <- function(infile){
  
  # read in data
  rawdata <- read.table(infile, sep=',', skip=54, header=TRUE, stringsAsFactors=FALSE)
  
  ## convert to output format
  out <- data.frame(Station='Haarlem',
                    Year=floor(rawdata$YYYYMMDD/10000),
                    Month=floor((rawdata$YYYYMMDD%%10000)/100),
                    Day=rawdata$YYYYMMDD%%100,
                    Time=c('08:00', '13:00', '22:00')[as.numeric(rawdata$M)],
                    P.1=as.numeric(substr(as.character(rawdata$P), 1, 2)),
                    P.2=as.numeric(substr(as.character(rawdata$P), 3, 4)),
                    P.3=as.numeric(substr(paste0(as.character(rawdata$P), '-'), 5, 5))*3,
                    P.units='English inches',
                    TA=floor(rawdata$T/10),
                    TA.units='F',
                    stringsAsFactors=FALSE)

  print("Only use integer temperatures as digit after decimal point is unclear")
  
  return(out)
  
}