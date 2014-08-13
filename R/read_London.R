#' read_London
#' 
#' read station pressure at London station
#' 
#' @param infile input file name
#' 
#' @keywords util
#' @export
read_London <- function(infile){
  
  # read in data
  rawdata <- read.table(infile, skip=1, header=FALSE, stringsAsFactors=FALSE)
  names(rawdata) <- c('Year', 'Month', 'Day', 'Time', 'TA', 'TP', 'P')
  
  rawdata$TA.units <- 'F'
  rawdata$TP.units <- 'F'
  rawdata$P.units <- 'English inches'
  rawdata$Station <- 'London'
  
  return(rawdata)
  
}