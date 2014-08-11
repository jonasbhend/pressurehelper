#' dms2dd
#' 
#' Function to convert degree minute seconds to decimal degrees
#'
#' @param x character with degrees (º) and minutes (')
#'   
#' @keywords util
#' 
#' @examples
#' dms <- c("4º15'", "-5º34'48\"")
#' dms2dd(dms)
#' # should be 4.25 and -5.58 respectively
#' 
#' @export
dms2dd <- function(x){
  
  if (all(is.numeric(x))){
    dec <- x
  } else {
    
    ## extract degrees, minutes and seconds
    ## suppress the warnings
    oldwarn <- options('warn')
    options(warn=-1)
    deg <- as.numeric(sapply(strsplit(x, '°'), function(x) x[1]))
    min <- as.numeric(sapply(strsplit(gsub(".*°", "", x), "'"), function(x) x[1]))
    sec <- as.numeric(gsub("\".*", "", gsub(".*'", "", x)))
    options(warn=oldwarn$warn)
    
    ## get the sign of the longitude or latitudes
    degsign <- rep(1, length(x))
    deg.i <- c(grep('W', x), grep('S', x), which(substr(x, 1, 1) == '-'))
    degsign[deg.i] <- -1
    
    ## set seconds and minutes to zero if missing
    min[is.na(min)] <- 0
    sec[is.na(sec)] <- 0
    
    ## compute decimal degrees
    dec <- deg + degsign*min/60 + degsign*sec/3600    
  }
  
  return(dec)
}