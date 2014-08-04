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
#' # should be 4.25 and 5.58 respectively
#' 
#' @export
dms2dd <- function(x){
  if (is.numeric(x)) stop('Cannot convert numeric values without units')
  
  ## extract degrees, minutes and seconds
  deg <- as.numeric(sapply(strsplit(x, 'º'), function(x) x[1]))
  min <- as.numeric(sapply(strsplit(gsub(".*º", "", x), "'"), function(x) x[1]))
  sec <- as.numeric(gsub("\"", "", gsub(".*'", "", x)))

  ## set seconds to zero if missing
  sec[is.na(sec)] <- 0
  
  ## compute decimal degrees
  dec <- deg + min/60 + sec/3600
  
  return(dec)
}