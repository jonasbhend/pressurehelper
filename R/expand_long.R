#' expand_long
#' 
#' Expands the long format to fill missing columns
#' 
#' @param df input data frame in long format (standard names)
#' 
#' @keywords util
#' @export
expand_long <- function(df){
  
  ## convert time in files
  datestring <- paste(apply(df[,c('Year', 'Month', 'Day')], 1, paste, collapse='-'), df$Time)
  df$Local.date <- as.POSIXct(datestring, format=c('%F %H:%M'), tz='UTC')
  
  ## reorder data frame
  df <- df[order(df$Local.date), ]
  
  ## convert local date to UTC
  df$UTC.date <- local2UTC(df$Local.date, df$Longitude)
  
  ## add in temperature if not present (where TP.derived and TA.derived)
  if (is.null(df$TP.derived) | is.null(df$TA.derived)){
    ## get temperature climatologies from file for corresponding time
    
  }
  
  ## reorder data frame
  
  return(df)
}