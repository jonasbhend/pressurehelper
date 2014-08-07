#' expand_long
#' 
#' Expands the long format to fill missing columns
#' 
#' @param df input data frame in long format (standard names)
#' 
#' @keywords util
#' @export
expand_long <- function(df){
  
  ## convert local date to UTC
  df$UTC.date <- local2UTC(df$Local.date, df$longitude)
  
  ## add in temperature if not present (where TP.derived and TA.derived)
  if (is.null(df$TP.derived) | is.null(df$TA.derived)){
    
  }
  
  ## reorder data frame
  
  return(df)
}