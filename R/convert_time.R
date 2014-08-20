#' convert_time
#' 
#' Converts time in various formats to HH:MM
#' 
#' @param year numeric or character, year corresponding to time
#' @param month numeric or character, month of time
#' @param day numeric or character, day of time
#' @param time character vector of representations of time
#' @param latitude latitude of station for conversion of sunset/rise times
#' 
#' @details
#' Conversion of time to times in HH:MM format is based on the solar declination
#' from the insol package if times specify 'sunset' or 'sunrise'. In addition, 
#' the string representations of 'morning', 'noon', 'afternoon', 'evening' are
#' set (see example(convert_time))
#' 
#' @examples
#' time <- c('12PM', '12AM', 'sunrise', 'sunset','morning', 'noon',  'afternoon', 'evening', '12:00', '1:00', '13:13', '04:00')
#' cbind(time, convert_time(1815, 1, 1, time, latitude=45))
#' 
#' @keywords util
#' @export
#' 
convert_time <- function(year, month, day, time, latitude=NULL){
  ntime <- length(time)
  out <- rep(NA, ntime)
  
  ## check if time is numeric
  if (is.numeric(time)){
    
    hours <- formatC(floor(time), width=2, flag='0')
    minutes <- formatC(floor((time %% 1)*60), width=2, flag='0')
    out <- paste(hours, minutes, sep=':')
  
  } else {
    
    ## check if time is AM or PM
    ampm.i <- substr(toupper(time), nchar(time) - 1, nchar(time)) %in% c('AM', 'PM')
    if (sum(ampm.i) > 0) out[ampm.i] <- format(as.POSIXct(paste0(year, '-', month, '-', day, ' ', toupper(time))[ampm.i], format='%F %I%p', tz='UTC'), '%H:%M')  
    
    ## check if time is in standard format HH:MM
    ## also suppress warnings for this
    oldwarn <- options('warn')
    options(warn=-1)
    hhmm.i <- sapply(strsplit(as.character(time), ':'), function(x) all(!is.na(as.numeric(x))))
    options(warn=oldwarn$warn)
    if (sum(hhmm.i) > 0) {
      ## add leading zeroes
      ttmp <- paste0('000', time[hhmm.i])
      out[hhmm.i] <- substr(ttmp, nchar(ttmp) - 4, nchar(ttmp))
    }
    
    ## check if time is sunrise or sunset
    sun.i <- tolower(time) %in% c('sunrise', 'sunset', 'sunrise+12')
    if (sum(sun.i) > 0){
      if (is.null(latitude)) stop('Latitude is missing, needed to convert sunrise and sunset times')
      decl <- declination(JDymd(year, month, day))
      halfdaylength <- acos(-tan(latitude/180*pi)*tan(decl/180*pi))/pi*12
      sunsign <- c(-1, 1)[match(substr(tolower(time), 1, 6), c('sunris', 'sunset'))]
      sunadd <- rep(0, ntime)
      if (any(time == 'sunrise+12')) sunadd[which(time == 'sunrise+12')] <- 12
      suntime <- (12 + sunsign*halfdaylength)[sun.i]
      sunhour <- formatC(floor(suntime) + sunadd, width=2, flag='0')
      sunminute <- formatC(floor((suntime%%1)*60), width=2, flag='0')
      out[sun.i] <- paste(sunhour, sunminute, sep=':')
    }
    
    ## check if time is string
    txt.i <- tolower(time) %in% c('morning', 'noon', 'afternoon', 'evening')
    if (sum(txt.i) > 0){
      txt.key <- c(morning='08:00', noon='12:00', afternoon='16:00', evening='20:00')
      out[txt.i] <- txt.key[tolower(time)[txt.i]]
    }    
  }
  
  ## throw warning if times are not converted properly
  if (any(is.na(out[!is.na(time)]))){
    warning(paste('No conversion for', paste(unique(time[is.na(out) & !is.na(time)]), collapse=',')))
  }
  
  return(out)
}