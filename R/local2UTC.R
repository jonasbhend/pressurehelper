#' local2UTC
#' 
#' Converts local (solar) time to UTC based on simple longitude correction.
#' So far, this conversion does not take into account the varying length of
#' solar days throughout the year.
#' 
#' @param x a datum (local time) of class POSIXct to be converted
#' @param longitude longitude of the location in degree at which local time is
#' supplied. If no longitude is supplied the same datum should be returned.
#' 
#' @examples
#' dd <- as.POSIXct('1815-01-01 12:00:01', tz='UTC')
#' local2UTC(dd)
#' 
#' # not setting the time zone to UTC throws a warning
#' local2UTC(as.POSIXct('1815-01-01 12:00:01'))
#' 
#' ## noon at the date line is midnight in England
#' local2UTC(dd, 180)
#' local2UTC(dd, -180)
#' 
#' @keywords util
#' @export
local2UTC <- function(x, longitude=0){
  tz <- attr(as.POSIXlt(x), 'tzone')
  if (! 'UTC' %in% tz) {
    warning('Input time zone differs from UTC. Resetting time zone to UTC without conversion.')
    x <- as.POSIXct(format(x, '%F %T'), format='%F %T', tz='UTC')  
  }
  xout <- x - longitude/360 * 24 * 3600
  return(xout)
}