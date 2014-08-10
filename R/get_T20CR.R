#' get_T20CR
#' 
#' get Temperature climatology from 20CR
#' 
#' @param lon longitude of station in degrees
#' @param lat latitude of station in degrees
#' @param time of measurement in UTC
#' @param tfile file path to temperature climatologies
#' @param tfilter filter weights to smooth temperature climatology
#' 
#' @keywords util
#' @export
get_T20CR <- function(lon, lat, time, tfile='~/Unibe/20CR/tmean.2m.1871-1900.nc', tfilter=rep(1/11, 11)){
  ## check if stationary or moving
  nlon <- length(unique(lon))
  nlat <- length(unique(lat))
  if (nlon == 1 & nlat == 1){
    nc <- open.ncdf(tfile)
    lons <- nc$dim$lon$vals
    lons[lons > 180] <- lons[lons > 180] - 360
    lon.i <- which.min((unique(lon) - lons)**2)
    lat.i <- which.min((unique(lat) - nc$dim$lat$vals)**2)
    tall <- get.var.ncdf(nc, 'tmean', start=c(lon.i, lat.i, 1), count=c(1,1,-1))

    ## filter climatology
    if (length(tfilter) > 1){
      tfilter <- tfilter / sum(tfilter)
      tall.arr <- array(tall, c(8, length(tall)/8))
      tall.filt <- t(apply(tall.arr, 1, filter, tfilter, circular=TRUE))
      tall <- as.vector(tall.filt)
    }
    
    nctimes <- as.POSIXct('1800-01-01 00:00:00', tz='UTC') + 3600*nc$dim$time$vals
    ## define times where everything but the year changes
    virttimes <- as.POSIXct(paste0(unique(format(nctimes, '%Y')), '-', format(time, '%m-%d %H:%M')), format='%F %H:%M', tz='UTC')
    ## get temperature at nearest obs time out of ncfile 
    ## (won't work well for change of year)
    time.i <- sapply(virttimes, function(x) which.min(as.numeric(x - nctimes)**2))
    temp.out <- tall[time.i]
    
  } else {
    stop("Moving stations (e.g. measurements during trips) not implemented yet")
  }

}