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

  ## check on filter
  tfilter <- tfilter / sum(tfilter)
  
  ## check if stationary or moving
  nlon <- length(unique(lon))
  nlat <- length(unique(lat))

  ## open NetCDF
  nc <- open.ncdf(tfile)
  lons <- nc$dim$lon$vals
  lons[lons > 180] <- lons[lons > 180] - 360
  lats <- nc$dim$lat$vals
  nctimes <- as.POSIXct('1800-01-01 00:00:00', tz='UTC') + 3600*nc$dim$time$vals
  ## close NetCDF file on exit  
  on.exit(close.ncdf(nc))
  
  ## define times where everything but the year changes
  virttimes <- as.POSIXct(paste0(unique(format(nctimes, '%Y')), '-', format(time, '%m-%d %H:%M')), format='%F %H:%M', tz='UTC')
  ## get temperature at nearest obs time out of ncfile 
  ## (won't work well for change of year)
  time.i <- sapply(virttimes, function(x) if (is.na(x)) NA else which.min(as.numeric(x - nctimes)**2))
  
  ## read in data at one point or all the data
  if (nlon == 1 & nlat == 1){
    print('Extract specific lon/lat from file')
    lon.i <- which.min((unique(lon) - lons)**2)
    lat.i <- which.min((unique(lat) - lats)**2)
    tall <- get.var.ncdf(nc, 'tmean', start=c(lon.i, lat.i, 1), count=c(1,1,-1))

    ## filter climatology
    if (length(tfilter) > 1){
      tall.arr <- array(tall, c(8, length(tall)/8))
      tall.filt <- t(apply(tall.arr, 1, filter, tfilter, circular=TRUE))
      tall <- as.vector(tall.filt)
    }
    
    temp.out <- tall[time.i]
    
  } else {
    print('Read in all data and extract lon/lat later')
    ## Get all the temperature values    
    tall <- get.var.ncdf(nc, 'tmean')
    tall <- array(tall, c(length(tall)/dim(tall)[3], dim(tall)[3]))
    ## repeat longitudes and latitudes for 2-d array
    lons <- rep(lons, nc$dim$lat$len)
    lats <- rep(lats, each=nc$dim$lon$len)
    
    ## find closest grid-points for each lon/lat pair
    loc.i <- apply(cbind(lon, lat), 1, function(x){
      if (all(!is.na(x))){
        which.min((lons - x[1])**2 + (lats - x[2])**2)
      } else {
        NA
      }
    })
    
    ## extract the grid-points (all times for filtering)
    tsub <- tall[loc.i,]
    
    ## filter by time step (and reorganise)
    tsub.filter <- array(aperm(apply(array(tsub, c(nrow(tsub), 8, ncol(tsub)/8)), c(1:2), filter, tfilter, circular=TRUE), c(2,3,1)), dim(tsub))
  
    ## select the appropriate time steps
    temp.out <- diag(tsub.filter[,time.i])

    ## clean up (probably not needed)
    rm(tall, tsub, tsub.filter)
    gc()
    
  }

  return(temp.out)
}