#' read_pressure
#' 
#' Read in the original (digitized) pressure series at different stations
#' 
#' @param station name of the station
#' 
#' @details
#' This is a simple wrapper function to read in the station data from Excel/ASCII.
#' For each new format (almost for every station), a new function exists. As this
#' may be unwieldy for short files, alternatively the original data may be manually
#' copied to the standard format.
#' 
#' The idea is to reorganise the original data to a long format (one observing time
#' per line) and convert the units to SI units in functions specific to each station.
#' From there, adding longitudes and latitudes, temperature (from climatology if not
#' in original files) and correcting to standard temperature (0 deg. C) and sea level
#' will be done automatically for all data read in.
#' 
#' @keywords util
#' @export
read_pressure <- function(station){
  infile <- list.files('../data', pattern=paste0('^', station, '\\.'), full.names=TRUE)
  if (!file.exists(infile)) stop(paste('File for station', station, 'does not exist'))
  if (station %in% c('Goteborg', 'Harnosand', 'Umea', 'Vaxjo')){
    out <- read_Sweden(station, infile=infile)
  } else if (station == 'Turin'){
    out <- read_Turin(station, infile=infile)
  } else {
    stop('Station not implemented yet (consider transforming manually)')
  }
  return(out)
}