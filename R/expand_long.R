#' expand_long
#' 
#' Expands the long format to fill missing columns
#' 
#' @param df input data frame in long format (standard names)
#' @param repository data frame containing station information available in repository
#' @param verbose logical, should information about processing steps be displayed?
#' 
#' @keywords util
#' @export
expand_long <- function(df, inventory=read_inventory(), verbose=TRUE){
  
  if (verbose) {
    print('##################################################')
    print(df$Station[1])
    print('--------------------------------------------------')
  }
  
  ## strip inventory down to relevant line
  station.i <- which(df$Station[1] == inventory$Standard.Name | df$Station[1] == inventory$Station)
  if (length(station.i) != 1) stop(paste('Station', df$Station[1], 'not in inventory'))
  inventory <- inventory[station.i, ]
  
  ## add and convert Longitudes and Latitudes if not present
  for (nn in c('Latitude', 'Longitude')){
    if (!is.null(df[[nn]])) {
      df[[paste0(nn, '.orig')]] <- df[[nn]]
      df[[nn]] <- dms2dd(df[[nn]])
      if (all(df[[nn]] == df[[paste0(nn, '.orig')]])) df[[paste0(nn, '.orig')]] <- NULL
    } else {
      df[[nn]] <- dms2dd(inventory[[nn]])
    }
  }
  df$Location.missing <- inventory$Location.Flag
  
  ## add and convert elevation
  if (is.null(df[['Elevation']])){
    df$Elevation <- as.numeric(gsub('~', '', inventory$Elevation))
  } else {
    df$Elevation <- as.numeric(gsub('~', '', df$Elevation))
  }
  df$Elevation.missing <- inventory$Elevation.Flag
  
  
  if (verbose) {
    print('')
    print('Convert time to as.POSIXct and to UTC')
    print('--------------------------------------------------')
  }

  ## convert time in files
  for (nn in c('Year', 'Month', 'Day')) df[[nn]] <- as.numeric(as.character(df[[nn]]))
  ## replace times if these are missing
  if (any(is.na(df$Time))){
    df$Time.missing <- is.na(df$Time)*1
    for (time.i in unique(df$Time.i[df$Time.missing == 1])){
      dtimes <- table(df$Time[df$Time.i == time.i & df$Time.missing == 0])
      df$Time[df$Time.i == time.i & df$Time.missing == 1] <- names(dtimes)[which.max(dtimes)]
    }
    #     dtimes <- unique(df[!is.na(df$Time),c('Time.i', 'Time')])
    #     dtimes <- dtimes[dtimes$Time.i %in% df$Time.i[df$Time.missing], ]
    #     if (any(table(dtimes$Time.i) > 1)) stop("Times are not unique, cannot fill in missing times")
    #     dtimes <- dtimes[order(dtimes$Time.i),]
    #     df$Time[df$Time.missing == 1] <- dtimes[df$Time.i[df$Time.missing == 1], 'Time'] 
  } else {
    df$Time.missing <- 0
  }
  df$Local.time <- convert_time(year=df$Year, 
                                month=df$Month, 
                                day=df$Day, 
                                time=df$Time, 
                                latitude=df$Latitude)
  ## set time flag to missing if hour part of time is different from original time)
  df$Time.missing[substr(df$Local.time, 1,2) != formatC(gsub('AM', '', gsub('PM', '', gsub(':.*', '', toupper(df$Time)))), width=2, flag='0')] <- 1
  ## remove original times if identical with derived time
  if (all(df$Local.time == df$Time & !is.na(df$Time))) df$Time <- NULL
  
  ## compute local date in standard internal format
  datestring <- paste0(df$Year, '-', 
                       formatC(df$Month, width=2, flag='0'), '-', 
                       formatC(df$Day, width=2, flag='0'), ' ', 
                       df$Local.time)
  df$Local.date <- as.POSIXct(datestring, format=c('%F %H:%M'), tz='UTC')
  
  ## reorder data frame for increasing time
  df <- df[order(df$Local.date), ]
  
  ## convert local date to UTC
  df$UTC.date <- local2UTC(df$Local.date, df$Longitude)
  
  if (verbose){
    print(paste('Range of local times:', paste(range(df$Local.date, na.rm=T), collapse=' to ')))
    print(paste('Range of UTC times:', paste(range(df$UTC.date, na.rm=T), collapse=' to ')))
  }
  
  ## convert temperatures from original units to deg. Celsius
  for (nn in c('TA', 'TP')){
    ## which columns are relevant
    Tnames <- setdiff(sort(names(df)[grep(paste0('^', nn), names(df))]), paste0(nn, '.units'))
    if (length(Tnames) > 0){
      
      if (verbose) {
        print('')
        print(paste('Convert',c(TA='air temperature', TP='temperature at barometer')[nn],'to degrees Celsius'))
        print('--------------------------------------------------')
      }
      
      if (length(Tnames) == 1){
        Temperature <- as.numeric(df[[Tnames]])
      } else if (length(Tnames) == 2) {
        print(paste('Range of temperature decimal column', paste(range(df[[Tnames[2]]], na.rm=T), collapse='-')))
        Temperature <- as.numeric(df[[Tnames[1]]]) + as.numeric(df[[Tnames[2]]]) / 10
        TisnotNA <- !is.na(as.numeric(df[[Tnames[1]]]))
        if (any(floor(Temperature)[TisnotNA] != as.numeric(df[[Tnames[1]]])[TisnotNA])) stop(paste('Problem with temperature decimal places in', nn))
      } else {
        stop("More than two temperature fields")
      }
      
      ## get temperature units
      T.units <- if (!is.null(df[[paste0(nn, '.units')]])) df[[paste0(nn, '.units')]][1] else inventory$T.units
      
      ## convert the temperatures
      if (is.null(T.units)) stop("Temperature units not known")
      df[[paste0(nn, '.orig')]] <- temperature2SI(Temperature, units=T.units, verbose=verbose) - 273.15
    }    
  }
  
  ## add in temperature from 20CR
  if (verbose) {
    print('')
    print('Read in temperature climatologies from 20CR')
    print('--------------------------------------------------')
  }
  if (is.null(df$TP.20CR) | is.null(df$TA.20CR)){
    ## get temperature climatologies from file for corresponding time
    Temp20CR <- get_T20CR(lon=df$Longitude, 
                          lat=df$Latitude, 
                          time=df$UTC.date, 
                          tfilter=rep(1/11, 11))
    ## add in the 20th century reanalysis temperatures  
    if (is.null(df$TP.20CR)) df$TP.20CR <- Temp20CR - 273.15
    if (is.null(df$TA.20CR)) df$TA.20CR <- Temp20CR - 273.15
  }
  
  if (is.null(df$QFF)){
    ## convert pressure reading
    if (verbose) {
      print('')
      print('Convert pressure')
      print('--------------------------------------------------')
    }
    P.unit <- df$P.units[1]
    ## simple conversions
    if (P.unit == 'hPa'){
      df$P.orig <- df$P
    } else {
      if (P.unit == 'mm'){
        df$mmHg <- df$P
      } else {
        if (verbose) print(paste('Convert from', P.unit, 'to mmHg'))
        P.names <- setdiff(names(df)[grep('^P', names(df))], c('P.units', 'P.orig'))
        ## replace X's with 11
        df[P.names] <- lapply(df[P.names], function(x){
          xout <- x
          xout[x == 'X'] <- 11
          xout <- as.numeric(xout)
          return(xout)
        })
        ## get the units and convert
        if (P.unit %in% c('inHG', 'inHg', 'English inches')){
          base <- 0.9144/36*c(1, 1/12, 1/12**2)
        } else if (P.unit == 'Swedish inches (dec tum)'){
          base <- 0.02969*c(1, 1/12, 1/12) ## Swedish inches, lines and points
        } else if (P.unit == 'Danish inches'){
          base <- 0.31385 / 12**c(1,2)
        } else if (P.unit == 'Portuguese inches-lines-quartsoflines'){
          base <- 0.0275  / c(1, 12, 12*4)
        } else if (length(grep('French inches', P.unit)) == 1){
          if (P.unit == 'French inches-lines-16thlines'){
            base <- 0.02707 * c(1, 1/12, 1/12/16)
          } else {
            base <- 0.02707 * c(1, 1/12, 1/12**2)
          }
        } else {
          stop('Units not implemented yet')
        }
        df$mmHg <- length2SI(as.matrix(df[,P.names]), units='', base=base[seq(P.names)], verbose=verbose) * 1000
        ## check the result of the conversion for non-sensical numbers
        mmHg.test <- length2SI(as.matrix(df[,P.names[1]]), units='', base=base[1], verbose=FALSE)*1000
        test.i <- which(abs(df$mmHg - mmHg.test) > base[1]*1000)
        if (length(P.names) == 3){
          mmHg.test <- length2SI(as.matrix(df[,P.names[1:2]]), units='', base=base[1:2], verbose=FALSE)*1000
          test.i <- c(test.i, which(abs(df$mmHg - mmHg.test) > base[2]*1000))
        }
        if (length(test.i) > 0){
          print('Conversion of pressure to mmHg is faulty, for the following values:')
          print(paste('Row:', test.i, apply(df[test.i,P.names], 1, paste, collapse=', ')))
          df$mmHg[test.i] <- NA
        }
        rm(mmHg.test, test.i)
      }
      
      ## convert mmHg to hPa and normalise gravity
      df$P.orig <- pressure2SI(df$mmHg, 0, df$Latitude)
    }
    
    
    ## Reduce to 0 degree Celsius
    if (verbose) {
      print('')
      print('Reduce pressure to 0 deg. C')
      print('--------------------------------------------------')
    }
    
    gamma <- 1.82e-4 ## thermal expansion of mercury at 0 deg. C
    Tname <- match(c('TP.orig', 'TP.20CR'), names(df))
    Tname <- names(df)[(Tname[!is.na(Tname)])[1]] ## grab the first possible Temperature
    ## assume that record is not temperature corrected if no information is available
    if (is.null(df$Tcorr)){
      if (verbose) print('Assuming that pressure is not reduced to 0º C')
      df$Tcorr <- 0
    }
    if (df$Tcorr[1] == 1){
      if (!is.null(df$Comments[1])){
        Tref <- gsub('.*reduced to ', '', df$Comments[1])
        TT <- strsplit(Tref, '°')[[1]]
        TTcelsius <- temperature2SI(as.numeric(TT[1]), TT[2]) - 273.15
        df$QFE <- (1 - gamma*TTcelsius)*df$P.orig
      } else {
        df$QFE <- df$P.orig
      }
    } else {
      print(paste('Using', Tname, 'for temperature reduction'))
      df$QFE <- (1 - gamma * df[[Tname]]) * df$P.orig
    }    
  } else {
    if (verbose){
      print('')
      print('Pressure AMSL already available')
      print('--------------------------------------------------')
      if (is.null(df$P.orig)) df$P.orig <- NA
      if (is.null(df$QFE)) df$QFE <- NA
    }
  }
  
  ## reorder data frame to be more easily readable
  dfnames <- c('Station', 'Longitude', 'Latitude', 'Location.missing', 'Elevation', 'Elevation.missing', 'UTC.date', 'Local.date', 'Year', 'Month', 'Day', 'Local.time', 'Time', 'Time.missing', names(df)[grep('^P', names(df))], names(df)[grep('^TP', names(df))], names(df)[grep('^TA', names(df))], 'T.units')
  dfnames <- intersect(dfnames, names(df))
  df <- df[,c(dfnames, setdiff(names(df), dfnames))]
  
  return(df)
}