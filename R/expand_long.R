#' expand_long
#' 
#' Expands the long format to fill missing columns
#' 
#' @param df input data frame in long format (standard names)
#' @param inventory data frame containing station information available in inventory
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
  ## check the stripped station name against standard name and long form against long name
  station.i <- which(gsub(' ', '', df$Station[1]) == inventory$Standard.Name | df$Station[1] == inventory$Station)
  if (length(station.i) != 1) stop(paste('Station', df$Station[1], 'not in inventory'))
  inventory <- inventory[station.i, ]
  
  ## include the full station name
  df$Station.name <- inventory[['Station']]
  
  ## add and convert Longitudes and Latitudes if not present
  for (nn in c('Latitude', 'Longitude')){
    if (!is.null(df[[nn]])) {
      df[[paste0(nn, '.orig')]] <- df[[nn]]
      df[[nn]] <- dms2dd(df[[nn]])
      if (all(df[[nn]][!is.na(df[[nn]])] == df[[paste0(nn, '.orig')]][!is.na(df[[nn]])])) df[[paste0(nn, '.orig')]] <- NULL
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
  ## insert Time if no time is present
  if (is.null(df[['Time']])){
    warning('Inserted time (14:00) as is missing', immediate. = FALSE)
    if (verbose) print('WARNING: insert time (14:00) as time is missing')
    df$Time <- '14:00'
    df$Time.missing <- 1
  }
  ## replace times if these are missing  
  if (any(is.na(df$Time))){
    df$Time.missing <- is.na(df$Time)*1
    ## replace missing times with most frequent observation
    if (!is.null(df$Time.i)){
      for (time.i in unique(df$Time.i[df$Time.missing == 1])){
        dtimes <- table(df$Time[df$Time.i == time.i & df$Time.missing == 0])
        df$Time[df$Time.i == time.i & df$Time.missing == 1] <- names(dtimes)[which.max(dtimes)]
      }      
    } else {
      dtimes <- table(df$Time[df$Time.missing == 0])
      df$Time[df$Time.missing == 1] <- names(dtimes)[which.max(dtimes)]
      if (verbose) print(paste('WARNING: replace missing times with', names(dtimes)[which.max(dtimes)]))
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
  
  ## convert pressure from whatever to QFE
  P.names <- setdiff(names(df)[grep('^P', names(df))], 'P.units')
  if (length(P.names) > 0){
    ## convert pressure reading
    if (verbose) {
      print('')
      print('Convert pressure to QFE')
      print('--------------------------------------------------')
    }
    P.unit <- df$P.units[1]
    ## simple conversions
    if (P.unit == 'hPa'){
      df$P.orig <- df$P
    } else {
      if (P.unit == 'mm'){
        if (is.null(df$mmHg)) df$mmHg <- df$P
      } else {
        if (verbose) print(paste('Convert from', P.unit, 'to mmHg'))
        ## replace X's with 11 and fix the Geneva series
        df[P.names] <- lapply(df[P.names], function(x){
          xout <- x
          xout[x == 'X'] <- 11
          ## fix Geneva series
          if (mean(xout == 0, na.rm=T) > 0.7){
            print("Fixing too many zeroes in pressure series")
            xtmp <- xout[!is.na(xout)]
            for (i in 2:length(xtmp)) xtmp[i] <- if (xtmp[i] == 0) xtmp[i-1] else xtmp[i]
            xout[!is.na(xout)] <- xtmp
          }
          xout <- as.numeric(xout)
          return(xout)
        })
        ## get the units and convert
        if (P.unit %in% c('inHG', 'inHg', 'English inches')){
          base <- 0.0254 / c(1, 12, 12**2)
        } else if (P.unit == 'Swedish inches (dec tum)'){
          base <- 0.02969 ## Swedish inches in decimal units
        } else if (P.unit == 'Danish inches'){
          base <- 0.31385 / 12**c(1,2)
        } else if (P.unit == 'Rijnlandse inches-lines-quartsoflines'){
          base <- 0.0262 / c(1,12,48)
        } else if (P.unit == 'Portuguese inches-lines-quartsoflines'){
          base <- 0.0275  / c(1, 12, 12*4)
        } else if (length(grep('French inches', P.unit)) == 1){
          if (P.unit == 'French inches-lines-16thlines'){
            base <- 0.02707 * c(1, 1/12, 1/12/16)
          } else if (P.unit == 'French inches-lines-quartsoflines'){
            base <- 0.02707 * c(1, 1/12, 1/12/4)
          } else {
            base <- 0.02707 * c(1, 1/12, 1/12**2)
          }
        } else {
          stop('Units not implemented yet')
        }
        if (is.null(df$mmHg)) df$mmHg <- length2SI(as.matrix(df[,P.names]), units='', base=base[seq(P.names)], verbose=verbose) * 1000
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
      df$P.orig <- pressure2SI(df$mmHg, 0, df$Latitude, df$Elevation)
    }
    
    ## check if QFE is already available
    if (is.null(df[['QFE']])){
      ## Reduce to 0 degree Celsius
      if (verbose) {
        print('')
        print('Reduce pressure to 0 deg. C')
        print('--------------------------------------------------')
      }
      
      gamma <- 1.82e-4 ## thermal expansion of mercury at 0 deg. C
      ## assume that record is not temperature corrected if no information is available
      if (is.null(df$Tcorr)){
        if (verbose) print('Assuming that pressure is not reduced to 0\u00b0 C')
        df$Tcorr <- 0
      }
      ## only correct temperature if 
      if (df$Tcorr[1] == 1){
        if (!is.null(df$Comments[1])){
          Tref <- gsub('.*educed.*to ', '', df$Comments[1])
          TT <- strsplit(Tref, '\u00b0')[[1]]
          TTcelsius <- temperature2SI(as.numeric(TT[1]), TT[2]) - 273.15
          print(paste('Change temperature reduction from', Tref, 'to 0\u00b0C'))
          if (is.null(df[['QFE']])) df$QFE <- (1 - gamma*TTcelsius)*df$P.orig
        } else {
          if (is.null(df[['QFE']])) df$QFE <- df$P.orig
        }
        ## set QFE flag to 1 (reduced for temperature in original record)
        df$QFE.flag <- 1
      } else {
        Tnames <- intersect(c('TP.orig', 'TA.orig', 'TP.20CR'), names(df))
        
        print(paste('Using', paste(Tnames, collapse=' and '), 'for temperature reduction'))
        Temperature <- df[[Tnames[1]]]
        ## specify QFE flag
        QFE.flagval <- c(TP.orig=2, TA.orig=3, TP.20CR=4)
        df$QFE.flag <- QFE.flagval[Tnames[1]]
        if (length(Tnames) > 1 & any(is.na(df[[Tnames[1]]]))){
          ## replace missing values in TP.orig with TA.orig or TP.20CR
          ## loop through remaining temperature names and fill the gaps
          for (tn in Tnames[2:length(Tnames)]){
            df$QFE.flag[is.na(Temperature)] <- QFE.flagval[tn]
            Temperature[is.na(Temperature)] <- df[[tn]][is.na(Temperature)]          
          }
        }
        ## reduce pressure at station to 0 deg. C
        if (is.null(df[['QFE']])) df$QFE <- (1 - gamma * Temperature) * df$P.orig
      }    
    } else {
      df$QFE.flag <- 1
    }
    
  }
  
  ## convert station pressure to MSLP
  if (verbose){
    print('')
    print('Convert QFE to QFF')
    print('--------------------------------------------------')
  }
  
  ## check if QFF is already available
  if (!is.null(df[['QFF']])){
    print('Pressure AMSL already available')
    df$QFF.flag <- 1
  } else {
    Tnames <- intersect(c('TA.orig', 'TA.20CR'), names(df))
    
    print(paste('Using', paste(Tnames, collapse=' and '), 'for reduction to sea level'))
    Temperature <- df[[Tnames[1]]]
    ## specify QFE flag
    df$QFF.flag <- c(TA.orig=2, TA.20CR=3)[Tnames[1]]
    if (length(Tnames) == 2 & any(is.na(df[[Tnames[1]]]))){
      ## replace missing values in TP.orig with 20CR
      Temperature[is.na(df[[Tnames[1]]])] <- df[[Tnames[2]]][is.na(df[[Tnames[1]]])]
      df$QFF.flag[is.na(df[[Tnames[1]]])] <- 3 ## to indicate that we're using 20CR
    }
    ## reduce pressure to sea level
    df[['QFF']] <- QFE2QFF(QFE=df[['QFE']], 
                           elevation=df[['Elevation']], 
                           latitude=df[['Latitude']], 
                           temperature=Temperature)
    
  }
  
  
  ## add in missing variables (dummies)
  if (is.null(df[['mmHg']])) df$mmHg <- NA
  if (is.null(df[['P.orig']])) df$P.orig <- NA
  if (is.null(df[['QFE']])) df$QFE <- NA    
  
  ## fix flags for missing values
  df$QFE.flag[is.na(df[['QFE']])] <- 0
  df$QFF.flag[is.na(df[['QFF']])] <- 0
  
  ## reorder data frame to be more easily readable
  dfnames <- c('Station', 'Station.name', 'Longitude', 'Latitude', 'Location.missing', 'Elevation', 'Elevation.missing', 'UTC.date', 'Local.date', 'Year', 'Month', 'Day', 'Local.time', 'Time', 'Time.missing', P.names, 'P.units', 'mmHg', 'Tcorr', 'P.orig', 'QFE', 'QFE.flag', 'QFF', 'QFF.flag', names(df)[grep('^TP', names(df))], names(df)[grep('^TA', names(df))], 'T.units')
  dfnames <- intersect(dfnames, names(df))
  df <- df[,c(dfnames, setdiff(names(df), dfnames))]
  
  ## add in remaining information from repository
  notinclude <- c('Standard.Name', 'Station', 'ISPD', 'Latitude', 'Longitude', 'Location.Flag', 'Elevation', 'Elevation.Flag', 'Unit', 'Tcorr', 'hasQFF', 'hasTP', 'T.units')
  for (nn in setdiff(names(inventory), notinclude)) if (is.null(df[[nn]])) df[[nn]] <- inventory[[nn]]
  
  return(df)
}