#' temperature2SI
#' 
#' Convert temperature from non-standard units to SI. By default,
#' this function converts degrees Celsius to Kelvin.
#' 
#' @param x temperature values
#' @param units temperature units
#' @param verbose logicl, should info about conversion factors be displayed
#' 
#' @references
#' http://www.thefullwiki.org/Conversion_of_units
#' 
#' @examples
#' x <- c(0, 32, 100)
#' temperature2SI(x, 'F')
#' 
#' temperature2SI(26.85)
#' 
#' @keywords util
#' @export
temperature2SI <- function(x, units='C', verbose=TRUE){
  # simplify units
  units <- gsub('\\.', '', gsub('degrees ', '', gsub('degree ', '', gsub('deg. ', '', gsub('°', '', units)))))
  if (units %in% c('C', 'Celsius')){
    if (verbose) print('Convert Celsius to Kelvin')
    xout <- x + 273.15
  } else if (units %in% c('F', 'Fahrenheit')){
    if (verbose) print('Convert Fahrenheit to Kelvin')
    xout <- (x + 459.67) * 5 / 9
  } else if (units %in% c('R', 'Re', 'Ré', 'Reaumur', 'Réaumur')){
    if (verbose) print('Convert Réaumur to Kelvin')
    xout <- x * 5 / 4 + 273.15
  } else if (units %in% c('D', 'De', 'Delisle')){
    if (verbose) print('Convert Delisle to Kelvin')
    xout <- 373.15 - x * 2/3
  } else if (units %in% c('N', 'Newton')){
    if (verbose) print('Convert Newton to Kelvin')
    xout <- x * 100 / 33 + 273.15
  } else if (units %in% c('Ro', 'Rø', 'Roemer', 'Rømer')){
    if (verbose) print('Convert Rømer to Kelvin')
    xout <- (x - 7.5) * 40/21 + 273.15
  } else if (units %in% c('Ra', 'Rankine')){
    if (verbose) print('Convert Rankine to Kelvin')
    xout <- x * 5 / 9
  }
    
  return(xout)
}