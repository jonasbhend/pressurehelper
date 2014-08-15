#' read_inventory
#' 
#' Reads metadata in common repository for later use
#' 
#' @param file path to file containing inventory information
#' 
#' @keywords util
#' @export
read_inventory <- function(file='~/Unibe/pressure/Working_inventory_pressure_1815-17.xls'){
  ## load the input file
  wb <- loadWorkbook(file)
  ## set empty cells to missing
  setMissingValue(wb, value=missvals)
  
  ## read the header
  inventory <- readWorksheet(wb, sheet=1)
  
  ## change the names for ease of access
  names(inventory)[grep('Location.Flag', names(inventory))] <- 'Location.Flag'
  names(inventory)[grep('Elevation.Flag', names(inventory))] <- 'Elevation.Flag'
  names(inventory)[names(inventory) == 'Elevation..m.'] <- 'Elevation'
  names(inventory)[grep('Times.of.observation', names(inventory))] <- 'Times.of.observation'
  names(inventory)[grep('Period.available', names(inventory))] <- 'Period.available'
  names(inventory)[grep('Temperature.at.the.barometer', names(inventory))] <- 'hasTP'
  names(inventory)[names(inventory) == 'Reduced.for.temperature'] <- 'Tcorr'  
  names(inventory)[names(inventory) == 'Reduced.to.sea.level'] <- 'hasQFF'  
  
  ## change some of the variables to boolean
  inventory$hasQFF <- as.logical(inventory$hasQFF)
  
  ## add in standard names as rownames for ease of selection
  rownames(inventory) <- inventory$Standard.Name
  
  return(inventory)
}