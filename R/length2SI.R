#' length2SI
#' 
#' Convert length units from non-standard units to SI.
#' This allows to convert imperial units to m, but also using
#' various bases (some of which are predefined, some of which
#' have to be supplied by the user).
#' 
#' @param x vector, or matrix of lengths
#' @param units length units to be used (see details)
#' @param base conversion factors for columns of matrix if non-standard
#' units are used, character if in base list
#' @param verbose logical, should information on conversion factors be displayed?
#' 
#' @references
#' http://www.thefullwiki.org/Conversion_of_units
#' 
#' @examples
#' length2SI(rbind(c(20,3,2), c(0,6,7)))
#' length2SI(1, 'finger')
#' 
#' @keywords util
#' @export
length2SI <- function(x, units='imperial', base=NULL, verbose=TRUE){
  
  baselist <- c(mile=1609.344,
                yard=0.9144,
                foot=0.9144 / 3,
                inch=0.9144 / 36,
                line=0.9144 / 36 / 12,
                ell=0.9144 * 1.25, 
                finger=0.022225,
                tum=0.02969)
  
  if (is.matrix(x) | is.data.frame(x)){
    if (is.null(base) & units == 'imperial') base <- c('yard', 'foot', 'inch')
    if (is.null(base)) stop('Base for conversion not supplied')
    if (verbose) print(paste('Convert with base', paste(base, collapse=', ')))
    if (is.character(base)){
      if (all(base %in% names(baselist))){
        base <- baselist[base]
      } else {
        stop('Units not in baselist')
      }
    }
    if (ncol(x) == length(base)){
      x <- as.matrix(x)
      ## replace missing values with zero (but not for base metric)
      if (ncol(x) > 1) {
        xtmp <- x[,-1]
        xtmp[is.na(xtmp)] <- 0
        x[,2:ncol(x)] <- xtmp
      }
      xout <- as.vector(x %*% base)
    } else {
      stop('Length of base for conversion does not match input')
    }
  } else {
    
    if (!is.null(units) & units %in% names(baselist)){
      xout <- x * baselist[units]
    } else{
      stop('Units not in baselist')
    }
    names(xout) <- NULL
    
    if (verbose) print(paste('Convert', units, 'to m'))
  }

  return(xout)
}