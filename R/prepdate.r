#' @title Prepare date
#' @description Checks if x is date-time object or day of the year (doy) and returns doy.
#' @param x date-time object or Day of the year
#' @return Day of the year
#' @importFrom lubridate is.timepoint
prep.date <- function(x) {

  if(all(is.timepoint(x)) == TRUE){
  doy <- as.numeric(strftime(x, format = "%j"))
  }

if(is.numeric(x)){
  if(any(x <= 0) | any(x > 366)){
    stop(c("Day of the year is not between 1-366"))}
  doy <- x
}
doy
}
