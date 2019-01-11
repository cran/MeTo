#' @title Inverse relative distance Earth-Sun
#' @description Calculate inverse relative distance Earth-Sun for daily and shorther periods.
#' @param x date-time object or Day of the year
#' @details
#' \describe{\item{x:}{
#' must be provided as number (1-366) or as a common date-time object (e.g, POSIXct, POSIXlt or Date objects).
#' All formats for which is.timepoint from the lubridate package returns TRUE can be used}}
#' @note eq. 23 of reference
#' @references Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998). Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56. FAO, Rome, 300(9).
dr <- function(x){
  doy <- prep.date(x)
  1 + (0.033*cos((2*pi*doy)/365))
}
