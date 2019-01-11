#' @title Sunset hour angle
#' @description Sunset hour angle for given latitude and solar declination (\code{\link{SolarDec}}).
#' @param x date-time object or day of the year
#' @param lat.rad latitude [rad]
#' @details \describe{\item{x:}{
#' must be provided as number (1-366) or as a common date-time object (e.g, POSIXct, POSIXlt or Date objects).
#' All formats for which is.timepoint from the lubridate package returns TRUE can be used}}
#' @return Sunset hour angle
#' @examples ws(x = 246, lat.rad = -0.35)
#' @note  eq. 25 of reference
#' @references Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998). Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56. FAO, Rome, 300(9).
#' @export

ws <- function(x, lat.rad) {
    doy <- prep.date(x)
    acos(-tan(lat.rad)*tan(SolarDec(doy)))
}
