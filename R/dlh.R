#' @title Dayligth hours (N)
#' @description Dayligth hours in dependence to latitude.
#' @param x date-time object or day of the year
#' @param lat.rad latitude [rad] (either lat.rad or lat.deg). Latitude is positive for the northern hemisphere and negative for the southern hemisphere
#' @param lat.deg latitude [degree] (either lat.rad or lat.deg). Latitude is positive for the northern hemisphere and negative for the southern hemisphere
#' @details \describe{\item{x:}{
#' must be provided as number (1-366) or as a common date-time object (e.g, POSIXct, POSIXlt or Date objects).
#' All formats for which is.timepoint from the lubridate package returns TRUE can be used}}
#' @examples
#' dlh(x = 105, lat.deg = 13.73)
#' dlh(x = 105, lat.rad = 0.283)
#' dlh(x = as.Date('2018-04-15'), lat.deg = 13.73)
#' @note eq. 34 of reference
#' @references Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998). Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56. FAO, Rome, 300(9).
#' @export
dlh <- function(x, lat.rad = NULL, lat.deg = NULL) {

  if (is.null(lat.rad) & is.null(lat.deg)) stop('no latitude')
  if (is.null(lat.rad)) lat.rad <- (pi/180)*lat.deg

  (24/pi) *ws(x, lat.rad = lat.rad)
}
