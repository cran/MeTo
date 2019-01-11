#' @title Clear-sky solar radiation (Rso)
#' @description Clear-sky solar radiation for daily and shorter periods.
#' @param x date-time object or day of the year (must be date-time object if calculation period is shorter than a day)
#' @param lat.rad latitude [rad]. Use either lat.rad or lat.deg. Latitude is positive for the northern and negative for the southern hemisphere
#' @param lat.deg latitude [degree]. Use either lat.deg or lat.rad. Latitude is positive for the northern and negative for the southern hemisphere
#' @param long.deg longitude of the measurement site (degrees east of Greenwich) (only needed for periods < 1 day)
#' @param elev station elevation above sea level [m]
#' @param tl length of calculation period [hour] (1 for hourly period, 0.5 for a 30-minute period or 24 for daily period).
#' @param control list for control parameters and empirical factors defined in
#'  \code{\link{controlDefaults}} and \code{\link{constDefaults}} (see Details)
#' @details
#' \describe{\item{x:}{
#' must be provided as.numeric (1-366) or as a common date-time object (e.g, POSIXct, POSIXlt, and Date objects).
#' All formats for which is.timepoint from the lubridate package returns TRUE can be used}}
#' \describe{\item{control:}{}
#' \item{}{Lz:\cr
#' longitude of the centre of the local time zone (degrees west of Greenhich)\cr
#' - 0 for Greenwich\cr
#' - 345 for Germany\cr
#' - 330 for Cairo (Egypt)\cr
#' - 255 for Bangkok (Thailand)\cr
#' - 75, 90, 105 and 120 for Eastern, Central, Rocky Mountain and Pacific time zones (United States)\cr
#' Lz is only needed if calculation period is shorter 1 day.}
#' \item{}{for day, hour and shorter periods}}
#' @examples
#' # --------------------------------------------
#' #  Daily period
#' # --------------------------------------------
#'
#' Rso(x = 135, elev = 1, lat.deg = -22.9)
#'
#' # --------------------------------------------
#' #  Hourly period
#' # --------------------------------------------
#'
#' Rso(x = as.POSIXct('2018-10-01 12:30'), tl = 1, elev = 8, lat.deg = 16.2,
#'     long.deg = 343.75, control = list(Lz = 15))
#'
#' @importFrom utils modifyList
#' @references eq. 37; Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998). Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56. FAO, Rome, 300(9).
#' @export
Rso <- function(x, lat.rad = NULL, lat.deg = NULL, long.deg = NULL, elev, tl, control = list(Lz = 345)){

  control <- modifyList(controlDefaults, control)
  # identify length of calculation period (tl) -----------------------------------------------------------------------
  if(all(is.numeric(x))){
    tl <- 24
  }
  if(all(is.timepoint(x))) {
    # calulate interval
    if(length(x) > 1) {
      tl <- as.numeric(difftime(x[2:length(x)], x[1:(length(x)-1)] , units = 'hour'))
      tl <- unique(tl)
      if(length(tl) > 1) stop('Difference betwenn time steps must be consistent in x!')
    }
    if(length(x) == 1) {
      stopifnot(exists('tl'))
    }
  }
  # -----------------------------------------------------------------------
  if(all(tl <= 1) & is.null(long.deg)) {
    stop('For hourly or shorter periods longitude (long.deg) must be provided!')
  }
  # -----------------------------------------------------------------------
  if (is.null(lat.rad) & is.null(lat.deg)) stop('no latitude')
  if (is.null(lat.rad)) lat.rad <- (pi/180)*lat.deg

  (0.75 + (0.00002)*elev)* Ra(x, lat.rad = lat.rad, long.deg = long.deg, tl = tl, control = control)
}


