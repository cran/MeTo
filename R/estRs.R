#' @title Estimate solar radiation (Rs)
#' @description Rs is calculated from relative sunshine duration and extraterrestrial radiation with the Angstrom formula.
#' @param x date-time object or Day of the year
#' @param n actual duration of sunshine [hour]
#' @param lat.rad latitude [rad] (either lat.rad or lat.deg). Latitude is positive for the northern hemisphere and negative for the southern hemisphere
#' @param lat.deg latitude [degree] (either lat.rad or lat.deg). Latitude is positive for the northern hemisphere and negative for the southern hemisphere
#' @param tl length of calculation period [hour] (1 for hourly, 0.5 for a 30-minute or 24 for daily period).
#' Only needed if length of x is date-time object with length of 1.
#' @param control list for control parameters and empirical factors defined in \code{\link{controlDefaults}} and \code{\link{constDefaults}} (see Details)
#' @details \describe{\item{control:}{}
#' \item{}{as: regression constant, expressing fraction of extraterrestrial radiation reaching earth on overcast days (n = 0) (default = 0.25) }
#' \item{}{bs: as + bs fraction of extraterrestrial radiation reaching earth on clear days (n = N) (default = 0.5)}}
#' \describe{\item{x:}{
#' must be provided as.numeric (1-366) or as a common date-time object (e.g, POSIXct, POSIXlt or Date objects).
#' All formats for which is.timepoint from the lubridate package returns TRUE can be used}}
#' @return solar or shortwave radiation (Rs) [MJ/(m2 day)]
#' @examples
#' estRs(x = 135, n = 7, lat.rad = NULL, lat.deg = -22.9)
#' @note eq. 35 of reference
#' @importFrom utils modifyList
#' @references Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998). Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56. FAO, Rome, 300(9).
#' @export
estRs <- function(x, n, lat.rad = NULL, lat.deg = NULL, tl, control = list(as = 0.25, bs = 0.5)) {

  control <- modifyList(controlDefaults, control)

  if (is.null(lat.rad) & is.null(lat.deg)) {stop('no latitude')}
  if (is.null(lat.rad)) {lat.rad<-(pi/180)*lat.deg}

  N <- dlh(x = x, lat.rad = lat.rad)
  (control$as + control$bs * (n/N))*
    Ra(x = x, lat.rad = lat.rad, tl = tl)
}

