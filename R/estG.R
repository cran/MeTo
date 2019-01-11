#' @title Estimate soil heat flux (G)
#' @description Estimate soil heat flux (G) for periods shorter than a day.
#' @param x date-time object (see details)
#' @param Rs incoming solar radiation [MJ/(m2 time)]
#' @param Tmean Mean air temperature [degreeC]
#' @param Rhmean Mean air humidity [percent]
#' @param lat.rad latitude [rad]. Use either lat.rad or lat.deg. Latitude is positive for the northern hemisphere and negative for the southern hemisphere
#' @param lat.deg latitude [degree]. Use either lat.deg or lat.rad. Latitude is positive for the northern hemisphere and negative for the southern hemisphere
#' @param long.deg longitude of the measurement site (degrees east of Greenwich)
#' @param elev station elevation above sea level [m]
#' @param tl length of calculation period [hour] (1 for hourly period, 0.5 for a 30-minute period or 24 for daily period).
#' Only needed if x is date-time object with length of 1.
#' @param control list for control parameters and empirical factors defined in
#'  \code{\link{controlDefaults}} and \code{\link{constDefaults}} (see Details)
#' @details
#' during daylight periods G is estimated to be \code{\link{Rn}} x 0.1 During nighttime G = \code{\link{Rn}} x 0.5.
#' Day is defined for extraterrestrial radiation > 0.
#' \describe{\item{x:}{
#' must be provided as a common date-time object (e.g, POSIXct, POSIXlt or Date objects).
#' All formats for which is.timepoint from the lubridate package returns TRUE can be used}}
#' \describe{\item{control:}{}
#' \item{}{albedo: default 0.23 for the hypothetical grass and alfalfa reference crops used in
#' the FAO-56 PM equations}
#' \item{}{Lz:\cr
#' longitude of the centre of the local time zone (degrees west of Greenwich)\cr
#' - 0 for Greenwich\cr
#' - 345 for Germany\cr
#' - 330 for Cairo (Egypt)\cr
#' - 255 for Bangkok (Thailand)\cr
#' - 75, 90, 105 and 120 for Eastern, Central, Rocky Mountain and Pacific time zones (United States)\cr
#' Lz is only needed if calculation period is shorter 1 day.}
#' \item{}{est.ratio.Rs.Rso: Rs/Rso is used to represent cloud cover. For hourly periods during the nighttime,
#' the ratio Rs/Rso is set equal to the Rs/Rso calculated for a time period occurring 2-3 hours before sunset.
#' If single values during nighttime are calculated Rs/Rso ration 2-3 hours before sunset can not be calculated
#' and an approximation is needed. Following Allen (1999) one can assume Rs/Rso = 0.4 to 0.6 during nighttime periods
#' in humid and subhumid climates and Rs/Rso = 0.7 to 0.8 in arid and semiarid climates. A value of Rs/Rso = 0.3 presumes
#' total cloud cover.}}
#' @examples
#' estG(x = as.POSIXct(c('2018-10-01 14:30', '2018-10-01 15:00')), Tmean = 38, Rhmean = 52, Rs = 2.450,
#'      elev = 8, lat.deg = 16.21, long.deg = 343.75, control = list(Lz = 15))
#'
#' estG(x = as.POSIXct('2018-10-01 02:30'), Tmean = 28, Rhmean = 90, tl = 1, Rs = 0, elev = 8,
#'      lat.deg = 16.2, long.deg = 343.75, control = list(Lz = 15, est.ratio.Rs.Rso = 0.8))
#'
#' estG(x = as.POSIXct('2018-10-01 14:30'), Tmean = 38, Rhmean = 52, tl = 1, Rs = 2.450, elev = 8,
#'      lat.deg = 16.21, long.deg = 343.75, control = list(Lz = 15))
#'
#' @note eq. 45 and 46 of reference
#' @references Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998). Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56. FAO, Rome, 300(9).
#' @export
estG <- function(x, Rs, Tmean, Rhmean, lat.rad = NULL, lat.deg = NULL,
                 long.deg = NULL, elev = 1, tl, control = list(albedo = 0.23, Lz = 345, est.ratio.Rs.Rso = NA)) {

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

  control <- modifyList(controlDefaults, control)

  obj <- Ra(x = x, lat.rad = lat.rad, long.deg = long.deg, tl = tl, control = control)
  ifelse(obj > 0,
         0.1*Rn(x = x, Rs = Rs, elev = elev, lat.rad = lat.rad, long.deg = long.deg,
                Rhmean = Rhmean, Tmean = Tmean, tl = tl, control = control), # during daytime periods
         0.5*Rn(x = x, Rs = Rs, elev = elev, lat.rad = lat.rad, long.deg = long.deg,
                Rhmean = Rhmean, Tmean = Tmean, tl = tl, control = control)  # during nighttime periods
         )
  }
