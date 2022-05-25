#' @title Net radiation (Rn)
#' @description Difference between the incoming net shortwave radiation (\code{\link{Rns}}) and the outgoing net longwave radiation (\code{\link{Rnl}}).
#' @param x date-time object or day of the year (must be date-time object if calculation period is shorter than a day)
#' @param Tmax maximum temperature [degreeC] during 24-hour period (for daily values)
#' @param Tmin minimum temperature [degreeC] during 24-hour period (for daily values)
#' @param Rhmax daily maximum of air humidity [percent] (for daily values)
#' @param Rhmin daily minimum of air humidity [percent] (for daily values)
#' @param Rs incoming solar radiation [MJ/(m2 time)]
#' @param n Actual hours of sunshine. Used to calculate Rs if missing.
#' @param elev station elevation above sea level [m]
#' @param lat.rad latitude [rad]. Use either lat.rad or lat.deg. Latitude is positive for the northern hemisphere and negative for the southern hemisphere
#' @param lat.deg latitude [degree]. Use either lat.deg or lat.rad. Latitude is positive for the northern hemisphere and negative for the southern hemisphere
#' @param long.deg longitude of the measurement site (degrees east of Greenwich) (for periods < 1 day)
#' @param Rhmean Mean air humidity [percent] for periods < day or if Rhmax and Rhmin are missing
#' @param actVP Actual vapor pressure [kPa]. If Rhmax, Rhmin and Rhmean are NULL
#' @param Tmean Mean air temperature [degree C] for periods < day
#' @param tl length of calculation period [hour] (1 for hourly period, 0.5 for a 30-minute period or 24 for daily period).
#' Only needed if x is date-time object with length of 1.
#' @param control list for control parameters and empirical factors (see Details)
#' @details for daily and hourly calculations
#' \describe{\item{x:}{
#' must be provided as.numeric (1-366) or as a common date-time object (e.g, POSIXct, POSIXlt or Date objects).
#' All formats for which is.timepoint from the lubridate package returns TRUE can be used}}
#' \describe{\item{control:}{
#' albedo: default 0.23 for the hypothetical grass and alfalfa reference crops used in
#' the FAO-56 PM equations \cr \cr
#' as: regression constant, expressing fraction of extraterrestrial radiation reaching earth on overcast days (n = 0) (default = 0.25) \cr \cr
#' bs: as + bs fraction of extraterrestrial radiation reaching earth on clear days (n = N) (default = 0.5)}}
#' @note eq. 40 of reference
#' @seealso \code{\link{Rns}}, \code{\link{Rnl}}
#' @return net radiation
#' @examples
#' # --------------------------------------------
#' #  Daily period
#' # --------------------------------------------
#'
#' Rn(x = 105, n = 8.5, elev = 2, actVP = 2.85, Tmax = 34.8,
#'    Tmin = 25.6, lat.deg = 13.73)
#'
#' Rn(x = 135, elev = 1, Rs = 14.5, Tmax = 25.1, Tmin = 19.1,
#'    lat.deg = -22.9, actVP = 2.1)
#'
#' # --------------------------------------------
#' #  Hourly period
#' # --------------------------------------------
#'
#' Rn(x = as.POSIXct(c('2018-10-01 14:30', '2018-10-01 15:30')), Tmean = c(38, 37.8),
#'    Rhmean = c(52, 52.2), Rs = c(2.450, 2.1), elev = 8, lat.deg = 16.2,
#'    long.deg = 343.75, control = list(Lz = 15))
#'
#' Rn(x = as.POSIXct('2018-10-01 14:30'), Tmean = 38, Rhmean = 52, tl = 1, Rs = 2.450,
#'    elev = 8, lat.deg = 16.2, long.deg = 343.75, control = list(Lz = 15))
#'
#' @references Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998). Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56. FAO, Rome, 300(9).
#' @export
Rn <- function(x, Tmax = NULL, Tmin = NULL, Rhmax = NULL, Rhmin = NULL,
               Rs = NULL, n = NULL, elev, lat.rad = NULL, lat.deg = NULL, long.deg = NULL,
               Rhmean = NULL, actVP = NULL, Tmean = NULL, tl, control = list()){

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
  if(all(tl <= 1) & is.null(long.deg)) {
    stop('For hourly or shorter periods longitude (long.deg) must be provided!')
  }
  # -----------------------------------------------------------------------

  # -----------------------------------------------------------------------
  if (is.null(lat.rad) & is.null(lat.deg)) stop('no latitude')
  if (is.null(lat.rad)) lat.rad <- (pi/180)*lat.deg
  # calculate solar radiation if missing -----------------------------------------------------------------------
  if (is.null(Rs)) Rs <- estRs(x, n, tl = tl, lat.rad = lat.rad, control = control)

  Rns(Rs = Rs, control) -
    Rnl(x = x, Tmax = Tmax, Tmin = Tmin, Rhmax = Rhmax,
        Rhmin = Rhmin, Rs = Rs, lat.rad = lat.rad, long.deg = long.deg,
        elev = elev, actVP = actVP, Tmean = Tmean, Rhmean = Rhmean, tl = tl, control = control)

}

