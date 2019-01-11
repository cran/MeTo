#' @title FAO-56 Penman-Monteith grass reference evapotranspiration
#' @description FAO Penman-Monteith equations to compute grass reference evapotranspiration from weather data for daily, monthly, hourly or shorter periods.
#' @param x date-time object or day of the year (must be date-time object if calculation period is shorter than a day)
#' @param Tmax maximum temperature [degreeC] during 24-hour period (for daily values)
#' @param Tmin minimum temperature [degreeC] during 24-hour period  (for daily values)
#' @param Rhmax maximum of air humidity [percent] during 24-hour period (for daily values)
#' @param Rhmin minimum of air humidity [percent] during 24-hour period(for daily values)
#' @param Tmean mean air temperature [degreeC]. For periods shorter 1 day.
#' @param Rhmean mean relative air humidity [percent]. For periods shorter 1 day or if Rhmax and Rhmin are missing.
#' @param Rs solar radiation [MJ/(m2 time)]
#' @param n actual hours of sunshine. Used to calculate Rs if Rs messurements are not available (see \code{\link{estRs}}).
#' @param elev station elevation above sea level [m]
#' @param lat.rad latitude [rad]. Use either lat.rad or lat.deg. Latitude is positive for the northern hemisphere and negative for the southern hemisphere
#' @param lat.deg latitude [degree]. Use either lat.deg or lat.rad. Latitude is positive for the northern hemisphere and negative for the southern hemisphere
#' @param long.deg longitude [degree] east of Greenwich (for periods < 1 day)
#' @param u wind speed [m/s] at 2 meter height. If measurement height is not 2 m, define height with control <- list(uz = 2)
#' @param P air pressure [kPa]. Estimated with \code{\link{estP}} if missing.
#' @param control list for control parameters and empirical factors (see details, \code{\link{controlDefaults}} and \code{\link{constDefaults}})
#' @param tl length of calculation period [hour] (1 for hourly, 0.5 for 30-minute and 24 for daily period).
#' Only needed if x is date-time object with length of 1.
#' @param actVP Actual vapor pressure [kPa]. If Rhmax, Rhmin and Rhmean are NULL
#' @param G soil heat flux (Assumed to be 0 for daily calculations) (for calculation periods shorter than a day estimated with \code{\link{estG}} if missing)
#' @details \describe{\item{x:}{
#' must be provided as.numeric (1-366) or as a common date-time object (e.g, POSIXct, POSIXlt or Date objects).
#' All formats for which is.timepoint from the lubridate package returns TRUE can be used}}
#' \describe{\item{control: (see also \code{\link{controlDefaults}} and \code{\link{constDefaults}})\cr}{
#' Lz:\cr
#' longitude of the centre of the local time zone (degrees west of Greenwich)\cr
#' - 0 for Greenwich\cr
#' - 345 for Germany\cr
#' - 330 for Cairo (Egypt)\cr
#' - 255 for Bangkok (Thailand)\cr
#' - 75, 90, 105 and 120 for Eastern, Central, Rocky Mountain and Pacific time zones (United States)\cr
#' Lz is only needed if calculation period is shorter than 1 day.\cr \cr
#' uz: height of wind measurements (m) \cr \cr
#' albedo: default 0.23 for the hypothetical grass and alfalfa reference crops used in
#' the FAO-56 PM equations \cr \cr
#' as: regression constant, expressing fraction of extraterrestrial radiation reaching earth on overcast days (n = 0) (default = 0.25) \cr \cr
#' bs: as + bs fraction of extraterrestrial radiation reaching earth on clear days (n = N) (default = 0.5)\cr \cr
#' Tko: reference temperature [degreeC] at elevation z0. Only needed if atmospheric pressure is missing. Often assumed to be 20 degreeC. \cr \cr
#' z0: elevation at reference level (fefault = 0 [m])\cr \cr
#' est.ratio.Rs.Rso: \cr Rs/Rso is used to represent cloud cover. For hourly or shorter periods during the nighttime,
#' the ratio Rs/Rso is set equal to the Rs/Rso calculated for a time period occurring 2-3 hours before sunset.
#' If single values during nighttime are calculated Rs/Rso ration 2-3 hours before sunset can not be calculated
#' and an approximation is needed. Following Allen (1999) one can assume Rs/Rso = 0.4 to 0.6 during nighttime periods
#' in humid and subhumid climates and Rs/Rso = 0.7 to 0.8 in arid and semiarid climates. A value of Rs/Rso = 0.3 presumes
#' total cloud cover. }}
#' @examples
#' # --------------------------------------------
#' #  Daily Evapotranspiration
#' # --------------------------------------------
#'
#' ETref(x = 187, Rs = 22.07, elev = 100, lat.deg = 50.8, Tmax = 21.5, Tmin = 12.3,
#'       Rhmax = 84, Rhmin = 63,
#'       u = 2.78, control = list(uz = 10), P = 100.1)
#'
#' # Calculation with sunshine hour (n) instead of
#' # global radiation (Rs) (Rs ist estimated from n with estRs):
#'
#' ETref(x = 187, n = 9.25, elev = 100, lat.deg = 50.8, Tmax = 21.5, Tmin = 12.3,
#'      Rhmax = 84, Rhmin = 63,
#'      u = 2.78, control = list(uz = 10), P = 100.1)
#'
#' # --------------------------------------------
#' #  Hourly Evapotranspiration
#' # --------------------------------------------
#'
#' ETref(x = as.POSIXct(c('2018-10-01 14:30', '2018-10-01 15:30')),
#'       Tmean = c(38, 37.8), Rhmean = c(52, 52.3), u = c(3.3, 3.2), Rs = c(2.450, 2.5), elev = 8,
#'       lat.deg = 16.22, long.deg = 343.75, G = c(0.175, 0.178) , P = c(101.21, 101.21) ,
#'       control = list(Lz = 15))
#'
#' # If only one time step is calculated tl must be provided (1 for hourly, 0.5 for 30 minute periods):
#'
#' ETref(x = as.POSIXct('2018-10-01 14:30'), tl = 1,
#'       Tmean = 38, Rhmean = 52, u = 3.3, Rs = 2.450, elev = 8,
#'       lat.deg = 16.22, long.deg = 343.75, G = 0.1749218, P = 101.2056,
#'       control = list(Lz = 15))
#'
#' # Calculation with missing soil heat flux (G) and atmospheric pressure (P) (G is estimated with estG
#' # and P with estP)
#'
#' ETref(x = as.POSIXct('2018-10-01 14:30'), tl = 1,
#'       Tmean = 38, Rhmean = 52, u = 3.3, Rs = 2.450, elev = 8,
#'       lat.deg = 16.22, long.deg = 343.75,
#'       control = list(Lz = 15))
#' # --------------------------------------------
#' @author Ullrich Dettmann
#' @references Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998). Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56. FAO, Rome, 300(9).
#' @note eq. 6 from reference for daily and eq. 53 for hourly or shorter periods
#' @return grass reference evapotranspiration [mm]
#' @export
#' @importFrom utils modifyList
ETref <- function(x, Tmax = NULL, Tmin = NULL, Rhmax = NULL, Rhmin = NULL, Tmean = NULL, Rhmean = NULL,
                  u = NULL, Rs = NULL, n = NULL, P = NULL, elev, lat.rad = NULL, lat.deg = NULL, long.deg = NULL,
                  tl, G = NULL, actVP = NULL, control = list()) {


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
  # Prepare and check input
  # ----------------------------------------------------------------------------------------
  doy <- prep.date(x)
  if (is.null(lat.rad) & is.null(lat.deg)) stop('no latitude')
  if (is.null(lat.rad)) lat.rad <- (pi/180)*lat.deg
  # air pressure
  if (is.null(P)) {
    P <- estP(elev = elev, control)
  }
  # Wind
  if(control$uz == 2) {
    u2 <- u
  }
  if(control$uz != 2) {
    u2 <- adj_u2(u = u, uz = control$uz)
  }
  # Calc ET0 ------------------------------------------------------------------------------------
  if (all(tl > 1)){
    if(!is.null(actVP)) {
      Vpres <- actVP
    }else{
      Vpres <- VP(Tmax, Tmin, Rhmax = Rhmax, Rhmin = Rhmin, Rhmean = Rhmean, interval = 'day')
    }
    # only for daily calculations
    if (is.null(Rs) & is.null(n)) {
      stop('For daily or longer periods solar radiation (Rs) or sunshine duration (n) must be provided!')
    }
    if (is.null(Rs)) { Rs <- estRs(x, n, lat.rad = lat.rad, control = control)}
    if (is.null(G)) { G <- 0 }
    Tmean <- ((Tmax + Tmin)/2)
    obj <- ((0.408* deltaVP(Tmax = Tmax, Tmin = Tmin)* #day and hour
               (Rn(x = doy, Tmax = Tmax, Tmin = Tmin, Rhmax = Rhmax, Rhmin = Rhmin, Rs = Rs, control = control,
                   lat.rad = lat.rad, elev = elev, Rhmean = Rhmean, actVP = actVP, Tmean = Tmean) - G))+
              (psyc_cons(elev = elev, P = P)*900/(Tmean + 273))*(u2)* (satVP(Tmax, Tmin, interval = 'day') -
                                                                      Vpres))/
      (deltaVP(Tmax = Tmax, Tmin = Tmin) + (psyc_cons(elev = elev, P)*(1+(0.34*u2))))
  }


  if (all(tl <= 1)) {
    # Check input -----------------------------------------------------------------------
    if(is.null(long.deg)) {
      stop('For hourly or shorter periods longitude (long.deg) must be provided!')
    }
    if(is.null(Rs)) {
      stop('For hourly or shorter periods solar radiation (Rs) must be provided!')
    }
    # if soil heat flux measurements are missing ----------------------------------------------
    if (is.null(G)) {G <- estG(x = x, Tmean = Tmean, Rhmean = Rhmean, Rs = Rs,
                               lat.rad = lat.rad, long.deg = long.deg, tl = tl, elev = elev,
                               control = control) }
    # -----------------------------------------------------------------------
    obj <- ((0.408* deltaVP(Tmean = Tmean)*
            (Rn(x = x, Rs = Rs, Tmean = Tmean, Rhmean = Rhmean, lat.rad = lat.rad,
                long.deg = long.deg, elev = elev, control = control, tl = tl) -
             G)) +
             (psyc_cons(elev, P, control = control)*(37/(Tmean + 273))* u2*
             (e0(Tmean) - VP(Tmean = Tmean, Rhmean = Rhmean, interval = 'hour'))
             ))/
           (deltaVP(Tmean = Tmean) + (psyc_cons(elev, P, control = control)*(1 + 0.34 * u2)))
  }
obj
}
