#' @title Extraterrestrial radiation
#' @description Extraterrestrial radiation [MJ/(m2 time)] in dependence to time, latitude and longitude.
#' @param x date-time object or day of the year (must be date-time object if calculation period is smaller 1 day)
#' @param lat.rad latitude [rad]. Use either lat.rad or lat.deg. Latitude is positive for the northern hemisphere and negative for the southern hemisphere
#' @param lat.deg latitude [degree]. Use either lat.deg or lat.rad. Latitude is positive for the northern hemisphere and negative for the southern hemisphere
#' @param long.deg longitude of the measurement site (degrees east of Greenwich) (only needed for periods shorter 1 day)
#' @param control list for control parameters and empirical factors defined in
#'  \code{\link{controlDefaults}} and \code{\link{constDefaults}} (see Details)
#' @param tl length of calculation period [hour] (1 for hourly period, 0.5 for a 30-minute period or 24 for daily period).
#' Only needed if x is date-time object with length of 1.
#' @details
#' \describe{\item{x:}{
#' must be provided as.numeric (1-366) or as a common date-time object (e.g, POSIXct, POSIXlt or Date objects).
#' All formats for which is.timepoint from the lubridate package returns TRUE can be used}}
#' \describe{\item{control:}{
#' Lz (for periods < 1 day):\cr
#' longitude of the centre of the local time zone (degrees west of Greenwich)\cr
#' - 0 for Greenwich\cr
#' - 345 for Germany\cr
#' - 330 for Cairo (Egypt)\cr
#' - 255 for Bangkok (Thailand)\cr
#' - 75, 90, 105 and 120 for Eastern, Central, Rocky Mountain and Pacific time zones (United States)}}
#' @note eq. 21 (period = 1 day) and eq. 28 (hourly or shorter) of the reference
#' @examples
#' # --------------------------------------------
#' #  Daily period
#' # --------------------------------------------
#'
#' Ra(x = 105, lat.deg = 13.73)
#'
#' # --------------------------------------------
#' #  Hourly period
#' # --------------------------------------------
#'
#' Ra(x = as.POSIXct(c('2018-10-01 14:30', '2018-10-01 15:30')),
#'    lat.deg = 16.21, long.deg = 343.75, control = list(Lz = 15))
#'
#' Ra(x = as.POSIXct('2018-10-01 14:30'), tl = 1,
#'    lat.deg = 16.21, long.deg = 343.75, control = list(Lz = 15))
#'
#' @importFrom lubridate is.timepoint
#' @importFrom utils modifyList
#' @references Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998). Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56. FAO, Rome, 300(9).
#' @export
#'
Ra <- function(x, lat.rad = NULL, lat.deg = NULL, long.deg, tl, control = list(Lz = 345)) {

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
  if (is.null(lat.rad) & is.null(lat.deg)) stop('no latitude')
  if (is.null(lat.rad)) lat.rad <- (pi/180)*lat.deg

  doy <- prep.date(x)

  # Daily Values ----------------------------------------------------------------------------------------------
  if (all(tl > 1)){
    obj <- ((24*60)/pi)*constDefaults$GSC*dr(doy)* (ws(doy, lat.rad)*
                                                     sin(lat.rad)* sin(SolarDec(x)) + cos(lat.rad)* cos(SolarDec(x)) *sin(ws(x, lat.rad)))
  }
  # hourly or shorter ---------------------------------------------------------------------------------------------------
  if (all(tl <= 1)) {
    # -----------------------------------------------------------------------
    if(is.null(long.deg)) {
      stop('For hourly or shorter periods longitude (long.deg) must be provided!')
    }
    # -----------------------------------------------------------------------

    Lm <- 360 - long.deg
    ws <- ws(doy, lat.rad = lat.rad)
    # solar time angle at midpoint of the period (eq.31)
    w.mid <- w(x = x, long.deg = long.deg, control = control)
    # Solar time angle at beginning/ending of period Allen 1999 Eq (2-19) --------------------------------------------------------
    w1 <- w.mid - ((pi*tl)/24)
    # Solar time angle at beginning/ending of period Allen 1999 Eq (2-20) -------------------------------------------------------
    w2 <- w.mid + ((pi*tl)/24)
    # Criteria for w1 and w2 -------------------------------------------------------------------------------------------------
    w1[w1< -ws] <- -ws[w1< -ws]
    w2[w2< -ws] <- -ws[w2< -ws]
    w1[w1> ws] <- ws[w1> ws]
    w2[w2> ws] <- ws[w2> ws]

    obj <- (((12*60)/pi)* constDefaults$GSC *1.0001 * ((w2 - w1)*sin(lat.rad)*sin(-0.0753) +
                                                                 (cos(lat.rad)*cos(-0.0753)*(sin(w2) - sin(w1)))))

    obj <- (((12*60)/pi)* constDefaults$GSC *dr(doy) * ((w2 - w1)*sin(lat.rad)*sin(SolarDec(doy)) +
                                                         (cos(lat.rad)*cos(SolarDec(doy))*(sin(w2) - sin(w1)))))
  }
  if (any(tl <= 1) & any(tl > 1)) {
    stop('Calculation period (tl) smaller and larger than 1 hour! Difference betwenn time steps must be consistent in x')
  }
  obj
}
