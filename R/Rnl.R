#' @title Net longwave radiation (Rnl)
#' @description Net longwave radiation (Rnl).
#' @param x date-time object or day of the year (must be date-time object if calculation period is shorter than a day)
#' @param Tmax maximum temperature [degreeC] during 24-hour period (for daily values)
#' @param Tmin minimum temperature [degreeC] during 24-hour period (for daily values)
#' @param Tmean mean temperature [degreeC] during the time period (for periods shorter than a day)
#' @param Rhmax daily maximum of air humidity [percent] (for daily values)
#' @param Rhmin daily minimum of air humidity [percent] (for daily values)
#' @param Rhmean Mean air humidity [percent] (for periods shorter than a day or if Rhmax and Rhmin are missing)
#' @param actVP Actual vapor pressure [kPa]. If Rhmax, Rhmin and Rhmean are NULL
#' @param Rs incoming solar radiation [MJ/(m2 time)]
#' @param lat.rad latitude [rad]. Use either lat.rad or lat.deg. Latitude is positive for the northern hemisphere and negative for the southern hemisphere
#' @param lat.deg latitude [degree]. Use either lat.deg or lat.rad. Latitude is positive for the northern hemisphere and negative for the southern hemisphere
#' @param elev station elevation above sea level [m]
#' @param tl length of calculation period [hour] (1 for hourly period, 0.5 for a 30-minute period or 24 for daily period).
#' Only needed if length of x is date-time object with length of 1.
#' @param long.deg see \code{\link{Rso}}
#' @param control list for control parameters and empirical factors (see Details and \code{\link{controlDefaults}})
#' @importFrom utils tail
#' @details
#' \describe{\item{x:}{
#' must be provided as.numeric (1-366) or as a common date-time object (e.g, POSIXct, POSIXlt or Date objects).
#' All formats for which is.timepoint from the lubridate package returns TRUE can be used}}
#' \describe{\item{control:}{
#' Lz: \cr
#' longitude of the centre of the local time zone (degrees west of Greenwich) \cr
#' - 0 for Greenwich \cr
#' - 345 for Germany \cr
#' - 330 for Cairo (Egypt) \cr
#' - 255 for Bangkok (Thailand) \cr
#' - 75, 90, 105 and 120 for Eastern, Central, Rocky Mountain and Pacific time zones (United States)\cr
#' Lz is only needed if calculation period is shorter 1 day.\cr\cr
#' est.ratio.Rs.Rso: \cr
#' Rs/Rso is used to represent cloud cover. For hourly or shorter periods during the nighttime,
#' the ratio Rs/Rso is set equal to the Rs/Rso calculated for a time period occurring 2-3 hours before sunset.
#' If single values during nighttime are calculated Rs/Rso ration 2-3 hours before sunset can not be calculated
#' and an approximation is needed. Following Allen (1999) one can assume Rs/Rso = 0.4 to 0.6 during nighttime periods
#' in humid and subhumid climates and Rs/Rso = 0.7 to 0.8 in arid and semiarid climates. A value of Rs/Rso = 0.3 presumes
#' total cloud cover.}}
#' @note eq. 39 of reference
#' @importFrom lubridate date
#' @importFrom utils tail modifyList
#' @return Rnl net outgoing longwave radiation [MJ(/m2 time)]
#' @references Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998). Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56. FAO, Rome, 300(9).
Rnl <- function(x, Tmax = NULL, Tmin = NULL, Rhmax = NULL, Rhmin = NULL, Rs = NULL,
                lat.rad = NULL, lat.deg = NULL, long.deg = NULL, elev, actVP = NULL, tl,
                Tmean = NULL, Rhmean = NULL, control = list()) {


  control <- modifyList(controlDefaults, control)
  # check input
  if (is.null(lat.rad) & is.null(lat.deg)) stop('no latitude')
  if (is.null(lat.rad)) lat.rad <- (pi/180)*lat.deg
  if(is.null(Rhmax) & is.null(Rhmin) & is.null(Rhmean) & is.null(actVP)) {
    stop('Please provide Rhmax and Rhmin, Rhmean or actVP')
  }
  # check x

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


  doy <- prep.date(x)

  if (all(tl > 1)){
    # Cloud function to account for impact of cloud temperature on Rnl
    ratio.Rs.Rso <- 1.35*Rs/Rso(x = x, lat.rad = lat.rad, elev = elev) - 0.35
    if (any(ratio.Rs.Rso > 1, na.rm = TRUE)){ warning('ratio Rs/Rso > 1') }
        ratio.Rs.Rso <- ifelse(ratio.Rs.Rso > 1, 1, ratio.Rs.Rso)
        # vapor pressure
        if(!is.null(actVP)) {
        Vpres <- actVP
        } else{
        Vpres <- VP(Tmax = Tmax, Tmin = Tmin, Rhmax = Rhmax, Rhmin = Rhmin, Rhmean = Rhmean, interval = 'day')
        }
        Obj <- constDefaults$sigma*(((Tmax + 273.16)^4 + (Tmin+273.16)^4)/2) *(0.34-(0.14*
             sqrt(Vpres)))*ratio.Rs.Rso

    }
  if (all(tl <= 1)) {
    # -----------------------------------------------------------------------
    if(is.null(long.deg)) {
      stop('For hourly or shorter periods longitude (long.deg) must be provided!')
    }
    # -----------------------------------------------------------------------
    # vapor pressure
    if(!is.null(actVP)) {
      Vpres <- actVP
    }else{
      Vpres <- VP(Rhmean = Rhmean, Tmean = Tmean, interval = 'hour')
    }

    ratio.Rs.Rso <- Rs/Rso(x = x, lat.rad = lat.rad, elev = elev, long.deg = long.deg, tl = tl, control = control)

    w.mid  <- w(x = x, long.deg = long.deg, control = control)
    ws <- ws(x = x, lat.rad = lat.rad)
    # 2-3 hours before sunset
    two.hbss <- (ws - 0.79) <= w.mid & w.mid <= (ws - 0.52)
    #
    days <- unique(date(x))
    if (is.na(control$est.ratio.Rs.Rso) & length(ratio.Rs.Rso) > 1) {
      ratio.rsw.new <- ratio.Rs.Rso
      # Rs/Rso during nighttime

      for(i in 1:length(which(two.hbss == T))){
        ratio.rsw.new[(which(two.hbss == T)[i]):
                        (tail(which(date(x) == days[i]), n = 1))] <- ratio.Rs.Rso[(which(two.hbss == T)[i])]
      }

      for(i in 1:(length(which(two.hbss == T)) - 1)) {
        temp <- which(days[i + 1] == date(x))
        temp <- temp[1:(length(temp)/2)]
        ratio.rsw.new[temp][which(is.na(ratio.rsw.new[temp]) | ratio.rsw.new[temp] < ratio.rsw.new[tail(which(days[i] == date(x)), n = 1)])] <- ratio.rsw.new[(tail(which(date(x) == days[i]), n = 1))]
      }
      ratio.Rs.Rso <- ifelse(ratio.rsw.new>1,1,ratio.rsw.new)
    }
    ratio.Rs.Rso <- ifelse(ratio.Rs.Rso == Inf | is.na(ratio.Rs.Rso), control$est.ratio.Rs.Rso, ratio.Rs.Rso)
    if (any(is.na(ratio.Rs.Rso))) warning('ratio.Rs.Rso is NA. Provide est.ratio.Rs.Rso (e.g. control = list(est.ratio.Rs.Rso = 0.8))!')
    Obj <- 2.043*10^-10*((Tmean+273.16)^4)*(0.34-(0.14*sqrt(Vpres)))*(1.35*ratio.Rs.Rso - 0.35)
  }

  if (any(tl <= 1) & any(tl > 1)) {
    stop('Calculation period (tl) smaller and larger than 1 hour! Difference betwenn time steps must be consistent in x')
  }

  Obj
}
