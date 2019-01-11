#' @title Solar time angle
#' @description Solar time angle at midpoint of the period (for periods < 1 day).
#' @param x date-time object (e.g, POSIXct, POSIXlt or Date objects).
#' @param long.deg longitude of the measurement site (degrees east of Greenwich) (for periods < 1 day)
#' @param control list for control parameters and empirical factors (see Details)
#' @details \describe{\item{control:}{}
#' \item{}{Lz: \cr
#' longitude of the centre of the local time zone (degrees west of Greenwich) \cr
#' - 0 for Greenwich \cr
#' - 345 for Germany \cr
#' - 330 for Cairo (Egypt) \cr
#' - 255 for Bangkok (Thailand) \cr
#' - 75, 90, 105 and 120 for the Eastern, Central, Rocky Mountain and Pacific time zones (United States)}
#' }
#' @note eq. 31 of reference
#' @importFrom lubridate hour minute
#' @importFrom utils modifyList
#' @references Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998). Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56. FAO, Rome, 300(9).
w <- function (x, long.deg, control = list(Lz = 345)) {

  control <- modifyList(controlDefaults, control)

  Lm <- 360 - long.deg
  # ---------------------------------------------------------------------------------------------
  doy <- prep.date(x)
  b <- (2*pi*(doy - 81))/364 # eq. 33
  Sc <- 0.1645*sin(2*b)- 0.1255*cos(b) - 0.025*sin(b) # eq. 32
  # num.hour clock time at midpoint of the period [hour]. For example for a period between 14.00 and 15.00 hours, t = 14.5
  num.hour <- as.numeric(hour(x)) + (as.numeric(minute(x))*1/60)
  # ---------------------------------------------------------------------------------------------
  (pi/12)*((num.hour + 0.06667*(control$Lz - Lm) + Sc) - 12)
}
