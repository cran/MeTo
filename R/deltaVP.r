#' @title Slope of the saturation vapor pressure curve
#' @description Slope of the saturation vapor pressure curve [kPa/degreeC].
#' @param Tmean Mean Temperature [degreeC] (mean daily, mean hourly, etc. air temperature)
#' @param Tmax maximum temperature during 24-hour period [degreeC] (if Tmean is missing)
#' @param Tmin minimum temperature during 24-hour period [degreeC] (if Tmean is missing)
#' @details valid for daily, hourly and shorter periods
#' @return slope of the saturation vapor pressure curve [kPa/degreeC]
#' @examples
#' deltaVP(Tmax = 34.8, Tmin = 25.6)
#' deltaVP(Tmean = 30.2)
#' @note eq. 13 of reference
#' @export
#' @references Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998). Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56. FAO, Rome, 300(9).

deltaVP <- function(Tmean = NULL, Tmax = NULL, Tmin = NULL){

  if (is.null(Tmean)) { Tmean <- (Tmax + Tmin)/2}

  (4098 * (0.6108 * exp(17.27 * Tmean/(Tmean + 237.3))))/(Tmean + 237.3)^2
}


