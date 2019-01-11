#' @title Saturation Vapor Pressure
#' @description Mean saturation vapour pressure [kPa] for short time intervals less than a day. Calculated with air temperature.
#' @param Temp Temperature [degreeC]
#' @return saturation vapour pressure at air temperature [kPa/degreeC])
#' @note  eq. 11 of the reference
#' @note for day, week, decade or month, the mean saturation vapour pressure should be computed with \code{\link{satVP}}
#' @seealso \code{\link{satVP}}, \code{\link{VP}}
#' @references Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998). Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56. FAO, Rome, 300(9).
e0 <- function(Temp) {
    0.6108*exp((17.27*Temp)/(Temp+237.3))
}
