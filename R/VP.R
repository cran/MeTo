#' @title Actual vapor pressure
#' @description Calculate actual vapour pressure [kPa] either from psychrometric data or from relative humidity data.
#' @param Tmax maximum temperature during 24-hour period (for daily values)
#' @param Tmin minimum temperature during 24-hour period  (for daily values)
#' @param Rhmax maximum relative humidity [precent] (for daily values)
#' @param Rhmin minimum relative humidity [precent] (for daily values)
#' @param Rhmean Mean air humidity [percent] (for periods shorter 1 day or if Rhmax and Rhmin are missing)
#' @param Tmean Mean air temperature [degreeC] (for periods shorter 1 day)
#' @param Twet wet bulb temperature (for calculation with psychrometric data)
#' @param Tdry dry bulb tamperature (for calculation with psychrometric data)
#' @param apsy coefficient depending on the type of ventilation of the wet bulb [kPa/(degreeC)] (for calculation with psychrometric data)
#' @param P atmospheric pressure [kPa]
#' @param interval hour, day, week or month
#' @details
#' \describe{\item{x:}{
#' must be provided as.numeric (1-366) or as a common date-time object (e.g, POSIXct, POSIXlt or Date objects).
#' All formats for which is.timepoint from the lubridate package returns TRUE can be used}}
#' \describe{\item{interval:}{
#' - use hour for periods <= one hour \cr
#' - for day, week or month the same equations are used}}
#' @examples
#' VP(Tmax = 25, Tmin = 18, Rhmax = 82, Rhmin = 54)
#' VP(Tmax = 25, Tmin = 18, Rhmean = 68)
#' @note eq. 17 of reference (Determination of actual vapour pressure from relative maximum and minimum humidity)
#' @note eq. 15 of reference (Actual vapour pressure derived from psychrometric data) (see \code{\link{psyc_cons}})
#' @note eq. 19 of reference (used in the absence of RHmax and RHmin)
#' @note eq. 54 of reference (for periods shorter than a day)
#' @references Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998). Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56. FAO, Rome, 300(9).
#' @export
#' @seealso \code{\link{e0}}, \code{\link{satVP}}
VP <- function(Tmax = NULL, Tmin = NULL, Rhmax = NULL, Rhmin = NULL, interval = 'day',
                  Tmean = NULL, Rhmean = NULL, Twet = NULL, Tdry = NULL, apsy = NULL, P = NULL){

  interval <- tolower(interval)
  if(interval == 'day' | interval == 'week' | interval == 'month') {
    interval <- 'day'
  }

# daily period ----------------------------------------------------------------------------------
  if(interval == 'day') {
# VP from relative humidity data -------------------------------------------------------------------
  if (!is.null(Rhmax) & !is.null(Rhmin)){
    obj <- (e0(Tmin)*(Rhmax/100) + e0(Tmax)*(Rhmin/100))/2 #eq. 17
  }
# VP from psychrometric data --------------------------------------------------------------------------------------------------
  if (is.null(Rhmax) | is.null(Rhmin)){
    obj <- e0(Twet) - apsy*P*(Tdry - Twet)  #eq. 15
  }
# From Rhmean if Rhmax and Rhmin are missing ---------------------------------------------------------------------------------------------
  if ((is.null(Rhmax) | is.null(Rhmin)) & (is.null(Twet) | is.null(Twet))){
    Tmean <- (Tmax + Tmin)/2
    obj <- (Rhmean/100)*satVP(Tmax, Tmin)  #eq. 19
  }
  }
# period shorter 1 day----------------------------------------------------------------------------------
  if(interval == 'hour'){
    if(is.null(Tmean)) {Tmean <- (Tmax + Tmin)/2}
    if(is.null(Rhmean)) {Rhmean <- (Rhmax + Rhmin)/2}
    obj <- (Rhmean/100)*e0(Tmean)  #eq. 54
  }

  if(identical(obj, numeric(0))) {
  stop('Input is missing!', call. = TRUE)
  }
obj
}

