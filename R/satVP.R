#' @title Saturation Vapor Pressure
#' @description Saturation vapor pressure [kPa].
#' @param Tmean mean air temperature [degreeC] for hourly or shorter periods
#' @param Tmin minimum temperature [degreeC] for daily, weekly, monthly periods
#' @param Tmax maximum temperature [degreeC] for daily, weekly, monthly periods
#' @param interval hour, day, week or month
#' @param print.warning TRUE or FALSE
#' @return Saturation Vapor Pressure [kPa]
#' @details \describe{\item{interval:}{hour (eq. 11 of reference)}
#' \item{interval:}{day, week or month (eq. 12 of reference or eq. 11 if only Tmean is provided)}
#' }
#' @seealso \code{\link{e0}}, \code{\link{VP}}
#' @examples
#' satVP(Tmax = 24.5, Tmin = 15, interval = 'day')
#' satVP(Tmax = 24.5, Tmin = 15, interval = 'week')
#' satVP(Tmax = 24.5, Tmin = 15, interval = 'month')
#'
#' satVP(Tmax = 24.5, Tmin = 15, interval = 'hour')
#' satVP(Tmean = 19.75, interval = 'hour')
#' @export
#' @references Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998). Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56. FAO, Rome, 300(9).

satVP <- function(Tmax = NULL, Tmin = NULL, Tmean = NULL, interval = 'day', print.warning = T){

  interval <- tolower(interval)
  if(interval == 'day' | interval == 'week' | interval == 'month') {
    interval <- 'day'
  }

  if(interval == 'day') {
    satVP <- (e0(Tmax) + e0(Tmin))/2
  }

  if (print.warning == T & (is.null(Tmax) & is.null(Tmin)) & interval != 'hour') {
    print('Warning! Using Tmean instead of Tmax and Tmin for time interval > hour underestimates the saturation vapour pressure and will result in underestimation of the reference crop evapotranspiration.')
  }

  if (is.null(Tmax) & is.null(Tmin) & !is.null(Tmean)) { interval <- 'hour' }

  if (interval == 'hour' & is.null(Tmean)) { Tmean <- (Tmax + Tmin)/2}
  if(!interval == 'day'){
    satVP <- e0(Tmean)
  }
  satVP
}

