#' @title Estimate atmospheric pressure (P)
#' @description Values for atmospheric pressure as a function of altitude.
#' @param elev elevation [m]
#' @param control list for control parameters and empirical factors defined in
#'  \code{\link{controlDefaults}} and \code{\link{constDefaults}} (see Details)
#' @return atmospheric pressure [kPa]
#' @details \describe{\item{Control variables:}{}
#' \item{}{Tko: reference temperature [degreeC] at elevation z0. Often assumed to be 20 degreeC}
#' \item{}{z0: elevation at reference level [m]}
#' \item{}{a1: constant lapse rate moist air (0.0065 [K/m])}
#' \item{}{g: gravitational acceleration (9.807 [m/s2])}
#' \item{}{R: specific gas constant (287 [J/(kg K)])}}
#' @examples
#' estP(elev = 25, control = list(Tko = 20))
#' @export
#' @note eq. 3-2 of Reference
#' @importFrom utils modifyList
#' @references Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998). Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56. FAO, Rome, 300(9).
estP <- function(elev, control = list(Tko = 20)) {

  control <- modifyList(controlDefaults, control)
  control <- modifyList(constDefaults, control)

  control$Po*(((control$Tko + 273.16)- control$a1*(elev - control$z0))/
                (control$Tko + 273.16))^(control$g/(control$a1*control$R))
  }

