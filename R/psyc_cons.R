#' @title psychrometric constant
#' @description The psychrometric constant [kPa/degreeC]
#' @param P atmospheric pressure [kPa]
#' @param elev elevation [m]
#' @param control list for control parameters and empirical factors
#' defined in \code{\link{controlDefaults}} and \code{\link{constDefaults}} (see Details)
#' @details
#' \describe{\item{control:}{
#' Tko: reference temperature [degreeC] at elevation z0. Only needed if atmospheric pressure is missing.
#' Often assumed to be 20 degreeC \cr\cr
#' elev: station elevation above sea level [m]. Needed if P = NULL for \code{\link{estP}}\cr\cr
#' lambda: latent heat of vaporization [MJ/kg] \cr\cr
#' eps: ratio molecular weight of water vapor/dry air = 0.622 \cr\cr
#' cp: specific heat of moist air = 1.013 x 10-3 [MJ/(kg degreeC)]}}
#' @return psychrometric constant [kPa/degreeC]
#' @examples psyc_cons(elev = 2, P = 101.3)
#' @note eq. 8 of reference
#' @references Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998). Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56. FAO, Rome, 300(9).
#' @export
psyc_cons <- function(elev, P = NULL, control = list()){

control <- modifyList(c(controlDefaults, constDefaults), control)

if(!is.null(P)){
    obj <- (control$cp*P)/(control$lambda*control$eps)
}
if(is.null(P)){
  obj <- (control$cp*estP(elev, control))/(control$lambda* control$eps)
}
obj
}

