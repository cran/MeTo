#' @title Net solar or net shortwave radiation (Rns)
#' @description Net shortwave radiation is the balance between incoming and reflected solar radiation.
#' @param Rs incoming solar radiation [MJ/(m2 time)]
#' @param control list for control parameters and empirical factors (see Details)
#' @details \describe{\item{control:}{
#' albedo [-]: 0.23 for hypothetical grass and alfalfa reference crops used in the FAO-56 PM equations}}
#' @return Rns net solar or shortwave radiation [MJ/(m2 time)]
#' @details valid for daily and shorter periods
#' @note eq. 38 of reference
#' @examples
#' Rns(Rs = 22.1)
#' Rns(Rs = 22.1, control = list(albedo = 0.20))
#' @export
#' @references Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998). Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56. FAO, Rome, 300(9).
Rns <- function(Rs, control = list(albedo = 0.23)) {

  control <- modifyList(controlDefaults, control)

  (1 - control$albedo)*Rs
}
