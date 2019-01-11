#' @title Adjust wind speed data
#' @description Adjust wind speed data to 2 meter height.
#' @param u measured wind speed at uz above ground surface [m/s]
#' @param uz height of windspeed measurement above ground surface [m]
#' @examples
#' adj_u2(3.2, uz = 10)
#' @note eq. 47 of reference
#' @export
#' @references Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998). Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56. FAO, Rome, 300(9).

adj_u2 <- function(u, uz) {
(u * 4.87/(log((67.8*uz - 5.42), base = exp(1))))
}

