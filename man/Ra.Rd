% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Ra.R
\name{Ra}
\alias{Ra}
\title{Extraterrestrial radiation}
\usage{
Ra(x, lat.rad = NULL, lat.deg = NULL, long.deg, tl, control = list(Lz = 345))
}
\arguments{
\item{x}{date-time object or day of the year (must be date-time object if calculation period is smaller 1 day)}

\item{lat.rad}{latitude [rad]. Use either lat.rad or lat.deg. Latitude is positive for the northern hemisphere and negative for the southern hemisphere}

\item{lat.deg}{latitude [degree]. Use either lat.deg or lat.rad. Latitude is positive for the northern hemisphere and negative for the southern hemisphere}

\item{long.deg}{longitude of the measurement site (degrees east of Greenwich) (only needed for periods shorter 1 day)}

\item{tl}{length of calculation period [hour] (1 for hourly period, 0.5 for a 30-minute period or 24 for daily period).
Only needed if x is date-time object with length of 1.}

\item{control}{list for control parameters and empirical factors defined in
\code{\link{controlDefaults}} and \code{\link{constDefaults}} (see Details)}
}
\description{
Extraterrestrial radiation [MJ/(m2 time)] in dependence to time, latitude and longitude.
}
\details{
\describe{\item{x:}{
must be provided as.numeric (1-366) or as a common date-time object (e.g, POSIXct, POSIXlt or Date objects).
All formats for which is.timepoint from the lubridate package returns TRUE can be used}}
\describe{\item{control:}{
Lz (for periods < 1 day):\cr
longitude of the centre of the local time zone (degrees west of Greenwich)\cr
- 0 for Greenwich\cr
- 345 for Germany\cr
- 330 for Cairo (Egypt)\cr
- 255 for Bangkok (Thailand)\cr
- 75, 90, 105 and 120 for Eastern, Central, Rocky Mountain and Pacific time zones (United States)}}
}
\note{
eq. 21 (period = 1 day) and eq. 28 (hourly or shorter) of the reference
}
\examples{
# --------------------------------------------
#  Daily period
# --------------------------------------------

Ra(x = 105, lat.deg = 13.73)

# --------------------------------------------
#  Hourly period
# --------------------------------------------

Ra(x = as.POSIXct(c('2018-10-01 14:30', '2018-10-01 15:30')),
   lat.deg = 16.21, long.deg = 343.75, control = list(Lz = 15))

Ra(x = as.POSIXct('2018-10-01 14:30'), tl = 1,
   lat.deg = 16.21, long.deg = 343.75, control = list(Lz = 15))

}
\references{
Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998). Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56. FAO, Rome, 300(9).
}
