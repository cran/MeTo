% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ws.R
\name{ws}
\alias{ws}
\title{Sunset hour angle}
\usage{
ws(x, lat.rad)
}
\arguments{
\item{x}{date-time object or day of the year}

\item{lat.rad}{latitude [rad]}
}
\value{
Sunset hour angle
}
\description{
Sunset hour angle for given latitude and solar declination (\code{\link{SolarDec}}).
}
\details{
\describe{\item{x:}{
must be provided as number (1-366) or as a common date-time object (e.g, POSIXct, POSIXlt or Date objects).
All formats for which is.timepoint from the lubridate package returns TRUE can be used}}
}
\note{
eq. 25 of reference
}
\examples{
ws(x = 246, lat.rad = -0.35)
}
\references{
Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998). Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56. FAO, Rome, 300(9).
}
