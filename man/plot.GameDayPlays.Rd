% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/GameDayPlays.R
\name{plot.GameDayPlays}
\alias{plot.GameDayPlays}
\title{plot.GameDayPlays}
\usage{
\method{plot}{GameDayPlays}(data, batterName = NULL, pitcherName = NULL,
  event = NULL, pch = 1)
}
\arguments{
\item{data}{A GameDayPlays set with fields 'our.x' and 'our.y'}

\item{batterName}{A character string containing the last name of a batter}

\item{pitcherName}{A character string containing the last name of a pitcher}

\item{pch}{A numeric}
}
\value{
an xyplot()
}
\description{
Visualize Balls in Play
}
\details{
Plots the balls in play from GameDay data. This function will plot (x,y)-coordinates
with a generic baseball field plotted in the background. Other lattice options can be passed
to xyplot().
}
\examples{
ds = getData()
plot(ds)
}

