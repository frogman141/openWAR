% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/getWAR.R
\name{getWAR}
\alias{getWAR}
\title{getWAR}
\usage{
getWAR(data, dataRepl = NULL, nteams = 30, verbose = TRUE, ...)
}
\arguments{
\item{data}{An object of class \code{'openWARPlays'}}

\item{dataRepl}{An object of class \code{'openWARPlays'} that will be used to calculate the replacement level.}

\item{nteams}{The nteams argument to be passed to getReplacementPlayers}
}
\description{
Tabulates WAR
}
\details{
Compute each player's WAR, given their RAA values
}
\examples{
ds = getData()
out = makeWAR(ds)
raa = getRAA(out$openWAR)
war = getWAR(raa)
}

