% Generated by roxygen2 (4.0.1): do not edit by hand
\name{dms2dd}
\alias{dms2dd}
\title{dms2dd}
\usage{
dms2dd(x)
}
\arguments{
\item{x}{character with degrees and minutes}
}
\description{
Function to convert degree minute seconds to decimal degrees
}
\examples{
dms <- c("4\\u00b015'", "-5\\u00b034'48\\"")
dms2dd(dms)
# should be 4.25 and -5.58 respectively
}
\keyword{util}

