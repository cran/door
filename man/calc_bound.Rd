% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/forest_plot.R
\name{calc_bound}
\alias{calc_bound}
\title{Calculate lower/upper bound of forest plot given upper, lower of data}
\usage{
calc_bound(lower, upper, mean)
}
\arguments{
\item{lower}{a numerical vector of lower limits of confidence intervals}

\item{upper}{a numerical vector of upper limits of confidene intervals}

\item{mean}{a numerical vector of DOOR probability estiamtes}
}
\value{
calculated upper and lower range of axis ticks of forest plot
}
\description{
Calculate lower/upper bound of forest plot given upper, lower of data
}
\keyword{internal}
