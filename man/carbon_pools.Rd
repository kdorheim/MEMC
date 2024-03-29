% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_componets.R
\name{carbon_pools}
\alias{carbon_pools}
\title{Define the carbon pools}
\usage{
carbon_pools(t, state, parms, flux_function = carbon_fluxes)
}
\arguments{
\item{t}{is for time}

\item{state}{A numeric vector of the different carbon pool states.}

\item{parms}{A data frame of the parameters.}

\item{flux_function}{a function that will return a list of functions that modify how carbon moves between
the carbon pools.}
}
\value{
A list of the state variables
}
\description{
\code{carbon_pools} Defines the system of equations that
describe the state of the carbon pools, it follows a general 5-pool scheme that is commonly
adopted in microbial-explicit models following \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
}
\references{
\href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
}
\concept{carbon pool functions}
