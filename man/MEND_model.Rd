% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{MEND_model}
\alias{MEND_model}
\title{MEMC model configurations for MEND Wang et al. 2013}
\format{
an object created form \code{configure_model}.
\describe{
\item{name}{MEND}
\item{params}{the default MEMC parameter table}
\item{state}{the default inital state values for MEMC}
\item{carbon_pools_func}{carbon pools based on Wang et al. 2013}
\item{carbon_fluxes_func}{carbon fluxes based on Wang et al. 2013}
}
}
\usage{
MEND_model
}
\description{
MEMC model configurations for MEND Wang et al. 2013
}
\examples{
MEND_model
solve_model(mod = MEND_model, time = 1:10)
}
\references{
\href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
}
\concept{MEMC configurations}
\keyword{datasets}
