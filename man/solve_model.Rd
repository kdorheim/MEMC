% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_functions.R
\name{solve_model}
\alias{solve_model}
\title{Solve a MEMC configuration}
\usage{
solve_model(mod, time, params = NULL, ...)
}
\arguments{
\item{mod}{model object created by \code{make_model}}

\item{time}{a vector of the time setps}

\item{params}{default set to NULL, will then use the paramter table read in with the "mod" object.}

\item{...}{additional arguments that can be read into \code{deSolve::ode}}
}
\value{
a long formatted data.table of the simulation results
}
\description{
Solve a MEMC configuration
}
\seealso{
Other helper functions: 
\code{\link{configure_model}()},
\code{\link{update_params}()}
}
\concept{helper functions}
