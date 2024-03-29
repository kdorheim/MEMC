% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_functions.R
\name{update_params}
\alias{update_params}
\title{Update a paramter table with new values}
\usage{
update_params(new_params, param_table)
}
\arguments{
\item{new_params}{a named vector of parameter values to update the param table.}

\item{param_table}{data.table containing the following columns: parameter, value, and units, this is the basic parameter table that will be updated with the new values.}
}
\value{
updated data.table containing the new parameter values
}
\description{
Update a paramter table with new values
}
\seealso{
Other helper functions: 
\code{\link{configure_model}()},
\code{\link{solve_model}()}

Other parameters: 
\code{\link{default_params}}
}
\concept{helper functions}
\concept{parameters}
