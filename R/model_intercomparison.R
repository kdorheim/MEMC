#' Solve different MEND configurations
#'
#'
#' @param parameters data.table containing the following columns: parameter, value, and units. Default values are stored as pacakge see \code{MEND_params}.
#' @param time a vector of the time setps.
#' @param inital_state a numeric vector of the different MEND carbon pool states. Default values are probvide as package data see \code{MEND_initalState}
#' @return a data frame of output from the different MEND configurations, \code{MEND} and \code{MEND_RM}
#' @family 2013 MEND model functions
#' @family model
#' @export
model_intercomparison <- function(parameters, time, inital_state){

  results <- data.table::as.data.table(rbind(cbind(model = 'MEND', MEND(parameters, time, inital_state)),
                                             cbind(model = 'MEND_RM', MEND_RM(parameters, time, inital_state))))
  return(results)

}


