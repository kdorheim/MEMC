#' Assign the paramter values
#'
#' Define parameters based on the a parameter data table that is read in.
#'
#' @param dt data.table containing the following columns: parameter, value, and units.
#' @param req default set to NULL, if a string vector is read will check to make sure that
#' all of the required parameters are defined in the data table.
#' @return Nothing, but has defined the elements of the parameter column as objects in
#' the environment.
#' @importFrom assertthat assert_that has_name
assign_parameters <- function(dt, req = NULL){

  assert_that(data.table::is.data.table(dt))
  has_name(x = dt, which = c('parameter', 'value', 'units'))
  assert_that(is.character(dt[['parameter']]))
  assert_that(is.numeric(dt[['value']]))
  if(!is.null(req)){
    missing <- !req %in% dt[['parameter']]
    assert_that(all(!missing), msg = cat('dt missing parameters: ', paste0(req[missing], collapse = ', ')))
  }

  mapply(assign, x = dt$parameter, value = dt$value, inherits = TRUE)

  # Return nothing
  return(NULL)
}

