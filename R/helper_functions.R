
#' Load the parameter values into a environment
#'
#' @param ptable data.table containing the following columns: parameter, value, and units.
#' @importFrom assertthat assert_that
#' @family helper functions
internal_load_params <- function(ptable, state){

  # Make sure that the parameter table has the correct names.
  req_names <- c("parameter", "description", "units", "value")
  assert_that(has_name(ptable, req_names))

  # Create the empty environment
  env <- new.env(parent = emptyenv())

  # Extract the parameter tables.
  p        <- ptable$value
  names(p) <- ptable$parameter

  mapply(function(pname, pvalue){
    assign(pname, pvalue, envir = env)
  }, pname = names(p), pvalue = p)

  mapply(function(sname, svalue){
    assign(sname, svalue, envir = env)
  }, sname = names(state), svalue = state)

  return(env)

}


load_env <- function(env, here){

  ll <- ls(env)
  for (i in ll){
    assign(i, env[[i]], envir = here)
  }

}

test <- function(ptable, state, flux){

  env <- internal_load_params(ptable, state)


}


test(MEMC::default_params, carbon_fluxes)



test(MEMC::default_params)




get_env_elements(x)

test_function <- function(){

  env <- internal_load_params(MEMC::default_params, MEMC::default_inital)
  carbon_pools(1, env, carbon_fluxes)



}


load_env <- function(env){

  ll <- ls(env)

  for (i in ll){

    assign(i, env[[i]])

  }


}


parent.frame(env)

#' Set up a model configuration
#'
#' @param params data.table containing the following columns: parameter, value, and units.
#' @param state a vector of the initial state values, must be named
#' @param carbon_pools_func a function defining the carbon  pools
#' @param carbon_fluxes_func a function defining the carbon fluxes between pools
#' @param name default set to NULL otherwise is a string of the model configuration name.
#' @importFrom assertthat assert_that
#' @export
#' @family helper functions
configure_model <- function(params, state, carbon_pools_func, carbon_fluxes_func, name = NULL){

  # Make sure that the pools and fluxes are being read in as functions and that
  # the have not been used in place of one another.
  assert_that(is.function(carbon_pools_func))
  req_args  <-c('t', 'state', 'parms', 'flux_function')
  pool_args <- as.vector(names(formals(carbon_pools_func)))
  missing   <- req_args[!req_args %in% pool_args]
  assert_that(length(missing) ==  0,  msg = paste('carbon_pool_func missing required arguments: ', paste(missing, collapse = ', ')))

  assert_that(is.function(carbon_fluxes_func))
  req_args  <- c("state", "parms")
  flux_args <- as.vector(names(formals(carbon_fluxes_func)))
  missing   <- req_args[!req_args %in% flux_args]
  assert_that(length(missing) == 0, msg = paste('carbon_flux_func missing required arguments: ', paste(missing, collapse = ', ')))
  assert_that(all(!grepl(pattern = 'flux', flux_args)), msg = 'check carbon_fluxes_func, make sure the pool function is not being used in stead.')

  # Check to make sure that there are no missing parameter values, an error will be thrown here if the params table is missing an entry.
  rslt <- carbon_pools_func(t = 1, state = state, parms = params, flux_function = carbon_fluxes_func)
  assert_that(all(is.numeric(unlist(rslt))))

  model_object <- list("name" = name,
                       "params" = params,
                       "state" = state,
                       "carbon_pools_func" = carbon_pools_func,
                       "carbon_fluxes_func" = carbon_fluxes_func)

  return(model_object)

}



#' Solve a MEMC configuration
#'
#' @param mod model object created by \code{make_model}
#' @param time a vector of the time setps
#' @param params default set to NULL, will then use the parameter table read in with the "mod" object.
#' @param ... additional arguments that can be read into \code{deSolve::ode}
#' @return a long formatted data.table of the simulation results
#' @importFrom assertthat assert_that
#' @importFrom deSolve ode
#' @export
#' @family helper functions
solve_model <- function(mod, time, params = NULL, ...){

  assert_that(is.list(mod))
  req_names <-  c("params", "state", "carbon_pools_func", "carbon_fluxes_func", "name")
  assert_that(has_name(x = mod, which = req_names), msg = "mod must a model object created by configure_model")
  assert_that((is.null(params) | is.data.frame(params)))
  assert_that(all(is.numeric(time)))

  # If no parameter table is passed is use the params read in with the model object.
  if(is.null(params)){

    rslt  <- ode(y = mod$state,      # initial state estimates
                          times = time,  # times to solve over
                          parms = mod$params,  # parameter table
                          func = mod$carbon_pools_func, # the pool representation we are interested in
                          flux_function =  mod$carbon_fluxes_func,
                          ...) # extra ode arguments

  } else {

    # Use the parameter table read in, check that there are no missing params.
    assert_that(has_name(x = params, which = names(mod$params)))
    assert_that(all(mod$params$parameter %in% params$parameter), msg = "problem with the params table being read in")

    rslt  <- deSolve::ode(y = mod$state,      # initial state estimates
                          times = time,       # times to solve over
                          parms = params,     # parameter table
                          func = mod$carbon_pools_func, # the pool representation we are interested in
                          flux_function = mod$carbon_fluxes_func,
                          ...) # extra ode arguments

  }

  # Now format the results into a nice data frame instead of a wide  matrix.
  out <- data.table::melt(data.table::as.data.table(rslt),
                          measure.vars = names(mod$state),
                          variable.name = "variable",
                          value.name = 'value')
  out$units <- 'mg C/g soil'

  if (is.null(mod$name)){
    name <- "(unnamed)"
  } else {
    name <- mod$name
  }
  out$name <- name

  return(out)


}


#' Update the parameter table with new values
#'
#' @param new_params a named vector of parameter values to update the param table.
#' @param param_table data.table containing the following columns: parameter, value, and units, this is the basic parameter table that will be updated with the new values.
#' @return updated data.table containing the new parameter values
#' @importFrom assertthat assert_that
#' @export
#' @family helper functions
#' @family parameters
update_params <- function(new_params, param_table){

  req_paramtable_names <- c("parameter", "description", "units", "value")
  assert_that(has_name(x = param_table, which = req_paramtable_names),
              msg = paste0("param_table is missing a required column: ", paste0(req_paramtable_names, collapse = ", ")))


  assert_that(!is.null(names(new_params)), msg = "new params must be named")
  pnames <- names(new_params)
  assert_that(all(pnames %in% param_table$parameter), msg = "new_params must refer to a parameter already existing in param_table")
  assert_that(is.numeric(new_params))

  # Update the param_table with the new values! To avoid there being a dependency on
  # the order in which the new_params are read into the function use a for loop to itterate
  # over all the parameters to be updated.
  for (p in pnames){
    index <- which(param_table$parameter == p)
    param_table$value[index] <- new_params[[p]]
  }

  return(param_table)

}


#' Modify a flux functions
#'
#' @param params a table of parameters
#' @param state a vector of the initial state values, must be named
#' @param flux_func a function that will return a list of functions that modify how carbon moves between the carbon pools.
#' @param replace_with a list containing the new elements of the functions
#' @return a list of carbon flux functions
#' @importFrom assertthat assert_that
#' @export
#' @family helper functions
modify_fluxes_func <- function(params, state, flux_func, replace_with){

  fluxes <- flux_func(state = state, parms = params)
  fluxes_copy <- fluxes

  # Check the inputs
  assert_that(is.function(flux_func))
  assert_that(all(unlist(lapply(fluxes, is.function))))
  assert_that(is.character(names(fluxes)))

  assert_that(is.list(replace_with), msg = 'replace_with should be a list of functions')
  assert_that(is.character(names(replace_with)))
  assert_that(all(names(replace_with) %in% names(fluxes)), msg = 'all elements of replace_with must be within flux_func')
  assert_that(all(unlist(lapply(replace_with, is.function))), msg = 'all elements of replace_with must be within flux_func')

  # Format the parameters into a vector.
  p        <- params$value
  names(p) <- params$parameter

  with(as.list(c(state, p)),{
    E.c <- 10
  # Subset the flux function so that it only contains default fluxes that should not be replaced.
  fluxes <- fluxes[!names(fluxes) %in% names(replace_with)]

  out <- function(state, parms){append(fluxes, replace_with)}

  # Make sure that the length of the modified fluxes matches the copy of the original fluxes.
  x <- out(state, parms)
  assert_that(length(x) == length(fluxes_copy))

  return(out)
  })
}







