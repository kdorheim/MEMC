#' Define the carbon pools
#'
#' \code{carbon_pools} Defines the system of equations that
#' describe the state of the carbon pools, it follows a general 5-pool scheme that is commonly
#' adopted in microbial-explicit models following \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
#'
#' @param t is for time
#' @param state A numeric vector of the different carbon pool states.
#' @param parms A data frame of the parameters.
#' @param flux_function a function that will return a list of functions that modify how carbon moves between
#' the carbon pools.
#' @return A list of the state variables
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
#' @importFrom assertthat assert_that has_name
#' @family carbon pool functions
#' @export
carbon_pools <- function(t, env, flux_function = carbon_fluxes){

  # # Check the inputs
  # required_states <- c("P", "M", "Q", "B", "D", "EP", "EM", "IC", "Tot")
  # missing_states  <- required_states[!required_states %in% names(state)]
  # assert_that(length(missing_states) == 0, msg = paste0('missing states: ', paste(missing_states, collapse = ',  ')))
  # assert_that(all(required_states  == names(state)), msg = paste0('state pools must be in the following order: ', paste(required_states, collapse = ',  ')))
  # assert_that(is.data.frame(parms))
  # assert_that(has_name(x = parms, which = c("parameter", "description", "units", "value")))
  # assert_that(is.function(flux_function))
  # assert_that(is.numeric(t), msg = "t must be numeric")
  #
  # req <- c('I.p', 'g.d', 'f.d', 'I.p', 'I.d')
  # missing <- setdiff(req, parms$parameter)
  # assert_that(length(missing) == 0, msg = paste('parms missing values for: ', paste0(missing, collapse = ', ')))
  #
#
#   # Format the parameters into a vector.
#   p        <- parms$value
#   names(p) <- parms$parameter

  with(as.list(env),{

    # Define the fluxes and check to make sure they meet the requirements to be used
    # by the MEND carbon pool structure.
    fluxes <- flux_function(env)

    expected_fluxes <- rep('F', length.out = length(1:8))
    expected_fluxes <- c(paste0(expected_fluxes, 1:8), 'F9.ep', 'F9.em', 'F10.ep', 'F10.em')
    assertthat::assert_that(assertthat::has_name(x = fluxes, which = expected_fluxes))
    assertthat::assert_that(all(unlist(lapply(fluxes, is.function))), msg = 'fluxes input must be a list of functions')

    # Define the system of differential equations to describes the
    # changes in the carbon pool states.
    # -----------------------------------------------------------
    # P = particulate organic carbon
    dP <- I.p + (1 - g.d) * fluxes$F8() - fluxes$F2()
    # M = mineral-associated organic carbon (MOC)
    dM <- (1 - f.d) * fluxes$F2() - fluxes$F3()
    # Q = active layer of MOC
    dQ <- fluxes$F6() - fluxes$F7()
    # B = microbial biomass carbon
    dB <- fluxes$F1() - (fluxes$F4() + fluxes$F5()) - fluxes$F8() - (fluxes$F9.ep() + fluxes$F9.em())
    # D = dissolved organic carbon
    dD <- I.d + f.d * fluxes$F2() + g.d * fluxes$F8() + fluxes$F3() + (fluxes$F10.em() + fluxes$F10.ep()) - fluxes$F1() - (fluxes$F6() - fluxes$F7())
    # EP = carbon stored as extra-cellular enzymes
    dEP <- fluxes$F9.em() - fluxes$F10.ep()
    # EM = carbon stored as extra-cellular enzymes
    dEM <- fluxes$F9.em() - fluxes$F10.em()
    # IC = inorganic carbon (CO2)
    dIC <- fluxes$F4() + fluxes$F5()
    # Tot = the total carbon pool
    dTot <- I.p + I.d - (fluxes$F4() + fluxes$F5())

    # Return outputs
    list(c(dP, dM, dQ, dB, dD, dEP, dEM, dIC, dTot))
  })
}


#' Define the carbon fluxes.
#'
#' \code{carbon_fluxes} Defines a system of equations that
#' describe fluxes between the general 5-pool scheme based on \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
#' By returning a list of named functions. The default configuration is set up to follow the fluxes defined in \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}.
#' More advanced users may choose to change the flux.
#'
#' @param state A numeric vector of the different carbon pool states.
#' @param parms A data frame of the parameters.
#' @return A list of functions that calculate the fluxes between carbon pools.
#' @family carbon flux functions
#' @references \href{https://doi.org/10.1890/12-0681.1}{Wang et al. 2013}
#' @importFrom assertthat assert_that has_name
#' @export
carbon_fluxes <- function(env){

  # #function(state, parms){
  #
  # # Check inputs
  # assert_that(has_name(x = state, which = c("B", "D", "P", "Q", "M", "EP", "EM")))
  # assert_that(has_name(x = parms, which = c("parameter", "description", "units", "value")))
  # req <- c('E.c', 'V.d', 'm.r', 'K.d', 'V.p', 'K.p', 'V.m', 'K.m', 'K.ads',
  #         'Q.max', 'K.des', 'p.ep', 'p.em', 'r.ep',  'r.em')
  # missing <- setdiff(req, parms$parameter)
  # assert_that(length(missing) == 0, msg = paste('parms missing values for: ', paste0(missing, collapse = ', ')))
  #
  # # Format the parameters into a vector.
  # p        <- parms$value
  # names(p) <- parms$parameter

  with(as.list(env), {

    fxn_list <- list(
      "F1" = function(){
        # DOC uptake by microbial biomass.
        (1/E.c) * (V.d + m.r) * B *D /( K.d + D)
      },
      "F2" = function(){
        # POC decomposition
        V.p * EP * P / (K.p + P)
      },
      "F3" = function(){
        # Break down of mineralized organic carbon
        V.m * EM * M / (K.m + M)
      },
      "F4" = function(){
        # Microbial respiration from biomass growth
        (1/E.c - 1) * V.d * B * D /( K.d + D)
      },
      "F5" = function(){
        # Metabolic/maintenance microbial respiration
        (1/E.c -1) * m.r * B * D /( K.d + D)
      },
      "F6" = function(){
        # Adsorption of DOC to mineral-associated organic carbon
        K.ads * D *(1- Q/ Q.max)
      },
      "F7" = function(){
        # Desorption of mineral-associated organic carbon to DOC
        K.des * Q/Q.max
      },
      "F8" = function(){
        # Carbon loss due to microbial biomass mortality
        (1 - p.ep - p.em) * m.r * B
      },
      "F9.ep" = function(){
        # Enzyme production
        p.ep * m.r *B
      },
      "F9.em" = function(){
        # Enzyme production
        p.em * m.r * B },
      "F10.ep" = function(){
        # Enzyme turn over
        r.ep * EP },
      "F10.em" = function(){
        # Enzyme turn over
        r.em * EM })

    return(fxn_list)

  })

}
