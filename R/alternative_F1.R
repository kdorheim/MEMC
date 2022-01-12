
#' Select a SOM decomposition flux to use
#'
#' @param kinetics method of kinetics to use in the SOM decomposition is a string ("MM", "RMM", "ECA", "LM") by default is set to MM
#' @return a function to be used in the flux function
#' @importFrom assertthat assert_that
#' @export
#' @family helper functions
select_F1 <- function(kinetics = "MM"){

  assert_that(is.character(kinetics))
  assert_that(length(kinetics) == 1)
  options <- c("MM", "RMM", "ECA", "LM")
  assert_that(any(kinetics %in% options))

  if(kinetics == "MM"){
    out <- list("F1" = function(){
      # DOC uptake by microbial biomass.
      # Michaelis menten kinetics
      (1/E.c) * (V.d + m.r) * B * D /(K.d + D)})

  } else if(kinetics == "RMM") {
    out <-  list('F1' = function(){
      # DOC uptake by microbial biomass.
      # Reverse michaelis menten kinetics
      (1/E.c) * (V.d + m.r) * B * D /(K.d + B)})

  } else if(kinetics == "ECA"){
    out <- list("F1" = function(){
        # DOC uptake by microbial biomass, note that this uses ECA kinetics.
        (1/E.c) * (V.d + m.r) * B * D /(K.d + B + D)
      })

  } else if(kinetics == "LM") {
    message("needs to be implemented")

  } else {
    stop("requesting unkown kinetics")

  }

  return(out)
}
