context('MEND2013')

# Define the inital state values
B = 2; D = 1
P = 10; Q = 0.1
M = 5; EP = 0.00001
EM =  0.00001; IC = 0
Tot = 18.10002

state <- c(P = P,  M = M,  Q = Q,  B = B,  D = D,  EP = EP,  EM = EM,  IC = IC,  Tot = Tot)


# Test the functions that make up the MEND 2013 system of equations.
test_that("MEND 2013 fluxes", {

  # Make sure that an error is thrown when the incorrect inputs are  used.
  expect_error(MEND2013_fluxes(state = state[1:2], parms = MEND2013_params),
               "state does not have all of these name(s): 'B', 'D', 'P', 'Q', 'M', 'EP', 'EM'", fixed = TRUE)
  expect_error(MEND2013_fluxes(state = state, parms = MEND2013_params[ ,1:2]),
               "parms does not have all of these name(s): 'parameter', 'description', 'units', 'value'", fixed = TRUE)
  expect_error(MEND2013_fluxes(state = state, parms = MEND2013_params[1:2,]),
               "parms missing values for:  E.c, V.d, m.r, K.d, V.m, K.m, K.ads, Q.max, K.des, p.ep, p.em, r.ep, r.em", fixed = TRUE)

  #  MEND2013_fluxes should return a functions that can return numeric values.
  x <- MEND2013_fluxes(state = state, parms = MEND2013_params)
  testthat::expect_true(all(unlist(lapply(x, is.function))))

  xx <- x$F1()
  testthat::expect_true(is.numeric(xx))


})
