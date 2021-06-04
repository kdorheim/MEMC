context('MEND_RM')

state <- MEND_initalState

# Test the functions that make up the MEND 2013 system of equations.
test_that("MEND_RM_fluxes", {

  # Make sure that an error is thrown when the incorrect inputs are  used.
  expect_error(MEND_RM_fluxes(state = state[1:2], parms = MEND_params),
               "state does not have all of these name(s): 'B', 'D', 'P', 'Q', 'M', 'EP', 'EM'", fixed = TRUE)
  expect_error(MEND_RM_fluxes(state = state, parms = MEND_params[ ,1:2]),
               "parms does not have all of these name(s): 'parameter', 'description', 'units', 'value'", fixed = TRUE)
  expect_error(MEND_RM_fluxes(state = state, parms = MEND_params[1:2,]),
               "parms missing values for:  E.c, V.d, m.r, K.d, V.m, K.m, K.ads, Q.max, K.des, p.ep, p.em, r.ep, r.em", fixed = TRUE)

  #  MEND_fluxes should return a functions that can return numeric values.
  x <- MEND_RM_fluxes(state = state, parms = MEND_params)
  expect_true(all(unlist(lapply(x, is.function))))

  xx <- x$F1()
  expect_true(is.numeric(xx))

})


test_that("MEND_RM", {

  out <- MEND_RM(MEND_params, t = 1:10, MEND_initalState)
  expect_true(all(is.numeric(out$value)))

})
