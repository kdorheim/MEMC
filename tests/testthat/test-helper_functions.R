context('helper_functions')

test_that("assign_parameters:", {

  expect_error(assign_parameters(data.frame(x = 'f'), 'data.table::is.data.table(x = dt) is not TRUE'))
  expect_error(assign_parameters(data.table::data.table('parameter' = 'g', 'value' = 'n'), 'dt[["value"]] is not a numeric or integer vector'))
  x <- assign_parameters(data.table::data.table('parameter' = 'g', 'value' = 11))
  expect_true(is.null(x))

  # If this function works then there should be an object called g, with the value of 11.
  expect_equal(g, 11)

  # Make sure that that an error is thrown if missing required parameters
  dt <- data.table::data.table('parameter' = 'g', 'value' = 11)
  expect_error(assign_parameters(dt, req = 'fake'))

  assign_parameters(data.table::data.table('parameter' = 'g', 'value' = 12), req = 'g')
  expect_equal(g, 12)

})


# Define the initial state values
B = 2; D = 1
P = 10; Q = 0.1
M = 5; EP = 0.00001
EM =  0.00001; IC = 0
Tot = 18.10002

state <- c(P = P,  M = M,  Q = Q,  B = B,  D = D,  EP = EP,  EM = EM,  IC = IC,  Tot = Tot)


testthat::test_that("solver:", {

  # Should run
  good <- solver(params = MEND2013_params, time = 0:10, state = state,
                 carbon_pools_func = MEND2013_pools, carbon_fluxes_func = MEND2013_fluxes)
  expect_true(is.data.frame(good))

  # Should throw error messages
  expect_error(solver(params = MEND2013_params, time = 0:10, state = state, carbon_pools_func = MEND2013_fluxes, carbon_fluxes_func = MEND2013_fluxes))
  expect_error(solver(params = MEND2013_params, time = 0:10, state = state, carbon_pools_func = MEND2013_pools, carbon_fluxes_func = MEND2013_pools))
  expect_error(solver(params = MEND2013_params, time = 0:10, state = state, carbon_pools_func = list(), carbon_fluxes_func = MEND2013_pools))
  expect_error(solver(params = MEND2013_params[1:10, ], time = 0:10, state = state,
                      carbon_pools_func = MEND2013_pools, carbon_fluxes_func = MEND2013_fluxes))

})
