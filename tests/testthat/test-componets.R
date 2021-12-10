
params <- MEMC::default_params
init   <- MEMC::default_inital

test_that("carbon_fluxes", {

  # Make sure that all of the elements in returned by the carbon fluxes are numeric elements
  out <- carbon_fluxes(init, params)
  expect_true(is.list(out))
  for (x in out){
    xx <- x()
    expect_true(is.numeric(xx))
  }

  # Make sure that an error message is thrown!
  expect_error(object = carbon_fluxes(init[1:3], params),
               "state does not have all of these name(s): 'B', 'D', 'P', 'Q', 'M', 'EP', 'EM'", fixed = TRUE)
  expect_error(object = carbon_fluxes(init, params[1:3, ]),
               "parms missing values for:  E.c, V.d, m.r, K.d, K.m, K.ads, Q.max, K.des, p.ep, p.em, r.ep, r.em", fixed = TRUE)
})

test_that("carbon_poools", {

  out1 <- unlist(carbon_pools(t = 1, state = init, parms = params, flux_function = carbon_fluxes))
  expect_true(all(is.numeric(out1)))

  new_table <- params
  new_table[1, ]$value <- new_table[1, ]$value * 10

  out2 <- unlist(carbon_pools(t = 1, state = init, parms = new_table, flux_function = carbon_fluxes))
  expect_true(any(out1 != out2))

  # Make sure that error messages are thrown!
  expect_error(object = carbon_pools(t = "l", state = init, parms = new_table, flux_function = carbon_fluxes),
               regexp = "t must be numeric", fixed = TRUE)
  expect_error(object = carbon_pools(t = 1, state = init[1:2], parms = new_table, flux_function = carbon_fluxes),
               regexp = "missing states: Q,  B,  D,  EP,  EM,  IC,  Tot", fixed = TRUE)
  expect_error(object = carbon_pools(t = 1, state = init, parms = new_table[1:10, ], flux_function = carbon_fluxes),
               regexp = "parms missing values for:  I.p, I.d", fixed = TRUE)
  expect_error(object = carbon_pools(t = 1, state = init, parms = new_table, flux_function = sum))

})



