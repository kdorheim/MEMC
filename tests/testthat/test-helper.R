
params <- MEMC::default_params
init   <- MEMC::default_inital

test_that("configure_model", {

  # check to make sure that the object returned is a list with the correct number of elements.
  out1 <- configure_model(params = params, state = init, carbon_pools_func = carbon_pools,
                          carbon_fluxes_func = carbon_fluxes)
  expect_true(is.list(out1))
  expect_true(length(out1) == 5)
  expect_true(is.null(out1$name))

  # Make sure that we can reset the model name behavior.
  test_name <- "test"
  out2 <- configure_model(params = params, state = init, carbon_pools_func = carbon_pools,
                          carbon_fluxes_func = carbon_fluxes, name = test_name)
  expect_equal(out2$name, test_name)

  # Checks to make sure that the appropriate errors are being thrown when the incorrect inputs are being read in.
  expect_error(configure_model(params = params, state = init, carbon_pools_func = carbon_fluxes,
                               carbon_fluxes_func = carbon_fluxes),
               regexp = "carbon_pool_func missing required arguments:  t, flux_function")
  expect_error(configure_model(params = params, state = init, carbon_pools_func = carbon_pools,
                               carbon_fluxes_func = carbon_pools),
               regexp = "check carbon_fluxes_func, make sure the pool function is not being used in stead.")
  expect_error(configure_model(params = params, state = init[1:3], carbon_pools_func = carbon_pools,
                                carbon_fluxes_func = carbon_fluxes, name = test_name),
                regexp = "missing states: B,  D,  EP,  EM,  IC,  Tot")
  expect_error(configure_model(params = params[1:5, ], state = init, carbon_pools_func = carbon_pools,
                               carbon_fluxes_func = carbon_fluxes, name = test_name),
               regexp = "parms missing values for:  I.p, g.d, f.d, I.d")


})

test_that("solve_model", {

  # Start by setting up a model and then solvinng, make sure that the stucture of the object being returned
  # is correct. Note, we are not checking model solutions here that will be done in the oldnew test to make
  # sure that the numerical solutions are robust to coding changes.
  test_time <- 1:5
  mod1 <- configure_model(params = params, state = init, carbon_pools_func = carbon_pools,
                          carbon_fluxes_func = carbon_fluxes)
  out1 <- solve_model(mod = mod1, time = test_time)
  expect_equal(unique(out1$time), test_time)
  expect_equal(length(unique(out1$time)) * length(unique(out1$variable)), nrow(out1))
  expect_equal(unique(out1$name), "(unnamed)")
  expect_named(out1, c("time", "variable", "value", "units", "name"))

  # Test to see that additional arguments works, aka passing in new parameter values
  # should change the numeric results.
  new_params <- params
  new_params$value <- params$value * 2
  out2 <- solve_model(mod = mod1, time = test_time, params = new_params)
  expect_gt(mean(abs(out2$value - out1$value)), 0)

  # Check to make sure that additional arguments can be pased into the ode solver.
  # Using the different solver methods should have a trivial impact on the numerical output.
  lsode <- solve_model(mod = mod1, time = test_time, method = "lsode")
  ode45 <- solve_model(mod = mod1, time = test_time, method = "ode45")
  expect_lte(mean(abs(lsode$value - ode45$value)), 1e-5)
  expect_lte(mean(abs(out1$value - ode45$value)), 1e-5)

  # Check that appropriate erros are being thrown.
  expect_error(solve_model(mod = mod1, time = "f"))
  expect_error(solve_model(mod = mod1[1:2], time = test_time), regexp = "mod must a model object created by configure_model")
  expect_error(solve_model(mod = mod1, time = test_time, params = params[1:5, ]), regexp = "problem with the params table being read in")
  expect_error(solve_model(mod = mod1, time = test_time, method = "fake"))
  expect_error(solve_model(mod = mod1, time = test_time, fake = "fake"))

})

test_that("update_params", {

  # make up a test dataframe to work with, note that it needs to have the MEMC default paramter table
  # column names.
  int <- 1:3
  table <- data.frame("parameter" = letters[int], "description" = "testthat", units = "testthat", value = int)
  new_params <- table$value

  # Check to make sure that all of the values are updated.
  names(new_params) <- rev(table$parameter)
  out1 <- update_params(new_params = new_params, param_table = table)
  expect_true(any(table$value != out1$value))

  # Check to make sure that a single value can be updated at a time.
  new_params <- 10
  names(new_params) <- 'c'
  out2 <- update_params(new_params = new_params, param_table = table)
  expect_equal( sum(table$value != out2$value), 1)

  # Ensure errors are thrown when appropriate!
  new_params <- 10
  names(new_params) <- 'fake'
  expect_error(update_params(new_params = new_params, param_table = table), "new_params must refer to a parameter already existing in param_table")

})
