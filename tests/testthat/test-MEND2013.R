
context('old new test')


testthat::test_that("MEND2013", {

  B = 2; D = 1
  P = 10; Q = 0.1
  M = 5; EP = 0.00001
  EM =  0.00001; IC = 0
  Tot = 18.10002

  state <- c(P = P,  M = M,  Q = Q,  B = B,  D = D,  EP = EP,  EM = EM,  IC = IC,  Tot = Tot)
  t <- seq(0, 1e3, by = 0.1)

  out1 <- solver(params = MEND2013_params,
                time = t,
                state = state,
                carbon_pools_func = MEND2013_pools,
                carbon_fluxes_func = MEND2013_fluxes)

  old <- read.csv('compdata/old_new-MEND2013.csv')

  expect_equal(dim(old), dim(out1))
  expect_equal(old$value, out1$value)

  out2 <- MEND2013(parameters = MEND2013_params, time = t, inital_state = state )
  expect_equal(out2, new)

})

