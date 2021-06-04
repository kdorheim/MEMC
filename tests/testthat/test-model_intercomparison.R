context('model_intercomparison')


test_that("model_intercomparison", {

  out <- model_intercomparison(MEND_params, t = 1:10, MEND_initalState)
  expect_true(data.table::is.data.table(out))

  # Make sure that the models return different results from one another.
  rslts <- data.table::dcast(out, units + variable + time ~ model, value.var = 'value')
  expect_true(sum(abs(rslts$MEND - rslts$MEND_RM)) > 0)

})

