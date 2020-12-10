context('helper_functions')

test_that("assign_parameters:", {

 expect_error(assign_parameters(data.frame(x = 'f'), 'data.table::is.data.table(x = dt) is not TRUE'))
  expect_error(assign_parameters(data.table::data.table('parameter' = 'g', 'value' = 'n'), 'dt[["value"]] is not a numeric or integer vector'))
  x <- assign_parameters(data.table::data.table('parameter' = 'g', 'value' = 11))
  expect_true(is.null(x))

  # If this function works then there should be an object called g, with the value of 11.
  expect_equal(g, 11)

  # Make sure that that an error is thrown if missing required parameters
  expect_error(assign_parameters(data.table::data.table('parameter' = 'g', 'value' = 11), req = 'fake'))
  assign_parameters(data.table::data.table('parameter' = 'g', 'value' = 12), req = 'g')
  expect_equal(g, 12)

})
