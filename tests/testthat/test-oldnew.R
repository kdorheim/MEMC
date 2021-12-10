# Read in the archived comparison data.
old <- read.csv('compdata/com-new.csv')
t <- unique(old$time)

test_that("MEND_model behavivor is preserved", {

  mend_out <- solve_model(mod = MEND_model, time = t)
  old_mend <- old[old["name"] == "MEND", ]
  compdf <- merge(mend_out, old_mend, by = c("time", "variable", "units", "name"))
  expect_equal(compdf$value, compdf$old_value)

})
