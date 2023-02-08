library(multiDiff)

# Simul data
data <- sim_dat_common(N=100, Time=10,
                       timing_treatment = 6:10, perc_treat=0.5)

mdd_data <- mdd_data_format(data)

test_that("Function runs smoothly", {
  expect_no_error(mdd_test_placebo(mdd_dat=mdd_data))
})

test_that("Error if T_treated %in% c(0, T_pre)", {
  expect_error(mdd_test_placebo(mdd_dat=mdd_data, T_treated = 0), "Arg. 'T_treated' should be >0", fixed=TRUE)
  expect_error(mdd_test_placebo(mdd_dat=mdd_data, T_treated = 5), "Arg. 'T_treated' should not be bigger than 4", fixed=TRUE)
})
