library(multiDiff)

# Simul data
data <- sim_dat_common(N=100, Time=10,
                       timing_treatment = 6:10, perc_treat=0.5)

mdd_data <- mdd_data_format(data)

## test pre-trends
test_that("Function runs smoothly", {
  expect_no_error(mdd_test_pre_trend_means(mdd_dat=mdd_data))
})

