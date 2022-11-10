library(multiDiff)

# Simul data
data <- sim_dat_common(N=100, Time=10,
                       timing_treatment = 6:10, perc_treat=0.5)

mdd_data <- mdd_data_format(data)

## estimate ES
ES <- mdd_event_study(mdd_data)
ES_omit5 <- mdd_event_study(mdd_data, time.omit = -5)

## test pre-trends
test_that("Function runs smoothly", {
  expect_no_error(mdd_test_pre_trend_means(mdd_dat=mdd_data))
})

mdd_test_pre_trend_means(mdd_dat=mdd_data, time_ref = "4")
mdd_test_pre_trend_event(mdd_dat = mdd_data)
mdd_test_pre_trend_event(mdd_dat = ES)

test_that("Function runs smoothly", {
  expect_error(suppressWarnings(mdd_test_pre_trend_event(mdd_dat = ES_omit5)),
               "arguments imply differing number of rows: 0, 1")
})
