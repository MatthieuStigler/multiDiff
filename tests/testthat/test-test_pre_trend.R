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

mdd_test_pre_trend_means(mdd_dat=mdd_data, time_ref = "5")
mdd_test_pre_trend_event(mdd_dat = mdd_data)
mdd_test_pre_trend_event(mdd_dat = ES)

test_that("Function runs smoothly", {
  expect_error(suppressWarnings(mdd_test_pre_trend_event(mdd_dat = ES_omit5)),
               "arguments imply differing number of rows: 0, 1")
})


################################
#'## Other names
################################

mdd_data_alter <- data |>
  dplyr::rename(unit_id=unit, time_id=Time,
                y_outcome=y, treat_var=tr) |>
  mdd_data_format(y_var = "y_outcome",
                  unit.index = "unit_id", time.index = "time_id",
                  treat = "treat_var")

test_that("Works with different variables names: _means", {
  expect_equal(mdd_test_pre_trend_means(mdd_dat=mdd_data),
               mdd_test_pre_trend_means(mdd_dat=mdd_data_alter))
})

test_that("Works with different variables names: _event", {
  expect_equal(mdd_test_pre_trend_event(mdd_dat=mdd_data),
               mdd_test_pre_trend_event(mdd_dat=mdd_data_alter))
})

