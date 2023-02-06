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

test_that("Function runs smoothly with time.omit=-2", {
  expect_no_error(mdd_test_pre_trend_event(mdd_dat=mdd_data, time.omit= -2))
})

## means: insensitive to base period
test_that("_means is invariant to base period", {
  means_pre5 <- mdd_test_pre_trend_means(mdd_dat=mdd_data, time_ref = "5")
  means_pre1 <- mdd_test_pre_trend_means(mdd_dat=mdd_data, time_ref = "1")
  expect_equal(means_pre5[1,2],
               means_pre1[1,2])
})

test_that("_event is invariant to base period", {
  wald_pre5 <- mdd_test_pre_trend_event(mdd_event_study(mdd_data, time.omit = -5))
  wald_pre1 <- mdd_test_pre_trend_event(mdd_event_study(mdd_data, time.omit = -1))
  expect_equal(wald_pre5[1,"statistic"],
               wald_pre1[1,"statistic"])
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

################################
#'## years, gaps
################################

dat_DiD_raw_years <- sim_dat_common(Time = 10, timing_treatment = 6:10) |>
  dplyr::filter(!Time %in% c(4,8))

test_that("Works with years", {
  expect_no_error(mdd_test_pre_trend_means(mdd_dat = mdd_data_format(dat_DiD_raw_years)))
})


dat_DiD_raw_gaps <- sim_dat_common(Time = 10, timing_treatment = 6:10) |>
  dplyr::filter(!Time %in% c(4,8))|>
  dplyr::mutate(Time =Time+2000)

test_that("Speciifcation of years: error if not found", {
  expect_error(mdd_test_pre_trend_means(mdd_dat = mdd_data_format(dat_DiD_raw_gaps), time_ref = "2020"),
               "Arg. 'time_ref' not found in identified pre-periods: 2001,2002,2003,2005", fixed=TRUE)
})

test_that("Current problem with gaps in years", {
  expect_no_error(mdd_test_pre_trend_means(mdd_dat = mdd_data_format(dat_DiD_raw_gaps)))
})

