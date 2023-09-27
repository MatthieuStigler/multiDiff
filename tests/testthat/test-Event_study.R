library(multiDiff)

DID_dat_raw <- sim_dat_common(N=5000, Time=8,
                              beta=1.1,
                              timing_treatment = 6:8, perc_treat=0.5)

DID_dat <- mdd_data_format(DID_dat_raw)

## Manu means
means_manu <- DID_dat |>
  multiDiff::mdd_group_means() |>
  tidyr::spread(.group, y) |>
  dplyr::mutate(diff=treated-control,
                diff_lag = dplyr::lag(diff),
                diff_diff = diff-diff_lag,
                ES = diff - diff[5])

means_manu

## Did simple full
DiD_simple <- mdd_DD_simple(DID_dat)
coef(DiD_simple)

## Did simple: only 2Y
test_that("DID for 2Y is same as diff-diff means", {
  expect_equal(dplyr::filter(means_manu, Time==6)$diff_diff,
               coef(mdd_DD_simple(mdd_dat=DID_dat |> dplyr::filter(Time %in% c(5, 6))))[[1]])
})

## event
ES <- mdd_event_study(mdd_dat = DID_dat)
summary(ES)
plot(ES)

test_that("ES is same as diffs in DD_manu: diff- diff[5]", {
  expect_equal(coef(ES) %>% as.numeric(),
               (means_manu$diff - means_manu$diff[5])[-5])
})

test_that("ES 0 effect is same as diffs annual", {
  expect_equal(coef(ES)["timing_to_treat0"][[1]],
               coef(mdd_DD_simple(mdd_dat=DID_dat %>% dplyr::filter(Time %in% c(5, 6))))[[1]])
})


mdd_event_study(mdd_dat=DID_dat)
mdd_event_study(mdd_dat=DID_dat, trim_high = 1)
mdd_event_study(mdd_dat=DID_dat, trim_high = 0)
mdd_event_study(mdd_dat=DID_dat, trim_high = 0, trim_low = -3)

## weights
w <- rep(c(0, 1), c(8, nrow(DID_dat)-8))
ES_with_weights <- suppressMessages(mdd_event_study(mdd_dat=DID_dat, weights = w))
ES_subset  <- mdd_event_study(mdd_dat=DID_dat |> tail(-8))

test_that("Using with weights in ES works", {
  expect_equal(coef(ES_with_weights),
               coef(ES_subset))
})

################################
#'## Approach is similar to formula
################################

## add timing to treat
df <- DID_dat |>
  multiDiff:::intrnl_add_treat_time_mdd(keep_mdd = TRUE) |>
  multiDiff:::intrnl_add_treat_status_mdd() |>
  dplyr::mutate(timing_to_treat_num = ifelse(treat_categ=="Treat", Time -treat_timing, 0),
                timing_to_treat = factor(timing_to_treat_num) |> relevel("-1"),
                timing_to_treat_vs_2 = factor(timing_to_treat_num) |> relevel("-2"))


ES_alter <- fixest::feols(y~ timing_to_treat|unit+Time, data=df)
ES_pkg <- mdd_event_study(DID_dat)

test_that("Lags versus factor approach is equivalent", {
  expect_equal(coef(ES_alter),
               coef(ES_pkg))
})

ES_alter_2 <- fixest::feols(y~ timing_to_treat_vs_2|unit+Time, data=df)
ES_pkg_2 <- mdd_event_study(DID_dat, time.omit = -2)

test_that("Lags versus factr approach is equivalent (vs -2)", {
  expect_equal(coef(ES_alter_2),
               coef(ES_pkg_2), ignore_attr = TRUE)
})


################################
#'## Other names
################################

DID_dat_alter <- DID_dat_raw |>
  dplyr::rename(unit_id=unit, time_id=Time,
                y_outcome=y, treat_var=tr) |>
  mdd_data_format(y_var = "y_outcome",
                  unit.index = "unit_id", time.index = "time_id",
                  treat = "treat_var")

test_that("Works with different variables names: mdd_event_study", {
  expect_equal(coef(mdd_event_study(DID_dat_alter)),
               coef(mdd_event_study(DID_dat)))
})

test_that("Works with different variables names: mdd_DD_simple", {
  expect_equal(coef(mdd_DD_simple(DID_dat_alter))[[1]],
               coef(mdd_DD_simple(DID_dat))[[1]])
})

