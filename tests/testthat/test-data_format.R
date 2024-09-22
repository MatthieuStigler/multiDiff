library(dplyr, warn.conflicts = FALSE)
library(multiDiff)

dat_DiD_raw <- sim_dat_common()
dat_any_raw <- sim_dat(seed = 123)
dat_stag_raw <- sim_dat_staggered(seed = 123)

## to mdd
dat_DiD <- mdd_data_format(data=dat_DiD_raw)
dat_any <- mdd_data_format(data=dat_any_raw)
dat_stag <- mdd_data_format(data=dat_stag_raw)

dat_all <- list(dat_DiD=dat_DiD, dat_any=dat_any, dat_stag = dat_stag)
dat_DiD
dat_any
dat_stag

plot(dat_DiD)
plot(dat_DiD, conf.int=TRUE)
plot(dat_stag)

## methods
lapply(dat_all, multiDiff:::intrnl_mdd_get_mdd_slot)
lapply(dat_all, multiDiff:::intrnl_mdd_get_pre_periods) |>suppressWarnings()

################################
#'## Other names?
################################

test_that("Missing vars are spotted", {
  expect_error(mdd_data_format(dat_DiD_raw, y_var = "HELOOOOO"),
               "Variable(s): HELOOOOO not in data?", fixed=TRUE)
})

################################
#'## Gaps in years?
################################

## Use years
dat_DiD_raw_years <- sim_dat_common(Time = 10, timing_treatment = 6:10) |>
  dplyr::mutate(Time =Time+2000)

test_that("Correct formatting of years", {
  form_dat_years <- mdd_data_format(data = dat_DiD_raw_years)
  expect_equal(attributes(form_dat_years)$mdd_dat_slot$treated_periods,
               2006:2010)
})



## add gaps
dat_DiD_raw_gaps <- sim_dat_common(Time = 10, timing_treatment = 6:10) |>
  dplyr::filter(!Time %in% c(4,8))|>
  dplyr::mutate(Time =Time+2000)

mdd_data_format(dat_DiD_raw_gaps)

test_that("Correct formatting of years with gaps", {
  form_dat_gaps <- mdd_data_format(data = dat_DiD_raw_gaps)
  expect_equal(attributes(form_dat_gaps)$mdd_dat_slot$treated_periods,
               c(2006, 2007, 2009, 2010))
})

## warning messages
test_that("Warn when miss variables", {
  dat_modif <- dat_DiD_raw_years |>
    dplyr::rename(y_out=y, treat=tr)
  expect_error(mdd_data_format(data = dat_modif),
               "Variable(s): y, tr not in data?", fixed=TRUE)
})


################################
#'## Output
################################

test_that("Check output of print", {
  expect_snapshot(dat_DiD)
  expect_snapshot(dat_any)
  expect_snapshot(dat_stag)
})

################################
#'## Cross sectional data
################################

df_raw <- sim_dat_common(Time=2, timing_treatment = 1, perc_treat = 0.5, seed = 45)

## create cross-section of regressions
df_cross <- df_raw |>
  mutate(keep = rep(c(TRUE, FALSE, FALSE, TRUE), times =500)) |>
  filter(keep) |>
  select(-keep)

## same, but with unequal size groups
df_cross_unequalN <- sim_dat_common(Time=2, timing_treatment = 1, perc_treat = 0.4, seed = 45) |>
  mutate(keep = rep(c(TRUE, FALSE, FALSE, TRUE), times =500)) |>
  filter(keep) |>
  select(-keep)

df_cross_unequalN |>
  add_count(unit, name = "n_obs_by_unit") |>
  count(n_obs_by_unit)

## as mdd
test_that("Warn when only 1 obs per unit", {
  expect_warning(mdd_data_format(data = df_cross),
               "Only one observation by unit? Might need to change argument `unit.index`?", fixed=TRUE)
  expect_warning(mdd_data_format(data = df_cross_unequalN),
               "Only one observation by unit? Might need to change argument `unit.index`?", fixed=TRUE)
})

test_that("Works for cross-sec regressions", {
  expect_no_warning(mdd_data_format(data = df_cross, unit.index = "treat_group"))
  expect_no_warning(mdd_DD_simple(mdd_data_format(data = df_cross, unit.index = "treat_group")))
  expect_no_warning(mdd_data_format(data = df_cross_unequalN, unit.index = "treat_group"))
})

test_that("Panel DiD with FE as group or unit gives same coef", {
  FE_unit <- mdd_data_format(data = df_raw, unit.index = "unit")
  FE_group <- mdd_data_format(data = df_raw, unit.index = "treat_group")
  expect_equal(coef(mdd_DD_simple(FE_unit)),
               coef(mdd_DD_simple(FE_group)))
})


test_that("cross-section", {
  FE_unit <- mdd_data_format(data = df_raw, unit.index = "unit")
  FE_group <- mdd_data_format(data = df_raw, unit.index = "treat_group")
  df_cross_mdd <- suppressWarnings(mdd_data_format(data = df_cross))
  df_cross_unequalN_mdd <- suppressWarnings(mdd_data_format(data = df_cross_unequalN))
  is_cross <- function(x) attributes(x)$mdd_dat_slot$is_cross_sec

  expect_true(is_cross(df_cross_mdd))
  expect_true(is_cross(df_cross_unequalN_mdd))
  expect_false(is_cross(FE_unit))
  expect_false(is_cross(FE_group))
  expect_false(all(purrr::map_lgl(dat_all, is_cross)))


})
