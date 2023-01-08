library(multiDiff)

dat_DiD_raw <- sim_dat_common()
dat_any_raw <- sim_dat()
dat_stag_raw <- sim_dat_staggered()

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
