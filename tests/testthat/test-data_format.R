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
plot(dat_stag)

## methods
lapply(dat_all, multiDiff:::intrnl_mdd_get_mdd_slot)
lapply(dat_all, multiDiff:::intrnl_mdd_get_pre_periods)

################################
#'## Other names?
################################

test_that("Missing vars are spotted", {
  expect_error(mdd_data_format(dat_DiD_raw, y_var = "HELOOOOO"),
               "Variable(s): HELOOOOO not in data?", fixed=TRUE)
})
