library(multiDiff)

dat_DiD_raw <- sim_dat_common()
dat_any_raw <- sim_dat()
dat_stag_raw <- sim_dat_staggered()

## to mdd
dat_DiD <- mdd_data_format(data=dat_DiD_raw)
dat_any <- mdd_data_format(data=dat_any_raw)
dat_stag <- mdd_data_format(data=dat_stag_raw)

dat_DiD
dat_any
dat_stag

plot(dat_DiD)
plot(dat_stag)
