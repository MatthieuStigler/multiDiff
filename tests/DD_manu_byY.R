library(multiDiff)
library(tidyverse)

DD_manu_many_diffs <-  multiDiff:::DD_manu_many_diffs
DD_manu_many_dids <-  multiDiff:::DD_manu_many_dids

## simu
dat_sim_1 <- sim_dat(N=5000, Time = 5)

##
out_w <- DD_manu_many(data = dat_sim_1,
                      y_var = "y",
                      time.index = "Time",
                      unit.index = "unit", lag = 1,
                      format_long = FALSE)

out_l1 <- DD_manu_many(data = dat_sim_1,
                       y_var = "y",
                       time.index = "Time",
                       unit.index = "unit", lag = 1)
out_l2 <- DD_manu_many(data = dat_sim_1,
                       y_var = "y",
                       time.index = "Time",
                       unit.index = "unit", lag = 2)



## Check got right
out_DD <- DD(data = dat_sim_1,
             y_var = "y",
             time.index = "Time")
compare <- out_DD %>%
  filter(DiD%in% c(1, 4)) %>%
  select(time, DiD, treat, control, estimate) %>%
  select(time, DiD, estimate) %>%
  mutate(DiD=case_when(DiD==1 ~ "did_01_vs_00",
                       DiD==4 ~ "did_10_vs_11")) %>%
  spread(DiD, estimate) %>%
  rename(`.time`=time) %>%
  mutate(did_10_vs_11 = -1*did_10_vs_11)

here <- DD_manu_many_dids(out_l1) %>%
  select(.time, starts_with("did")) %>%
  mutate(.time = as.integer(.time))

all.equal(compare %>% as.data.frame(), here %>% as.data.frame())



DD_manu_many_diffs(df=out_l1)
DD_manu_many_diffs(out_l2)
