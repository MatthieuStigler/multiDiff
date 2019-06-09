library(multiDiff)
library(tidyverse)
library(lfe)
library(matPkg)
library(plm)

data <- sim_dat(N=10000)

## pre trend
data_pre <- data %>%
  right_join(filter(data, Time==5 & lag_1==0 & lag_2==0) %>%
               mutate(group = paste(lag_2, lag_1, tr, sep="_")) %>%
               select(unit, group), by = "unit") %>%
  filter(Time %in% c(3,4,5))

data_pre %>%
  filter(Time==5) %>%
  count(group, tr, lag_1, lag_2)

## test at 5
felm(y~ tr |unit+Time, data=filter(data_pre, Time %in% c(4,5)))

## test at 3-4
dat_345 <- data_pre %>% filter(Time %in% c(3,4, 5))
dat_34 <- data_pre %>% filter(Time %in% c(3,4))

DD_manu(data=dat_345, treat_gr = "0_0_1", control_gr="0_0_0", time_per = c(3,4))

dat_34 %>%
  group_by(group, Time) %>%
  summarise(mean=mean(y)) %>%
  ungroup() %>%
  spread(group, mean) %>%
  mutate(diff = `0_0_1`-`0_0_0`) %>%
  mat_add_total_row(fun=diff) %>%
  mutate(diff2 = `0_0_1`-`0_0_0`)

felm(y~ lead_1 |unit+Time, data=dat_34)

felm(y~ -1+ group *factor(Time), data=filter(data_pre, Time %in% c(3,4)))
felm(y~ Time + group*Time, data=filter(data_pre, Time %in% c(3,4)))

felm(y~ Time + group:Time, data=filter(data_pre, Time %in% c(3,4)))

##
plm(y~ 1, data=filter(data_pre, Time %in% c(3,4)) %>%
      pdata.frame(index = c("unit", "Time")), model = "fd")

felm(y~ group |Time, data=filter(data_pre, Time %in% c(3,4)))
