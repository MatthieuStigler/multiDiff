library(multiDiff)
library(tidyverse)
library(lfe)
library(matPkg)
library(plm)



################################
#'## Simulate data
################################

data <- sim_dat(N=10000, Time = 5, seed=1234)

## pre trend
data_pre <- data %>%
  right_join(filter(data, Time==5 & lag_1==0 & lag_2==0) %>%
               mutate(group = paste(lag_2, lag_1, tr, sep="_")) %>%
               select(unit, group), by = "unit") %>%
  filter(Time %in% c(2, 3,4,5))

data_pre %>%
  filter(Time==5) %>%
  count(group, tr, lag_1, lag_2)

## subsets
dat_345 <- data_pre %>% filter(Time %in% c(3,4, 5))
dat_34 <- data_pre %>% filter(Time %in% c(3,4))
dat_45 <- data_pre %>% filter(Time %in% c(4,5))


################################
#'## Standard DiD
################################

## DiD at 5
felm(y~ tr |unit+Time, data=filter(data_pre, Time %in% c(4,5)))

DD_manu(data=dat_45, treat_gr = "0_1", control_gr="0_0")
DD_manu(data=dat_345, treat_gr = "0_0_1", control_gr="0_0_0",
        subperiod = c(NA,1,2))

################################
#'## Placebo DiD
################################


### LAG 1 ###

## manual
DD_manu(data=dat_345, treat_gr = "0_0_1", control_gr="0_0_0",
        subperiod = c(1,2,NA))

## with reg
felm(y~ lead_1 |unit+Time, data=dat_34)

### LAG 2 ###

## lag 2 manu
DD_manu(data=data_pre %>%
          filter(Time %in% c(2, 3, 4, 5)),
        treat_gr = "0_0_0_1", control_gr="0_0_0_0",
        subperiod = c(1,2,NA, NA))

##felm
felm(y~ lead_2 |unit+Time, data=data_pre %>% filter(Time %in% c(2, 3, 4)))

felm(y~ -1+ group *factor(Time), data=filter(data_pre, Time %in% c(3,4)))
felm(y~ Time + group*Time, data=filter(data_pre, Time %in% c(3,4)))

felm(y~ Time + group:Time, data=filter(data_pre, Time %in% c(3,4)))

##
plm(y~ 1, data=filter(data_pre, Time %in% c(3,4)) %>%
      pdata.frame(index = c("unit", "Time")), model = "fd")

felm(y~ group |Time, data=filter(data_pre, Time %in% c(3,4)))

