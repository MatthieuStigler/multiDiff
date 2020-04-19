library(multiDiff)
library(tidyverse, warn.conflicts=FALSE)
data <- sim_dat(N=100)

## TEST standard
DD(data=data)


## TEST no year with variation
data3 <- data %>%
  mutate(tr = 0)
DD_out <- DD(data=data3)
DD_out


## TEST some years without variation
data2 <- data %>%
  mutate(tr = if_else(Time %in% c(1,2), 0L, tr))
DD_out <- DD(data=data2)
DD_out
