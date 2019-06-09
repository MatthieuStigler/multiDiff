library(tidyverse)
library(matPkg)
library(multiDiff)

dat_sim_1 <- sim_dat(N=100000, Time = 5)

dat_34 <- dat_sim_1 %>%
  filter(Time%in%c(3,4))

dat_sim_1 %>%
  count(Time)


################################
#'## standard y
################################
res_out <- DD(data = dat_sim_1, time.index = "Time")

DiD_aggreg(res_out)
DiD_aggreg(res_out, by_DiD = TRUE, DiD_keep = 1:4)

res_out %>%
  mutate(time = if_else(DiD %in% c(2,4), time-1L, time)) %>%
  ggplot(aes(x=time, y=estimate))+
  # geom_ribbon(aes(ymin=conf.low, ymax = conf.high, group=factor(DiD)), fill="grey", alpha = I(0.4))+
  geom_line(aes(colour=factor(DiD))) +
  geom_hline(aes(yintercept =estimate), data = DiD_aggreg(res_out)) +
  geom_hline(aes(yintercept =estimate, colour = factor(DiD)),
             data = DiD_aggreg(res_out, by_DiD=TRUE, DiD_keep = 1:4),
             linetype = 2)


## by period
DD_manu(data = dat_34, treat_gr="1_0",  control_gr="1_1")

res_out %>%
  filter(time==4) %>%
  select(time, DiD, treat, control, estimate) %>%
  filter(treat=="1_0" & control=="1_1")


################################
#'## Counts
################################


sum(dat_sim_1$tr)

## counts
CNT <- res_out %>%
  select(time, DiD, n_treat, n_control) %>%
  gather(group, n, n_treat, n_control) %>%
  mutate(group=str_remove(group, "n_")) %>%
  left_join(res_out %>% distinct(DiD, treat, control), by = "DiD") %>%
  mutate(group = if_else(group=="treat", treat, control)) %>%
  arrange(time) %>%
  distinct(time, group, n) %>%
  group_by(time) %>%
  mutate(Tot = sum(n)) %>%
  ungroup()

CNT %>%
  distinct(time, Tot)


CNT %>%
  filter(time==3)

res_out %>%
  select(time, DiD, treat, control, n_treat, n_control) %>%
  filter(time==3)

res_out %>%
  select(time, DiD, treat, control, n_treat, n_control) %>%
  gather(group, value, treat, control, n_treat, n_control) %>%
  mutate(var_type = if_else(str_detect(group, "n_"), "count", "value"),
         group_type = str_extract(group, "treat|control")) %>%
  filter(time==2) %>%
  spread()
sum(res_out$n_treat)/4

################################
#'## y asym
################################

res_asym <- DD(data = dat_sim_1, time.index = "Time", y_var = "y_asym")

DiD_aggreg(res_asym, by_DiD = TRUE, DiD_keep = 1:4)


res_asym %>%
  mutate(time = if_else(DiD %in% c(2,4), time-1L, time)) %>%
  ggplot(aes(x=time, y=estimate))+
  # geom_ribbon(aes(ymin=conf.low, ymax = conf.high, group=factor(DiD)), fill="grey", alpha = I(0.4))+
  geom_line(aes(colour=factor(DiD))) +
  geom_hline(aes(yintercept =estimate), data = DiD_aggreg(res_asym)) +
  geom_hline(aes(yintercept =estimate, colour = factor(DiD)),
             data = DiD_aggreg(res_asym, by_DiD=TRUE, DiD_keep = 1:4),
             linetype = 2)

res_asym %>%
  filter(time==4) %>%
  select(time, DiD, treat, control, estimate) %>%
  filter(treat=="1_0" & control=="1_1")
