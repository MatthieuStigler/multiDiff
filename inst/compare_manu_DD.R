library(tidyverse)
library(matPkg)
library(multiDiff)

dat_sim_1 <- sim_dat(N=100000, Time = 10, seed=123, prob_treat=0.5)
dat_sim_2 <- sim_dat(N=1000000, Time = 10, seed=123, prob_treat=0.5)

seqs  <-  dat_sim_1 %>%
  select(unit, Time, tr) %>%
  spread(Time, tr) %>%
  unite(full_seq, as.character(1:10))

seqs_2  <-  dat_sim_2 %>%
  select(unit, Time, tr) %>%
  spread(Time, tr) %>%
  unite(full_seq, as.character(1:10))

add_DiD <- function(df) {
  df %>%
    mutate(DiD_type = paste(DiD, ": ", treat, " vs ", control, sep=""))
}


dat_sim_1_c <- dat_sim_1 %>%
  left_join(seqs, by ="unit")


seqs_2_ok <- seqs_2 %>%
  filter(full_seq %in% c("0_0_0_0_0_0_0_0_0_0", "1_1_1_1_1_1_1_1_1_1",
                                    "0_1_0_1_0_1_0_1_0_1", "1_0_1_0_1_0_1_0_1_0"))

dat_sim_2_c_4G <- dat_sim_2 %>%
  semi_join(seqs_2_ok, by ="unit") %>%
  left_join(seqs_2, by ="unit")


dat_sim_2_c_4G %>%
  count(full_seq)

dat_34 <- dat_sim_1 %>%
  filter(Time%in%c(3,4))

dat_sim_1 %>%
  count(Time, tr) %>%
  spread(tr, n)


################################
#'## Illustrate one
################################

means_one <- function(df, time.index = "Time", treat = "tr", unit.index="unit", y_var="y") {
  df %>%
    add_group() %>%
    group_by(!!rlang::sym(time.index), .group) %>%
    summarise(!!rlang::sym(y_var):=mean(!!rlang::sym(y_var)),
              n_obs = n()) %>%
    ungroup() %>%
    rename(timing = !!rlang::sym(time.index))
}

dat_sim_1 %>%
  filter(Time %in% c(1,2)) %>%
  means_one()

get_means <- function(df, time.index = "Time", treat = "tr", unit.index="unit", y_var="y") {
  times <- sort(unique(pull(df, !!rlang::sym( time.index))))
  tibble(Time = times[-1]) %>%
    mutate(means = map(Time, ~filter(df, !!rlang::sym( time.index) %in% c(.x-1,.x)) %>%
                         means_one(y_var=y_var))) %>%
    unnest(means)
}

means_all <- dat_sim_1 %>%
  get_means() %>%
  mutate(type = case_when(.group %in% c("0_0", "1_1") ~ "Control",
                          .group %in% c("0_1", "1_0") ~ "Treat"))

means_all_asym <- dat_sim_1 %>%
  get_means(y_var = "y_asym") %>%
  mutate(type = case_when(.group %in% c("0_0", "1_1") ~ "Control",
                          .group %in% c("0_1", "1_0") ~ "Treat")) %>%
  rename(y=y_asym)


pl_seqs <- means_all_asym %>%
  ggplot(aes(x= timing, y = y, colour = factor(Time),  linetype = .group)) +
  geom_line()

pl_seqs


################################
#'## standard y
################################
res_out <- DD(data = dat_sim_1, time.index = "Time")

DiD_aggreg(res_out)
DiD_aggreg(res_out, by_DiD = TRUE, DiD_keep = 1:4)

res_out_c <- res_out %>%
  mutate(time = if_else(DiD %in% c(2,4), time-1L, time)) %>%
  add_DiD()

res_out_c %>%
  ggplot(aes(x=time, y=estimate))+
  geom_ribbon(aes(ymin=conf.low, ymax = conf.high, group=DiD_type), fill="grey", alpha = I(0.4))+
  geom_line(aes(colour=DiD_type)) +
  geom_hline(aes(yintercept =estimate), data = DiD_aggreg(res_out)) +
  geom_hline(aes(yintercept =estimate, colour = DiD_type),
             data = DiD_aggreg(res_out, by_DiD=TRUE, DiD_keep = 1:4) %>%
               add_DiD, linetype = 2)


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
res_asym_3G <- DD(data =dat_sim_2_c_4G,
                  time.index = "Time", y_var = "y_asym")%>%
  mutate(time = if_else(DiD %in% c(2,4), time-1L, time)) %>%
  add_DiD()


DiD_aggreg(res_asym, by_DiD = TRUE, DiD_keep = 1:4)

res_asym_c <- res_asym %>%
  mutate(time = if_else(DiD %in% c(2,4), time-1L, time)) %>%
  add_DiD()

pl_DiD_asym <- res_asym_c %>%
  ggplot(aes(x=time, y=estimate))+
  geom_ribbon(aes(ymin=conf.low, ymax = conf.high, group=DiD_type), fill="grey", alpha = I(0.4))+
  geom_line(aes(colour=DiD_type)) +
  geom_hline(aes(yintercept =estimate), data = DiD_aggreg(res_asym)) +
  geom_hline(aes(yintercept =estimate, colour = DiD_type),
             data = DiD_aggreg(res_asym, by_DiD=TRUE, DiD_keep = 1:4) %>%
               add_DiD(),
             linetype = 2)


pl_DiD_asym


## 4 groups only
res_asym_3G  %>%
  mutate(DiD_group = case_when(DiD %in% c(1, 4) ~ "T2",
                               DiD %in% c(2, 3) ~ "T1")) %>%
  ggplot(aes(x=time, y=estimate))+
  geom_ribbon(aes(ymin=conf.low, ymax = conf.high, group=DiD_type, fill=DiD_type), alpha = I(0.2))+
  geom_line(aes(colour=DiD_type)) +
  facet_grid(DiD_group~.)

pl_seqs

res_asym %>%
  filter(time==4) %>%
  select(time, DiD, treat, control, estimate) %>%
  filter(treat=="1_0" & control=="1_1")
