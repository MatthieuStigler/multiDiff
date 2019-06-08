## check one manually
add_group <- function(df, time.index = "Time", treat = "tr", unit.index="unit"){
  groups <- df %>%
    filter_at(vars(!!enquo(time.index)), ~.==max(.)) %>%
    mutate(seq = paste(lag_1, tr, sep="_"))
  df %>%
    left_join(groups %>%
                select(unit, seq), by = "unit")
}


DD_manu <-  dat_sim_1 %>%
  filter(Time %in% c(1,2)) %>%
  add_group() %>%
  filter(seq %in% c("0_1", "1_1")) %>%
  group_by(Time, seq) %>%
  summarise(y=mean(y)) %>%
  ungroup() %>%
  mutate(seq = paste("seq", seq, sep="_")) %>%
  spread(seq, y) %>%
  mutate(diff = seq_0_1-seq_1_1) %>%
  mat_add_total_row(fun=diff) %>%
  mutate(diff2 = seq_0_1-seq_1_1)

DD_manu
