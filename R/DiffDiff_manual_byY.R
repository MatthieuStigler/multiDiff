DD_manu_many <- function(y_var="y", data, time.index = "Time", treat = "tr", unit.index="unit",
                         lag=1, format_long=TRUE) {

  # time.index = quo("Time")
  # treat = quo("tr")
  # unit.index = quo("unit")

  lf_formula <-  as.formula(paste(y_var, ".treat|.unit+.time", sep="~"))
  data2 <- data %>%
    rename(.time=!!enquo(time.index),
           .treat=!!enquo(treat),
           .unit=!!enquo(unit.index),
           .yvar=!!enquo(y_var))

  times <- sort(unique(pull(data2, .data$.time)))
  years_df <- tibble(time = tail(times, -1))

  ## Generate treatment sequence: like 0_0_1 for unit 3 at time 4
  # data_treat_new <- data2 %>%
  #   select(.data$.time, .data$.treat, .data$.unit) %>%
  #   lag_group_many(group_var=.data$.unit,
  #                  time_var=.data$.time,
  #                  lagamount = 1, .treat)
  #
  # data_treat_new2 <- data_treat_new %>%
  #   tidyr::unite(seq, c(paste0(".treat_lag", seq_len(lag)), ".treat"), sep="_") %>%
  #   select(.time, seq, .unit)
  #
  # data_treat_new2
  # data_treat

  data_treat <- data2 %>%
    select(.data$.time, .data$.treat, .data$.unit, .data$.yvar)
  for(i in seq_len(lag)) {
    tr_lag_name <- rlang::sym(paste0(".treat_lag_", i))
    y_lag_name <- rlang::sym(paste0(".yvar_lag_", i))
    data_treat <-  data_treat %>%
      lag_group(group_var=.data$.unit, time_var=.data$.time, lag_var=.data$.treat, lagamount = i) %>%
      rename(!!tr_lag_name:=lag) %>%
      lag_group(group_var=.data$.unit, time_var=.data$.time, lag_var=.data$.yvar, lagamount = i) %>%
      rename(!!y_lag_name:=lag)
  }
  data_treat <- data_treat %>%
    tidyr::unite(seq, c(paste0(".treat_lag_", seq_len(lag)), ".treat"), sep="_", remove=FALSE)

  ## compute mean for each seq/time
  out <- data_treat %>%
    group_by_at(vars(one_of(c(".time", "seq")), starts_with(".tr"))) %>%
    summarise_at(vars(starts_with(".yvar")), mean) %>%
    ungroup() %>%
    left_join(data_treat %>%
                count(.data$.time, seq, name = "n_obs"), by = c(".time", "seq"))

  ## long?
  if(format_long){
    out <-  out %>%
      gather("variable", "value", starts_with(".yvar"), starts_with(".treat")) %>%
      mutate(lag = if_else(str_detect(.data$variable, "lag"), str_extract(.data$variable, "[0-9]$"), "0") %>%
               as.integer,
             variable = str_remove(.data$variable, "_lag_[0-9]")) %>%
      spread(.data$variable, .data$value) %>%
      mutate(actual_time = .data$.time-.data$lag,
             seq_unique = paste(.data$.time, seq, sep=": "))
  }
  out
}


################################
#'## TEST
################################

if(FALSE) {
  library(multiDiff)
  library(tidyverse)
  dat_sim_1 <- sim_dat(N=5000, Time = 5)
  coef(lfe::felm(y~tr|Time+unit, data = dat_sim_1))
  res_out <- DD(data = dat_sim_1,
                time.index = "Time")

  ## weighting?
  nrow(dat_sim_1)
  sum(res_out$n_vals)
  with(res_out, weighted.mean(estimate, D_var*(n_vals-2)))

  ##
  out <- DD_manu_many(data = dat_sim_1,
                      y_var = "y",
                      time.index = "Time",
                      unit.index = "unit", lag = 2)

  out

  ## Check got right
  out_DD <- DD(data = dat_sim_1,
                      y_var = "y",
                      time.index = "Time")

  ## plot
  out %>%
    filter(seq %in% c("0_0_0", "0_0_1")) %>%
    filter(.time %in% c(3, 5)) %>%
    ggplot(aes(x = actual_time, y = .yvar, color = factor(.time), linetype = seq)) +
    geom_line()

  out

}
