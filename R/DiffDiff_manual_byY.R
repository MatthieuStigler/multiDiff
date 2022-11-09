#' Multi-year diff and diff, manually
#'
#' This function computes the year-to-year Did. This is useful mainly for visualisation, in
#'  particular for the pre-trends (settng lag>1), but has no inference for the DiD effect itself.
#'
#' @template param_all
#' @param lag How many lags to look at? Lag=1 will consider 0_1 sequences, lag 2: 0_1_1 etc
#' @param format_long Should it format in long or wide?
#' @export
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
    select(".time", ".treat", ".unit", ".yvar") %>%
    lag_group(group_var=".unit", time_var=".time",
              value_var= c(".treat",".yvar"), lagamount = seq_len(lag))%>%
    tidyr::unite(seq, c(paste0(".treat_lag", rev(seq_len(lag))), ".treat"), sep="->", remove=FALSE)
  # for(i in seq_len(lag)) {
  #   tr_lag_name <- rlang::sym(paste0(".treat_lag_", i))
  #   y_lag_name <- rlang::sym(paste0(".yvar_lag_", i))
  #   data_treat <-  data_treat %>%
  #     lag_group(group_var=.data$.unit, time_var=.data$.time, lag_var=.data$.treat, lagamount = i) %>%
  #     rename(!!tr_lag_name:=lag) %>%
  #     lag_group(group_var=.data$.unit, time_var=.data$.time, lag_var=.data$.yvar, lagamount = i) %>%
  #     rename(!!y_lag_name:=lag)
  # }
  # data_treat <- data_treat

  ## compute mean for each seq/time
  out <- data_treat %>%
    group_by_at(vars(one_of(c(".time", "seq")), starts_with(".tr"))) %>%
    summarise_at(vars(starts_with(".yvar")), mean) %>%
    ungroup() %>%
    left_join(data_treat %>%
                count(.data$.time, seq, name = "n_obs"), by = c(".time", "seq"))

  ## long?
  if(format_long){
    out <- DD_manu_many_tolong(out)
  }
  out
}

DD_manu_many_tolong <- function(df) {
  df %>%
    gather("variable", "value", starts_with(".yvar"), starts_with(".treat")) %>%
    mutate(lag = if_else(str_detect(.data$variable, "lag"),
                         str_extract(.data$variable, "[0-9]+$"),
                         "0") %>%
             as.integer,
           variable = str_remove(.data$variable, "_lag[0-9]+")) %>%
    spread(.data$variable, .data$value) %>%
    mutate(actual_time = .data$.time-.data$lag,
           seq_unique = paste(.data$.time, seq, sep=": "))
}


DD_manu_many_diffs <- function(df) {
  lag_max <-  max(df$lag)
  res <- df %>%
    select(".time", "seq", "lag", ".yvar") %>%
    mutate(lag = factor(paste0("lag_", .data$lag),
                        levels = paste0("lag_", rev(unique(.data$lag))))) %>%
    spread(.data$lag, .data$.yvar) %>%
    mutate(diff_l0_l1 = .data$lag_0 -.data$lag_1)

  if(lag_max>1) {
    add_diff <- function(i) {
      var_out <- paste0("diff_l", i, "_l", i-1)
      var_in <- rlang::sym(paste0("lag_", i))
      var_in_minus1 <- rlang::sym(paste0("lag_", i-1))
      dplyr::transmute(res, !!var_out:= !!var_in- !!var_in_minus1)
    }

    res <- res %>%
      bind_cols(purrr::map_dfc(2:lag_max, add_diff))
  }
  res
}

DD_manu_many_dids <- function(df) {
  diffs <- DD_manu_many_diffs(df)
  diffs %>%
    select(".time", "seq", "diff_l0_l1") %>%
    mutate(seq = paste0("seq_", seq) %>%
             str_replace_all("->", "_")) %>%
    filter(!str_detect(seq, "NA")) %>%
    spread(seq, .data$diff_l0_l1) %>%
    mutate(did_01_vs_00 = .data$seq_0_1-.data$seq_0_0,
           did_10_vs_11 = .data$seq_1_0-.data$seq_1_1 )
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


  ## compute Did
  out_w %>%
    mutate(diff_y = .yvar - .yvar_lag1) %>%
    filter(seq%in%c("0->0", "0->1")) %>%
    select(.time, seq, diff_y) %>%
    mutate(seq=str_replace_all(seq, "->", "_")) %>%
    pivot_wider(names_from = seq, values_from=diff_y,
                names_prefix = "seq_") %>%
    mutate(Did = seq_0_1 - seq_0_0)

  ## Check got right
  out_DD <- DD(data = dat_sim_1,
                      y_var = "y",
                      time.index = "Time")
  out_DD %>%
    filter(DiD%in% c(1, 4)) %>%
    select(time, DiD, treat, control, estimate) %>%
    spread(DiD, estimate)

  DD_manu_many_dids(out_l1)


  ## plot
  out_l2 %>%
    filter(seq %in% c("0_0_0", "0_0_1")) %>%
    filter(.time %in% c(3, 5)) %>%
    ggplot(aes(x = actual_time, y = .yvar, color = factor(.time), linetype = seq)) +
    geom_line()

  out

  ##
  DD_manu_many_diffs(df=out_l1)
  DD_manu_many_diffs(out_l2)
  out_l %>%
    # .[c( 19, 20, 21),]
    # select(-n_obs, -actual_time) %>%
    select(.time, seq, lag, .yvar) %>%
    mutate(lag = factor(paste0("lag_", lag),
                        levels = paste0("lag_", rev(unique(lag))))) %>%
    spread(lag, .yvar) %>%
    mutate(diff_l1_l0 = lag_0 -lag_1,
           diff_l2_l1 = lag_1 -lag_2)

}
