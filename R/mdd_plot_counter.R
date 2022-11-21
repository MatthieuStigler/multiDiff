#' Plot couterfactual means
#'
#' @noRd
mdd_plot_counter <- function(mdd_DiD, mdd_dat){


  ## data formatting
  mdd_dat_slot <- intrnl_mdd_get_mdd_slot(mdd_dat)
  mdd_vars <- mdd_dat_slot$var_names

  ## empirical means
  means_empi <- mdd_dat %>%
    mutate(!!sym(mdd_vars$y_var) :=stats::fitted(mdd_DiD)) %>%
    mdd_group_means()

  ## counterfactual means
  dat_counter <- mdd_dat %>%
    # as_tibble() %>%
    filter(!!sym(mdd_vars$treat)==1) %>%
    mutate(across(mdd_vars$treat, ~0))
  pred_counter <- stats::predict(mdd_DiD, newdata=dat_counter)

  means_counter <- dat_counter %>%
    mutate(!!sym(mdd_vars$y_var) :=pred_counter) %>%
    mdd_group_means() %>%
    mutate(.group = "treated")

  if(n_distinct(means_empi$.group)>2){
    rlang::abort("mdd_plot_counter not working for more than 2 groups for now")
  }


  ## bind means
  df <- rbind(means_empi %>%
                mutate(type="empirical"),
              means_counter %>%
                mutate(type="counterfactual"))
  class(df) <- c("mdd_counter_means", class(df))
  attr(df, "mdd_dat_slot") <- mdd_dat_slot
  df
}

#' Plot
#' @noRd
plot.mdd_counter_means <- function(x, ...){

  ## data formatting
  mdd_dat_slot <- intrnl_mdd_get_mdd_slot(x)
  mdd_vars <- mdd_dat_slot$var_names

  ## add last
  last_period_untreat <- tail(mdd_dat_slot$periods[!mdd_dat_slot$periods %in% mdd_dat_slot$treated_periods], 1)
  tr_last <- x %>%
    filter(.data$.group=="treated" & !!sym(mdd_vars$time.index) ==last_period_untreat) %>%
    mutate(type="counterfactual")

  x %>%
    bind_rows(tr_last) %>%
    mutate(type = relevel(.data$type, "empirical")) %>%
    ggplot(aes(x = !!sym(mdd_vars$time.index),
               y = !!sym(mdd_vars$y_var),
               color = .data$.group,
               linetype=.data$type))+
    ggplot2::geom_line()
}

if(FALSE){

  # library(multiDiff)
  # set.seed(123)
  # DID_dat <- sim_dat_common(timing_treatment = 6:8, Time = 8) %>%
  #   mutate(y= if_else(treat_group=="treated", y+0.3, y))
  #
  # DID_dat_mdd <- mdd_data_format(DID_dat)
  #
  # ## Estimate DiD
  # DiD <- mdd_DD_simple(mdd_dat=DID_dat_mdd)
  #
  # ##HERE
  # emp_counter <- mdd_plot_counter(mdd_DiD=DiD, mdd_dat=DID_dat_mdd)
  # emp_counter %>%
  #   filter(.data$.group=="treated") %>%
  #   spread(.data$type, y) %>%
  #   mutate(diff=counterfactual-empirical)
  # emp_counter|>
  #   subset(Time %in% 6:8)
  #
  # plot(x=emp_counter)
  # plot(DID_dat_mdd)


}
