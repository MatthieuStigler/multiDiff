#' Estimate simple did
#'
#' @template param_all
mdd_mdd_DD_simple <-  function(data, y_var="y", time.index = "Time", treat = "tr", unit.index="unit"){
  formu <- paste0(rlang::as_name(y_var), " ~ ",
                  rlang::as_name(treat), " | ",
                  rlang::as_name(time.index), " + ",
                  rlang::as_name(unit.index))

  lfe::felm(as.formula(formu), data =data)
}

if(FALSE){

  DID_dat <- multiDiff::sim_dat_staggered(N=100, perc_always = 0, Time=10,
                                          timing_treatment = 6, perc_treat=0.5)
  DD(data=DID_dat)
}

add_treat_group <- function(data, time.index = "Time", treat = "tr", unit.index="unit"){
  data %>%
    group_by({{unit.index}}) %>%
    mutate(treat_group = if_else(any({{treat}}==1), "treated", "control"))
}



#' Event study
mdd_event_study_study <-  function(data, y_var="y", time.index = "Time", treat = "tr", unit.index="unit",
                      lag = 3, trim_low=NULL, trim_high=NULL){

  # y_var=quo(y)
  # time.index = quo(Time)
  # treat = quo(tr)
  # unit.index <- quo(unit)

  ## data rename
  dat_renamed <- data %>%
    intrnl_dat_rename(treat = !!sym(treat), y_var=!!sym(y_var),
                      time.index=!!sym(time.index), unit.index=!!sym(unit.index)) %>%
    intrnl_add_treat_time()


  ## Prep leads and lags
  T_after <-  dat_renamed %>% distinct(time.index, treat) %>%
    pivot_wider(names_from = "treat", values_from = "treat", names_prefix = "status_") %>%
    arrange(time.index)
  first_treat <- filter(T_after, status_1==1)[1,"time.index", drop=TRUE]
  K_after <- sum(T_after$time.index>first_treat)
  K_before <- sum(T_after$time.index<first_treat)


  ## add leads and lags
  # lags <- c(-2,1, 2)
  lags <- seq(-1*K_before, K_after, by=1) %>% discard(~.%in% c(0, -1))

  ## way A: leads and lags
  # data_aug <- dat_renamed |>
  #   lag_group(group_var= "unit.index", value_var="treat_timing", time_var="time.index",
  #             lagamount = lags) %>%
  #   mutate(across(starts_with("treat_timing_l"), ~replace_na(., 0))) %>%
  #   rename_with(~str_replace(., "lead", "before") %>%
  #                 str_replace("lag", "after"))
  #
  # ## cosntruct formu
  # vars_timing <- colnames(data_aug) %>% keep(~str_detect(., "treat_timing"))
  # formu <- paste0("y_var ~ ",
  #                 paste(vars_timing, collapse = " + "),
  #                 " |  unit.index")

   ## Way B: time to treat
  data_aug <- dat_renamed |>
    group_by(unit.index) %>%
    mutate(timing_to_treat = x_time_to_treat(treat, trim_low=trim_low, trim_high=trim_high)) %>%
    ungroup() %>%
    mutate(timing_to_treat =  fct_relevel(factor(timing_to_treat), "-1"))


  ## factor way
  formu <- "y_var ~ timing_to_treat |time.index+  unit.index"

  ### lead/lag way
  lfe::felm(as.formula(formu), data =data_aug)
}


if(FALSE){
  library(multiDiff)
  source("R/utilities.R")
  library(tidyverse)

  DID_dat <- multiDiff::sim_dat_staggered(N=5000, perc_always = 0,
                                          Time=8,
                                          beta=1.1,
                                          timing_treatment = 6, perc_treat=0.5)

  ## Manu means
  means_manu <- DID_dat %>%
    intrnl_dat_rename() %>%
    intrnl_add_treat_status() %>%
    group_by(treat_categ, time.index) %>%
    summarise(mean= mean(y_var)) %>%
    spread(treat_categ, mean) %>%
    mutate(diff=Treat-Control)

  means_manu

  ## Did simple full
  coef(mdd_DD_simple(DID_dat))

  ## Did simple: only 2Y
  diff(means_manu[c(5,6), "diff", drop=TRUE])
  coef(mdd_DD_simple(data=DID_dat %>% filter(Time %in% c(5, 6))))

  ## event
  ES <- mdd_event_study(data=DID_dat)
  summary(ES)

  ## Those are numerically equal to diff-diffs!
  all.equal(coef(ES),
            (means_manu[, "diff", drop=TRUE]-means_manu[c(5), "diff", drop=TRUE])[-5], check.attributes=FALSE)


  all.equal(coef(ES)["timing_to_treat0"],
            coef(mdd_DD_simple(data=DID_dat %>% filter(Time %in% c(5, 6)))), check.attributes=FALSE)


  mdd_event_study(data=DID_dat)
  mdd_event_study(data=DID_dat, trim_high = 1)
  mdd_event_study(data=DID_dat, trim_high = 0)
  mdd_event_study(data=DID_dat, trim_high = 0, trim_low = -3)
}
