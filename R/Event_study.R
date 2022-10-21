#' Estimate simple did
#'
#' @template param_all
DD <-  function(data, y_var="y", time.index = "Time", treat = "tr", unit.index="unit"){
  formu <- paste0(rlang::as_string(y_var), " ~ ",
                  rlang::as_string(treat), " | ",
                  rlang::as_string(time.index), " + ",
                  rlang::as_string(unit.index))

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
dd_event <-  function(data, y_var="y", time.index = "Time", treat = "tr", unit.index="unit",
                      lag = 3){

  y_var=quo(y)
  time.index = quo(Time)
  treat = quo(tr)
  unit.index <- quo(unit)

  ## data rename
  dat_renamed <- data %>%
    intrnl_dat_rename("treat" ={{treat}}) %>%
    intrnl_add_treat_time()


  ## Prep leads and lags
  T_after <-  data %>% distinct({{time.index}}, {{treat}}) %>%
    pivot_wider(names_from = "tr", values_from = "tr", names_prefix = "status_") %>%
    arrange(Time)
  first_treat <- filter(T_after, status_1==1)[1,"Time", drop=TRUE]
  K_after <- sum(T_after$Time>first_treat)
  K_before <- sum(T_after$Time<first_treat)


  ## add leads and lags
  lags <- seq(-1*(K_before-1), K_after-1, by=1) %>% discard(~.%in% c(0, -1))
  # lags <- c(-2,1, 2)
  data_aug <- dat_renamed |>
    lag_group(group_var= "unit.index", value_var="treat_timing", time_var="time.index",
              lagamount = lags) %>%
    mutate(across(starts_with("treat_timing_l"), ~replace_na(., 0))) %>%
    rename_with(~str_replace(., "lead", "before") %>%
                  str_replace("lag", "after"))

  ## cosntruct formu
  vars_timing <- colnames(data_aug) %>% keep(~str_detect(., "treat_timing"))
  formu <- paste0("y_var ~ ",
                  paste(vars_timing, collapse = " + "),
                  " | time.index + unit.index")

  lfe::felm(as.formula(formu), data =data_aug)
}


if(FALSE){


  data <- multiDiff::sim_dat_staggered(N=100, perc_always = 0,
                                       Time=6,
                                       timing_treatment = 4, perc_treat=0.5)
}
dd_event(data=DID_dat)
