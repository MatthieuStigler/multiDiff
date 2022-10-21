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

  ## add leads and lags
  T_after <-  data %>% distinct({{time.index}}, {{treat}}) %>%
    spread(tr, tr)
  data_aug <- data |>
    lag_group(group_var={{unit.index}}, value_var="y", time_var={{time.index}})

  lags <- seq(-1*lag, lag, by=1) %>% discard(~.==0)

  data_aug <- data |>
    lag_group(group_var=rlang::as_name(unit.index), value_var="y", time_var="Time",
              lag= lags)

  formu <- paste0(rlang::as_string(y_var), " ~ ",
                  rlang::as_string(treat), " | ",
                  rlang::as_string(time.index), " + ",
                  rlang::as_string(unit.index))

  lfe::felm(as.formula(formu), data =data)
}

dd_event(data=DID_dat)
