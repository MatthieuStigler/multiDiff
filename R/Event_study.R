#' Estimate simple did
#'
#' @template param_all
#' @export
mdd_DD_simple <-  function(data, y_var="y", time.index = "Time", treat = "tr", unit.index="unit"){
  formu <- paste0(rlang::as_name(y_var), " ~ ",
                  rlang::as_name(treat), " | ",
                  rlang::as_name(time.index), " + ",
                  rlang::as_name(unit.index))

  res <- lfe::felm(as.formula(formu), data =data)

  ## format result
  class(res) <- c("mdd_DiD", class(res))
  res
}

if(FALSE){

  DID_dat <- multiDiff::sim_dat_staggered(N=100, perc_always = 0, Time=10,
                                          timing_treatment = 6, perc_treat=0.5)
  DD(data=DID_dat)
}


#' Event study
#'
#' @template param_all
#' @param trim_low,trim_high Upper/lower bound on parameters to include
#' @param time.omit Which is the base year omitted in the analysis?
#' @param weights Variable containing the weights
#' @export
mdd_event_study <-  function(data, y_var="y", time.index = "Time", treat = "tr", unit.index="unit",
                                   trim_low=NULL, trim_high=NULL, time.omit = -1, weights=NULL){

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
    tidyr::pivot_wider(names_from = "treat", values_from = "treat", names_prefix = "status_") %>%
    arrange(time.index)
  first_treat <- filter(T_after, .data$status_1==1)[1,"time.index", drop=TRUE]
  K_after <- sum(T_after$time.index>first_treat)
  K_before <- sum(T_after$time.index<first_treat)


  ## add leads and lags
  # lags <- c(-2,1, 2)
  lags <- seq(-1*K_before, K_after, by=1) %>% purrr::discard(~.%in% c(0, -1))

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
    mutate(timing_to_treat =  relevel(factor(.data$timing_to_treat), as.character(time.omit)))


  ## factor way
  formu <- "y_var ~ timing_to_treat |time.index+  unit.index"

  ### lead/lag way
  res <- lfe::felm(as.formula(formu), data =data_aug, weights = weights)
  class(res) <- c("mdd_event_study", class(res))
  res$event_slot <- list(time.omit=time.omit)
  res
}

#'
#' @param x the ES object
#' @param ... currently not used
#' @rdname mdd_event_study
#' @export
plot.mdd_event_study <- function(x, ...){

  ## construct data
  coef_df <- broom::tidy(x, conf.int=TRUE) %>%
    tibble::add_row(term = paste0("timing_to_treat", x$event_slot$time.omit),
            estimate=0,
            conf.low=0, conf.high=0) %>%
    mutate(time = str_extract(.data$term, "-?[0-9]+") %>% as.integer)

  coef_df %>%
    ggplot(aes(x=.data$time, y=.data$estimate))+
    ggplot2::geom_ribbon(aes(ymin=.data$conf.low, ymax=.data$conf.high),
                color="grey", alpha=0.8)+
    ggplot2::geom_point()

}

