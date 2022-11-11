#' Estimate simple did
#'
#' Standard two-way FE estimation of a DiD
#' @template param_mdd_dat
#' @template param_weights
#' @template param_cluster
#' @examples
#' ## simulate and format data
#' DID_dat_raw <- sim_dat_common(timing_treatment = 5:10)
#' DID_dat <- mdd_data_format(DID_dat_raw)
#'
#' ## Estimate DiD
#' mdd_DD_simple(DID_dat)
#' @seealso \code{\link{mdd_event_study}} for the event study.
#' @export
mdd_DD_simple <-  function(mdd_dat, weights = NULL, cluster = NULL){

  if(is.character(cluster)) cluster <- as.formula(paste0("~", cluster))

  ## mdd formatting
  if(!inherits(mdd_dat, "mdd_dat")) stop("Data should be formatted with 'mdd_data_format' first ")
  mdd_vars <- intrnl_mdd_get_mdd_slot(mdd_dat)$var_names

  formu <- paste0(mdd_vars$y_var, " ~ ",
                  mdd_vars$treat, " | ",
                  mdd_vars$unit.index, " + ",
                  mdd_vars$time.index)

  res <- fixest::feols(as.formula(formu), data =mdd_dat, weights = weights, cluster = cluster)

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
#' @template param_mdd_dat
#' @param trim_low,trim_high Upper/lower bound on parameters to include
#' @param time.omit Which is the base year omitted in the analysis?
#' @template param_cluster
#' @template param_weights
#' @examples
#' ## simulate and format data
#' DID_dat_raw <- sim_dat_common(timing_treatment = 5:10)
#' DID_dat <- mdd_data_format(DID_dat_raw)
#'
#' ## Estimate DiD
#' mdd_DD_simple(DID_dat)
#'
#' ## estimate ES
#' ES_out <- mdd_event_study(DID_dat)
#' ES_out
#' summary(ES_out)
#' plot(ES_out)
#'@seealso \code{\link{mdd_test_pre_trend_event}} to run a parallel trend assumption
#' @export
mdd_event_study <-  function(mdd_dat,
                             trim_low=NULL, trim_high=NULL, time.omit = -1,
                             weights=NULL, cluster=NULL){


  ## mdd formatting
  if(!inherits(mdd_dat, "mdd_dat")) stop("Data should be formatted with 'mdd_data_format' first ")
  mdd_dat_slot <- intrnl_mdd_get_mdd_slot(mdd_dat)
  mdd_vars <- mdd_dat_slot$var_names

  if(is.character(cluster)) cluster <- as.formula(paste0("~", cluster))

  # y_var=quo(y)
  # time.index = quo(Time)
  # treat = quo(tr)
  # unit.index <- quo(unit)

  ## data rename
  dat_renamed <- mdd_dat %>%
    intrnl_dat_rename(treat = !!sym(mdd_vars$treat), y_var=!!sym(mdd_vars$y_var),
                      time.index=!!sym(mdd_vars$time.index), unit.index=!!sym(mdd_vars$unit.index)) %>%
    intrnl_add_treat_time()


  ## Prep leads and lags
  T_after <-  dat_renamed %>%
    distinct(.data$time.index, .data$treat) %>%
    tidyr::pivot_wider(names_from = "treat", values_from = "treat", names_prefix = "status_") %>%
    arrange(.data$time.index)
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
    group_by(.data$unit.index) %>%
    mutate(timing_to_treat = x_time_to_treat(.data$treat, trim_low=trim_low, trim_high=trim_high)) %>%
    ungroup() %>%
    mutate(timing_to_treat =  relevel(factor(.data$timing_to_treat), as.character(time.omit)))


  ## factor way
  formu <- "y_var ~ timing_to_treat |unit.index + time.index"

  ### lead/lag way
  res <- fixest::feols(as.formula(formu), data =data_aug, weights = weights, cluster = cluster)


  ## format result
  class(res) <- c("mdd_event_study", class(res))
  res$mdd_dat_slot <- mdd_dat_slot
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

