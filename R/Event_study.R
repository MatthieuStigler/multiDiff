#' Event study
#'
#' @template param_mdd_dat
#' @param trim_low,trim_high Upper/lower bound on parameters to include
#' @param time.omit Which is the base year omitted in the analysis?
#' @template param_cluster
#' @template param_weights
#' @details
#' This function uses lags of pre/post treatment, omitting the lag -1
#'  (can be changed with `time.omit`).
#'  The interpretation of the event-study coefficient is then the following:
#'  each coefficient represents the value of a DiD relating the treated/control difference
#'  at relative year -3, -2, 1, 2
#'  to the treated/control difference at relative year -1.
#'
#' @examples
#' ## simulate and format data
#' DID_dat <- sim_dat_common(timing_treatment = 5:10, as_mdd = TRUE)
#'
#' ## Estimate DiD
#' mdd_DD_simple(DID_dat)
#'
#' ## estimate ES
#' ES_out <- mdd_event_study(DID_dat)
#' ES_out
#' summary(ES_out)
#' plot(ES_out)
#'
#' ## compare to year-t- base year diff-diffs:
#' means_manu <- DID_dat |>
#'   mdd_group_means() |>
#'   as.data.frame() |>
#'   reshape(idvar = "Time",timevar = ".group",
#'           direction = "wide")
#' diff_by_period <- means_manu$y.treated - means_manu$y.control
#'   all.equal((diff_by_period - diff_by_period[5])[-5],
#'             coef(ES_out), check.attributes= FALSE)
#'@seealso \code{\link{mdd_test_pre_trend_event}} to run a parallel trend assumption
#' @export
mdd_event_study <-  function(mdd_dat,
                             trim_low=NULL, trim_high=NULL, time.omit = -1,
                             weights=NULL, cluster=NULL, ...){


  ## mdd formatting
  if(!inherits(mdd_dat, "mdd_dat")) stop("Data should be formatted with 'mdd_data_format' first ")
  mdd_dat_slot <- intrnl_mdd_get_mdd_slot(mdd_dat)
  mdd_vars <- mdd_dat_slot$var_names

  if(is.character(cluster)) cluster <- as.formula(paste0("~", cluster))

  if(!is.null(trim_low) && trim_low> -1) stop("Argument 'trim_low' should be <= -1")

  # y_var=quo(y)
  # time.index = quo(Time)
  # treat = quo(tr)
  # unit.index <- quo(unit)

  ## data rename
  dat_renamed <- mdd_dat %>%
    intrnl_dat_rename(treat = !!sym(mdd_vars$treat), y_var=!!sym(mdd_vars$y_var),
                      time.index=!!sym(mdd_vars$time.index), unit.index=!!sym(mdd_vars$unit.index))
    # intrnl_add_treat_time()


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
  # lags <- seq(-1*K_before, K_after, by=1) %>% purrr::discard(~.%in% c(0, -1))

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
  res <- fixest::feols(as.formula(formu), data =data_aug, weights = weights, cluster = cluster, ...)


  ## format result
  class(res) <- c("mdd_event_study", class(res))
  res$mdd_dat_slot <- mdd_dat_slot
  res$event_slot <- list(time.omit=time.omit)
  res
}

#'
#' @param x the ES object
#' @param ... passed to feols for mdd_event_study
#' @rdname mdd_event_study
#' @export
plot.mdd_event_study <- function(x, ...){

  ## construct data
  coef_df <- broom::tidy(x, conf.int=TRUE) %>%
    tibble::add_row(term = paste0("timing_to_treat", x$event_slot$time.omit),
            estimate=0,
            conf.low=NA, conf.high=NA) %>%
    mutate(time = str_extract(.data$term, "-?[0-9]+") %>% as.integer)

  coef_df %>%
    ggplot(aes(x=.data$time, y=.data$estimate))+
    ggplot2::geom_ribbon(aes(ymin=.data$conf.low, ymax=.data$conf.high),
                color="grey", alpha=0.8)+
    ggplot2::geom_point()

}

