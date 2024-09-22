utl_char_to_num <- function(x) match(x, unique(x))

if(FALSE){
  utl_char_to_num(rep(LETTERS[1:5], each=3))
}


#' Apply Callaway and Sant'Anna (2021) estimate
#'
#' Simple wrapper to function \code{\link[did]{att_gt}} from package did.
#' @template param_mdd_dat
#' @param ... further objects passed to \code{\link[did]{att_gt}}
#' @returns Output of \code{\link[did]{att_gt}}, therefore of class \code{\link[did]{MP}}.
#' @examples
#' dat <- sim_dat_staggered(as_mdd = TRUE, Time=6)
#' mdd_CS(dat)
#'
#' ## with a single treatment time, this is equivalent to mdd_event_study()
#' dat_common <- sim_dat_common(as_mdd = TRUE, timing_treatment = 5:10)
#' all.equal(unname(coef(mdd_event_study(dat_common))),
#'           broom::tidy(mdd_CS(dat_common, base_period = "universal"))$estimate[-4])
#' @export
mdd_CS <- function(mdd_dat, ...){

  requireNamespace("did")
  vars <- intrnl_mdd_get_mdd_slot(mdd_dat)$var_names

  ## conv to did format
  dat_did <- mdd_conv_mdd_to_did(mdd_dat)
    # mutate(treat_timing2= if_else(.data$treat_timing>0, min(.[[vars$time.index]])+.data$treat_timing, 0))

  ## make unit.index (idname) numeric
  if(!is.numeric(dat_did[[vars$unit.index]])){
    nam <- vars$unit.index
    dat_did <- dat_did %>%
      mutate({{nam}} := utl_char_to_num(vars$unit.index))
  }

  ## now run
  did::att_gt(yname = vars$y_var, tname = vars$time.index, idname = vars$unit.index,
              gname = "treat_timing",
              data = dat_did,
              ...)
}

if(FALSE){
  dat <- sim_dat_staggered(as_mdd = TRUE, Time=6)
  dat
  environment(mdd_CS) <- environment(mdd_data_format)
  mdd_CS(dat)

}

################################
#'## Manual
################################

mdd_CS_manu <- function(mdd_dat, control_group = c("nevertreated", "notyettreated")){

  if(!inherits(mdd_dat, "mdd_dat")) stop("Data should be formatted with 'mdd_data_format' first ")
  mdd_dat_slot <- multiDiff:::intrnl_mdd_get_mdd_slot(mdd_dat)
  mdd_vars <- mdd_dat_slot$var_names
  if(mdd_dat_slot$DID_type!="staggered") warning("Not staggered?")
  control_group <- match.arg(control_group)

  ## add treat_timing
  data2 <- mdd_dat %>%
    multiDiff:::intrnl_add_treat_time_mdd() %>%
    # mutate(across(c("treat_timing_num", "treat_timing"), ~if_else(.==0, Inf, .))) %>%
    select(!!mdd_vars$y_var, !!mdd_vars$time.index, !!mdd_vars$unit.index, "treat_timing")
  # data2 %>%
  #   distinct(treat_timing_num, treat_timing)

  ## get timings
  periods <- mdd_dat_slot$periods
  treated_periods <- mdd_dat_slot$treated_periods
  untreated_periods <- periods[!periods%in% treated_periods]

  timing_df <- tidyr::expand_grid(group=treated_periods,
                                  time=tail(sort(periods),-1))

  ## run for each
  data2 %>%
    count(treat_timing)
  dat_1 <- mdd_CS_manu_prep_1(data2, time_treat = 2003, group_treat = 2007,
                              mdd_dat_slot=mdd_dat_slot)
  mdd_DD_simple(dat_1)
  dat_1 %>%
    as.data.frame() %>%
    count(tr, treat_timing, Year)
  timing_df %>%
    # slice(6) %>%
    mutate(dat_here = map2(group, time, ~mdd_CS_manu_prep_1(data2,
                                                            group_treat = .x,
                                                            time_treat = .y,
                                                            mdd_dat_slot=mdd_dat_slot)),
           dd = map(dat_here, mdd_DD_simple),
           dd_coef = map(dd, tidy)) %>%
    tidyr::unnest(dd_coef) %>%
    select(-dat_here)
}

mdd_CS_manu_prep_1 <- function(data, time_treat, group_treat,
                               mdd_dat_slot, time_var, treat_var,
                               control_group = c("nevertreated", "notyettreated"),
                               keep_mdd=TRUE){

  ## check args
  control_group <- match.arg(control_group)
  mdd_vars <- mdd_dat_slot$var_names
  periods <- mdd_dat_slot$periods

  ## select pre -period
  if(group_treat<=time_treat){
    pre_period <- periods[which(periods==group_treat)-1]
  } else {
    ## for placebo
    pre_period <- periods[which(periods==time_treat)-1]
  }

  dat_out <- data %>%
    ## select periods
    # filter(Year %in% c(time_treat, last_untreated)) %>%
    filter(!!sym(mdd_vars$time.index) %in% c(time_treat, pre_period)) %>%
    ## select group
    filter(.data$treat_timing %in% c(group_treat, 0)) %>%
    ## create dummy
    mutate(!!sym(mdd_vars$treat) := if_else(.data$treat_timing==group_treat & !!sym(mdd_vars$time.index) ==time_treat, 1, 0))

  if(keep_mdd) dat_out <- multiDiff:::intrnl_back_to_mdd(dat_out, mdd_dat_slot$var_names)
  dat_out
}

if(FALSE){
  dat_raw <- sim_dat_staggered(as_mdd = FALSE, Time=6) |>
    dplyr::left_join(tibble::tibble(Time=1:6, Year = c(2000, 2003, 2007:2010)), by="Time") |>
    dplyr::select(unit, Year, tr, y)

  mdd_dat <- mdd_data_format(dat_raw, time.index = "Year")
  mdd_dat


  environment(mdd_CS) <- environment(mdd_DD_simple)
  CS_mine <- mdd_CS_manu(mdd_dat)
  CS_CS <- mdd_CS(mdd_dat)

  all.equal(CS_mine %>%
              select(group, time, estimate),
            tidy(CS_CS) %>%
              select(group, time, estimate) %>%
              as_tibble())

}
