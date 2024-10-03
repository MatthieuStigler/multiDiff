utl_char_to_num <- function(x) match(x, unique(x))

if(FALSE){
  utl_char_to_num(rep(LETTERS[1:5], each=3))
}


#' Apply Callaway and Sant'Anna (2021) estimate
#'
#' Simple wrapper to function \code{\link[did]{att_gt}} from package did.
#' @template param_mdd_dat
#' @param timing_treat_var variable indicating the timing to treat. Not necessary, unless the dataset is not balanced
#' @param panel Argument \code{panel} to \code{\link[did]{att_gt}}. If not specified, will try to guess.
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
mdd_CS <- function(mdd_dat, timing_treat_var=NULL, panel=NULL, ...){

  requireNamespace("did")
  vars <- intrnl_mdd_get_mdd_slot(mdd_dat)$var_names

  ## conv to did format
  ## add treat_timing if not there
  if(is.null(timing_treat_var)) {
    dat_did <- mdd_conv_mdd_to_did(mdd_dat)
  } else {
    dat_did <- mdd_dat %>%
      as.data.frame() %>%
      rename(treat_timing= !!sym(timing_treat_var)) %>%
      mutate(treat_timing= if_else(.data$treat_timing==Inf, 0, .data$treat_timing))
  }

  ## make unit.index (idname) numeric
  if(!is.numeric(dat_did[[vars$unit.index]])){
    nam <- vars$unit.index
    dat_did <- dat_did %>%
      mutate({{nam}} := utl_char_to_num(vars$unit.index))
  }

  ## cross-section?
  if(is.null(panel)){
    is_cross_sec <- intrnl_mdd_get_mdd_slot(mdd_dat)$is_cross_sec
    panel <- !is_cross_sec
  }

  ## now run
  did::att_gt(yname = vars$y_var, tname = vars$time.index, idname = vars$unit.index,
              gname = "treat_timing",
              data = dat_did,
              panel = panel,
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

#' @noRd
mdd_CS_manu <- function(mdd_dat, control_group = c("nevertreated", "notyettreated"),
                        timing_treat_var = NULL, keep_raw_data=FALSE, cluster=NULL){

  if(!inherits(mdd_dat, "mdd_dat")) stop("Data should be formatted with 'mdd_data_format' first ")
  mdd_dat_slot <- intrnl_mdd_get_mdd_slot(mdd_dat)
  mdd_vars <- mdd_dat_slot$var_names
  if(mdd_dat_slot$DID_type!="staggered") warning("Not staggered?")
  control_group <- match.arg(control_group)

  ## cross-section?
  is_cross_sec <- intrnl_mdd_get_mdd_slot(mdd_dat)$is_cross_sec

  ## add treat_timing if not there
  if(is.null(timing_treat_var)) {
    data_with_treat_timing <-  mdd_dat %>%
      intrnl_add_treat_time_mdd()
  } else {
    data_with_treat_timing <- mdd_dat %>%
      as_tibble() %>%
      rename(treat_timing= !!sym(timing_treat_var)) %>%
      mutate(treat_timing= if_else(.data$treat_timing==Inf, 0, .data$treat_timing))
  }
  data_with_treat_timing <- data_with_treat_timing %>%
    select(!!mdd_vars$y_var, !!mdd_vars$time.index, !!mdd_vars$unit.index, "treat_timing") %>%
    ## groups with treatment timing larger than max time considered as never treated
    mutate(treat_timing = if_else(.data$treat_timing>max(!!sym(mdd_vars$time.index)), 0, .data$treat_timing))

  ## get timings
  periods <- sort(mdd_dat_slot$periods)
  treated_periods <- sort(unique(c(mdd_dat_slot$treated_periods, unique(data_with_treat_timing$treat_timing))))
  treated_periods <- treated_periods[treated_periods!=0 & treated_periods>min(periods)]
  untreated_periods <- periods[!periods%in% treated_periods]
  if(length(untreated_periods)==0) untreated_periods <- NA

  timing_df <- tidyr::expand_grid(group=treated_periods,
                                  time=periods[!periods %in% min(untreated_periods)])

  ## need to cast back to mdd, but with different style
  back_to_md <- function(data, is_cross){
    if(nrow(data)==0) return(NA)
    if(!is_cross_sec){
      res <- intrnl_back_to_mdd(data, mdd_vars)
    } else {
      mdd_vars2 <- mdd_vars
      mdd_vars2$unit.index <-"group_treat_fe"
      res <- intrnl_back_to_mdd(data, mdd_vars2)
    }
    res
  }

  ## test 1
  # dat_TOY <- mdd_CS_manu_prep_1(data_with_treat_timing,
  #                               group_treat = timing_df$group[[3]],
  #                               time_treat = timing_df$time[[3]],
  #                               mdd_dat_slot=mdd_dat_slot,
  #                               keep_mdd = FALSE,
  #                               is_cross_sec=is_cross_sec,
  #                               control_group=control_group) %>%
  #   back_to_md()
  #
  # dat_TOY %>% as_tibble() %>% count(Time, treat_timing, tr)



  ## run for each
  res <- timing_df %>%
    mutate(data_mdd = map2(.data$group, .data$time,
                           ~mdd_CS_manu_prep_1(data_with_treat_timing,
                                               group_treat = .x,
                                               time_treat = .y,
                                               mdd_dat_slot=mdd_dat_slot,
                                               control_group =control_group,
                                               keep_mdd = FALSE,
                                               is_cross_sec=is_cross_sec) %>%
                             back_to_md())) %>%
    ## remove data not fit for estimation
    filter(!is.na(.data$data_mdd)) %>%
    ## estimate now
    mutate(dd = map(.data$data_mdd, ~mdd_DD_simple(., cluster=cluster)),
           dd_coef = map(.data$dd, tidy)) %>%
    tidyr::unnest("dd_coef") %>%
    mutate(term = paste0("ATT(", .data$group, ",", .data$time, ")")) %>%
    relocate("term")

  ## remove raw data
  if(!keep_raw_data){
    res <- res %>%
      select(-"data_mdd")
  }
  res
}

mdd_CS_manu_prep_1 <- function(data_with_treat_timing, time_treat, group_treat,
                               mdd_dat_slot, time_var, treat_var,
                               control_group = c("nevertreated", "notyettreated"),
                               keep_mdd=TRUE, is_cross_sec=FALSE){

  ## check args
  control_group <- match.arg(control_group)
  mdd_vars <- mdd_dat_slot$var_names
  periods <- sort(mdd_dat_slot$periods)

  # determine type, this will influence control periods/groups
  type <- if_else(group_treat<=time_treat, "effect", "placebo")

  ## select last pre-period
  if(type=="effect"){
    ## for effect: last pre-period before group's treatment time
    pre_period <- periods[which(periods<group_treat) %>% tail(1)]
  } else {
    ## for placebo: last pre-period before desired time
    pre_period <- periods[which(periods<time_treat) %>% tail(1)]
  }
  # if(length(pre_period)==0) return(head(data_with_treat_timing,0))

  ## select control groups
  if(control_group=="nevertreated"){
    control_group_here <- 0
  } else{
    all_groups <- unique(data_with_treat_timing$treat_timing)
    ## use later groups, but only if not already treated
    if(type=="effect"){
      later_treated <- all_groups[all_groups>group_treat & all_groups >time_treat]
    } else {
      ## use all groups not treated
      later_treated <- all_groups[all_groups>time_treat]
    }
    control_group_here <- c(0, later_treated)
  }


  dat_out <- data_with_treat_timing %>%
    ## select periods
    filter(!!sym(mdd_vars$time.index) %in% c(time_treat, pre_period)) %>%
    ## select groups
    filter(.data$treat_timing %in% c(group_treat, control_group_here)) %>%
    ## create dummy
    mutate(!!sym(mdd_vars$treat) := if_else(.data$treat_timing==group_treat & !!sym(mdd_vars$time.index) ==time_treat,
                                            1,
                                            0))


  ## make sure estimable!?
  gr_count <- dat_out %>%
    count(.data$treat_timing, !!sym(mdd_vars$treat), !!sym(mdd_vars$time.index))
  treated_has_2 <- n_distinct(filter(gr_count, .data$treat_timing==group_treat) %>% pull(!!sym(mdd_vars$time.index)))==2
  control_has_2 <- n_distinct(filter(gr_count, .data$treat_timing!=group_treat)%>% pull(!!sym(mdd_vars$time.index)))==2
  if(!treated_has_2|!control_has_2) return(head(dat_out, 0))

  if(is_cross_sec) dat_out <- dat_out %>%
    mutate(group_treat_fe = if_else(.data$treat_timing == group_treat, "treat", "ctrl"))
  if(keep_mdd) dat_out <- intrnl_back_to_mdd(dat_out, mdd_dat_slot$var_names)
  dat_out
}

if(FALSE){
  dat_raw <- sim_dat_staggered(as_mdd = FALSE, Time=6) |>
    dplyr::left_join(tibble::tibble(Time=1:6, Year = c(2000, 2003, 2007:2010)), by="Time") |>
    dplyr::select(unit, Year, tr, y)

  mdd_dat <- mdd_data_format(dat_raw, time.index = "Year")
  mdd_dat


  environment(mdd_CS) <- environment(mdd_DD_simple)
  environment(mdd_CS_manu) <- environment(mdd_DD_simple)
  CS_mine <- mdd_CS_manu(mdd_dat)
  CS_CS <- mdd_CS(mdd_dat)

  CS_mine
  tidy(CS_CS) %>% as_tibble()


  all.equal(CS_mine %>%
              select(term, group, time, estimate),
            tidy(CS_CS) %>%
              select(term, group, time, estimate) %>%
              as_tibble())

}
