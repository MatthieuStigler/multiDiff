#' Diff Diff manual table
#'
#' @template param_all
#' @export
#' @examples
#' dat_DiD_raw <- sim_dat_common()
#' dat_DiD <- mdd_data_format(dat_DiD_raw)
mdd_data_format <-  function(data, y_var="y", time.index = "Time", treat = "tr", unit.index="unit"){

  ## Check vars
  if(!all(c(y_var, time.index, treat, unit.index) %in% colnames(data))){
    which_miss_num <- which(!c(y_var, time.index, treat, unit.index) %in% colnames(data))
    which_miss <- paste(c(y_var, time.index, treat, unit.index)[which_miss_num], collapse = ", ")
    stop("Variable(s): ", which_miss, " not in data?")
  }

  ## check if cross-section?
  T_periods <- n_distinct(data[[time.index]])
  n_obs_by_unit_by_time <- intrnl_n_obs_by_unit_by_time(data, unit.index = unit.index, time.index = time.index)
  n_obs_by_unit <- intrnl_n_obs_by_unit(data, unit.index = unit.index)
  if(all(n_obs_by_unit==1)) warning("Only one observation by unit? Might need to change argument `unit.index`?")
  is_cross_sec <- all(n_obs_by_unit_by_time > 1)

  ##
  sequences <- get_sequences(data, time.index = {{time.index}},
                             treat = {{treat}}, unit.index = {{unit.index}},
                             is_cross_sec = is_cross_sec) %>%
    rename(.group="seq")
  seq_uniques <- unique(sequences$.group)


  ## Compute various info
  is_reversible <- any(str_detect(seq_uniques, "1_0"))
  n_units <- nrow(sequences)
  n_treated <- sum(str_detect(sequences$.group, "1"))
  n_seq <- length(seq_uniques)
  treated_periods_num <- map(str_split(seq_uniques, "_"), ~which(.=="1")) %>%
    unlist() %>% unique() %>% sort()
  periods <- unique(pull(data, {{time.index}}))
  treated_periods <- periods[treated_periods_num]


  ## classify time
  DID_type <- case_when(n_seq==2 & !is_reversible ~ "classical",
                        n_seq==2 & is_reversible ~ "classical-single time",
                        n_seq>2 & !is_reversible ~ "staggered",
                        n_seq>2 & is_reversible ~ "general",
                        TRUE ~"UNCLASSFIED SCHEME YET?")

  ## put all info together
  vars <- list(y_var=rlang::as_name(enquo(y_var)),
               time.index=rlang::as_name(enquo(time.index)),
               treat=rlang::as_name(enquo(treat)),
               unit.index=rlang::as_name(enquo(unit.index)))
  mdd_dat_slot <- list(n_units = n_units,
                       n_seq = n_seq,
                       n_treated = n_treated,
                       is_reversible = is_reversible,
                       treated_periods = treated_periods,
                       treated_periods_num = treated_periods_num,
                       periods = periods,
                       var_names = vars,
                       DID_type = DID_type)

  ## add to object
  class(data) <- c("mdd_dat", class(data))
  attr(data, "mdd_dat_slot") <- mdd_dat_slot
  data
}

#' @param x object of class mdd_dat
#' @param ... unused
#' @export
#' @rdname mdd_data_format
print.mdd_dat <- function(x, ...){

  mdd_dat_slot <- attributes(x)$mdd_dat_slot

  ## Printing of treated periods
  treated_periods_clean <- mdd_dat_slot$treated_periods
  is_regular_seq <- all(diff(treated_periods_clean)==1)
  expected_seq <- min(treated_periods_clean):max(treated_periods_clean)
  if(is_regular_seq  && all(treated_periods_clean== expected_seq)) {
    treated_periods_print <- paste(range(treated_periods_clean), collapse = ":")
  } else {
    treated_periods_print <- treated_periods_clean
  }

  cat("### MDD data\n")
  cat("\t-Design type: ", mdd_dat_slot$DID_type, "\n")
  cat("\t-Reversible treatment: ", mdd_dat_slot$is_reversible, "\n")
  cat("\t-N units: ", mdd_dat_slot$n_units, ". Treated: ", mdd_dat_slot$n_treated, "\n", sep="")
  cat("\t-N treatment sequences: ", mdd_dat_slot$n_seq, "\n")
  cat("\t-T periods: ", length(mdd_dat_slot$periods), ". Treated: ", length(treated_periods_clean), "\n", sep="")
  cat("\t-Treated periods: ", treated_periods_print, "\n")
  #
  # NextMethod(head(x))
  invisible(x)
}

#' @param conf.int Whether the plot should show confidence intervals to,
#' see \code{\link{mdd_group_means}}
#' @export
#' @rdname mdd_data_format
plot.mdd_dat <- function(x, conf.int=FALSE, ...){

  mdd_vars <- attributes(x)$mdd_dat_slot$var_names

  ## get means
  means <- mdd_group_means(x, conf.int=conf.int)

  ## plot
  base_plot <- means %>%
    ggplot(aes(x = !!sym(mdd_vars$time.index),
               y = !!sym(mdd_vars$y_var),
               color = .data$.group))+
    ggplot2::geom_line()

  ## add CI in case
  if(conf.int){
    base_plot <- base_plot +
      ggplot2::geom_ribbon(aes(ymin=.data$conf.low, ymax=.data$conf.high, fill = .data$.group),
                           alpha =0.6)
  }

  base_plot
}


## reassign
#' @noRd
intrnl_mdd_data_format_reassign <- function(mdd_new, mdd_old){

  mdd_vars_old <- intrnl_mdd_get_mdd_slot(mdd_old)$var_names

  mdd_data_format(mdd_new, y_var = mdd_vars_old$y_var, time.index = mdd_vars_old$time.index,
                  treat = mdd_vars_old$treat, unit.index = mdd_vars_old$unit.index)

}


if(FALSE){
  library(multiDiff)
  dat_DiD_raw <- sim_dat_common()
  dat_any_raw <- sim_dat()
  dat_stag_raw <- sim_dat_staggered()

  ## to mdd
  dat_DiD <- mdd_data_format(data=dat_DiD_raw)
  dat_any <- mdd_data_format(data=dat_any_raw)
  dat_stag <- mdd_data_format(data=dat_stag_raw)

  dat_DiD
  dat_any
  dat_stag

  plot(dat_DiD)
  plot(dat_DiD, conf.int=TRUE)
  plot(dat_stag)

}
