#' Diff Diff manual table
#'
#' @template param_all
#' @export
#' @examples
#' dat_DiD_raw <- sim_dat_common()
#' dat_DiD <- mdd_data_format(dat_DiD_raw)
mdd_data_format <-  function(data, y_var="y", time.index = "Time", treat = "tr", unit.index="unit"){

  ##
  sequences <- get_sequences(data, time.index = {{time.index}},
                             treat = {{treat}}, unit.index = {{unit.index}}) %>%
    rename(.group="seq")
  seq_uniques <- unique(sequences$.group)


  ## Compute various info
  is_reversible <- any(str_detect(seq_uniques, "1_0"))
  n_units <- nrow(sequences)
  n_seq <- length(seq_uniques)
  treated_periods <- map(str_split(seq_uniques, "_"), ~which(.=="1")) %>%
    unlist() %>% unique() %>% sort()
  periods <- unique(pull(data, {{time.index}}))


  ## classify time
  DID_type <- case_when(n_seq==2 & !is_reversible ~ "classical",
                        n_seq==2 & is_reversible ~ "classical-single time",
                        n_seq>2 & !is_reversible ~ "staggered",
                        n_seq>2 & is_reversible ~ "general",
                        TRUE ~"UNCLASSFIED YET?")

  ## put all info together
  vars <- list(y_var=rlang::as_name(enquo(y_var)),
               time.index=rlang::as_name(enquo(time.index)),
               treat=rlang::as_name(enquo(treat)),
               unit.index=rlang::as_name(enquo(unit.index)))
  mdd_dat_slot <- list(n_units = n_units,
                       n_seq = n_seq,
                       is_reversible = is_reversible,
                       treated_periods = treated_periods,
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

  treated_periods_clean <- mdd_dat_slot$treated_periods
  if(mdd_dat_slot$is_reversible) {
    treated_periods_print <- treated_periods_clean
  } else {
    treated_periods_print <- paste(range(treated_periods_clean), collapse = ":")
  }

  cat("### MDD data\n")
  cat("\t-Design type: ", mdd_dat_slot$DID_type, "\n")
  cat("\t-N units: ", mdd_dat_slot$n_units, "\n")
  cat("\t-N treatment groups: ", mdd_dat_slot$n_seq, "\n")
  cat("\t-Reversible treatment: ", mdd_dat_slot$is_reversible, "\n")
  cat("\t-treated periods: ", treated_periods_print, "\n")
  #
  # NextMethod(head(x))
  invisible(x)
}

#' @export
#' @rdname mdd_data_format
plot.mdd_dat <- function(x, ...){

  mdd_vars <- attributes(x)$mdd_dat_slot$var_names

  means <- mdd_simple_means(x)

  means %>%
    ggplot(aes(x = !!sym(mdd_vars$time.index),
               y = !!sym(mdd_vars$y_var),
               color = .data$.group))+
    ggplot2::geom_line()
}

mdd_simple_means <- function(mdd_dat) {
  if(!inherits(mdd_dat, "mdd_dat")) stop("Data should be formatted with 'mdd_data_format' first ")
  mdd_dat_slot <- attributes(mdd_dat)$mdd_dat_slot
  mdd_vars <- mdd_dat_slot$var_names

  # add groups
  mdd_dat_add <- mdd_dat %>%
    add_group(time.index = mdd_vars$time.index, treat = mdd_vars$treat,
              unit.index = mdd_vars$unit.index, group_rename_maybe=TRUE)

  ##
  mdd_dat_add %>%
    group_by(dplyr::across(c(".group", mdd_vars$time.index))) %>%
    summarise(dplyr::across(mdd_vars$y_var, mean),
              .groups = "drop")

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
  plot(dat_stag)

  mdd_simple_means(dat_DiD)
  mdd_simple_means(mdd_data_format(data=dat_stag_raw))

}
