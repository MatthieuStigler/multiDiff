util_add_total_row <- function(df, fun=sum) {
  df %>%
    bind_rows(summarise_all(df, list(~if(is.numeric(.)) fun(., na.rm=TRUE) else if(is.logical(.)) NA else "Diff Time")))
}

#' Diff Diff manual table
#'
#' @template param_all
#' @param control_gr The control group of interest
#' @param treat_gr The treatment group of interest
#' @param subperiod A variable indicating how to group, exclude time periods. See details.
#' @param time_subset The restriction of time period (if gave more than 2 periods)
#' @export
#' @examples
#' dat_DiD_1 <- sim_dat_staggered(Time=2, timing_treatment=2)
#' DD_manu(data=dat_DiD_1)
#'
#' ## Compare with felm
#' library(lfe)
#' felm(y~tr|unit+Time, data=dat_DiD_1)
#'
#' ## DiD for longer sequences
#' dat_DiD_long <- sim_dat_staggered(Time=10, timing_treatment=6)
#' DD_manu(data=dat_DiD_long,  control_gr=paste(rep(0, 10), collapse = "_"),
#'         treat_gr=paste(c(rep(0, 5), rep(1,5)), collapse = "_"),
#'         subperiod= rep(c("pre", "post"), each=5))
#' DD_manu(data=dat_DiD_long,  control_gr=paste(rep(0, 10), collapse = "_"),
#'         treat_gr=paste(c(rep(0, 5), rep(1,5)), collapse = "_"),
#'         subperiod= rep(c("placebo", "pre", "post", "dynamic"), c(4,1,1,4)))
#' felm(y~tr|unit+Time, data=dat_DiD_long)
#'
DD_manu <-  function(data, y_var="y", time.index = "Time", treat = "tr", unit.index="unit",
                     control_gr="0_0", treat_gr="0_1", time_subset=NULL,
                     subperiod = NULL) {

  if(!is.null(time_subset)) warning("'time_subset' is deprecated.")
  # time.index = quo("Time")
  # treat = quo("tr")
  # unit.index = quo("unit")
  # y_var= "y"
  seqs <- get_sequences(data, time.index = time.index, unit.index = unit.index,
                        treat = {{treat}}) %>%
    distinct(seq)

  gr_treat <- paste("group", treat_gr, sep="_")
  gr_cntrl <- paste("group", control_gr, sep="_")
  if(!all(c(control_gr, treat_gr) %in% seqs$seq)) {
    print(c(gr_treat, gr_cntrl))
    stop(paste("Missing sequence! Available sequences are:", paste(seqs$seq, collapse = ", ")))
  }


  ## periods
  if(!is.null(subperiod)) {
    data <- data %>%
      left_join(tibble(!!rlang::sym(time.index):=sort(unique(data[,time.index, drop=TRUE])),
                       .period=subperiod), by = time.index)
  } else {
    data <- data %>%
      mutate(.period = !!rlang::sym(time.index))
  }

  ## compute
  data2 <- data %>%
    add_group(time.index = !!enquo(time.index),
              treat = !!enquo(treat),
              unit.index=!!enquo(unit.index))%>%
    filter(!is.na(.data$.period)) %>%
    filter(.data$.group %in% c(control_gr, treat_gr)) %>%
    group_by(.data$.period, .data$.group) %>%
    summarise(!!enquo(y_var):=mean(!!rlang::sym(y_var))) %>%
    ungroup() %>%
    mutate(.group = paste("group", .data$.group, sep="_"))


  if(!is.null(time_subset)) {
    data2 <- data2 %>%
      filter_at(.data$.period, all_vars(. %in% time_subset))
  }
  data3 <- data2 %>%
    spread(.data$.group, !!enquo(y_var)) %>%
    mutate(diff = !!rlang::sym(gr_treat) - !!rlang::sym(gr_cntrl))%>%
    mutate_at(".period", as.character)

  if(nrow(data3)==2) {
    data3 <-  data3 %>%
      util_add_total_row(fun=diff)
  }
    data3 %>%
      mutate(diff_check = !!sym(gr_treat) - !!sym(gr_cntrl))
}

if(FALSE) {
  data <- dplyr::filter(sim_dat(N=10000), Time %in% c(3,4))
  data_345 <- dplyr::filter(sim_dat(N=10000), Time %in% c(3,4, 5))


  multiDiff:::add_group(data, time.index = "Time",
                        treat = "tr", unit.index="unit")

  multiDiff:::add_group(df=data_345)

  DD_manu(data, treat_gr = "0_1", control_gr="0_0")
  DD_manu(data, treat_gr = "0_1", control_gr="1_1")

  DD_manu(data, treat_gr = "1_0", control_gr="0_0")
  DD_manu(data, treat_gr = "1_0", control_gr="1_1")

  DD_manu(data=data_345, treat_gr = "0_0_1", control_gr="0_0_0", time_subset = c(3,4))
}

