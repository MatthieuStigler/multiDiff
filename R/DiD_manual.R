## check one manually
add_group <- function(df, time.index = "Time", treat = "tr", unit.index="unit"){
  groups <- df %>%
    select(!!enquo(unit.index), !!enquo(time.index), !!enquo(treat)) %>%
    spread(!!enquo(time.index), !!enquo(treat)) %>%
    tidyr::unite(.group, -!!enquo(unit.index))
  df %>%
    left_join(groups %>%
                select(.data$unit, .data$.group), by = "unit")
}

util_add_total_row <- function(df, fun=sum) {
  df %>%
    bind_rows(summarise_all(df, list(~if(is.numeric(.)) fun(., na.rm=TRUE) else if(is.logical(.)) NA else "Total")))
}

#' Diff Diff manual table
#'
#' @param y_var The name of the y variable
#' @param data data
#' @param time.index time.index
#' @param treat treatment variable
#' @param unit.index unit.index
#' @param control_gr The control group of interest
#' @param treat_gr The treatment group of interest
#' @param time_per The restriction of time period (if gave more than 2 periods)
#' @export
#' @examples
#' data <- sim_dat(N=100, Time =2)
#' DD_manu(data=data)
DD_manu <-  function(data, y_var="y", time.index = "Time", treat = "tr", unit.index="unit",
                     control_gr="0_0", treat_gr="0_1", time_per=NULL) {

  # time.index = quo("Time")
  # treat = quo("tr")
  # unit.index = quo("unit")

  gr_treat <- paste("group", treat_gr, sep="_")
  gr_cntrl <- paste("group", control_gr, sep="_")

  data2 <- data %>%
    add_group(time.index = !!enquo(time.index), treat = !!enquo(treat), unit.index=!!enquo(unit.index)) %>%
    filter(.data$.group %in% c(control_gr, treat_gr)) %>%
    group_by(.data$Time, .data$.group) %>%
    summarise(!!enquo(y_var):=mean(!!rlang::sym(y_var))) %>%
    ungroup() %>%
    mutate(.group = paste("group", .data$.group, sep="_"))

  if(!is.null(time_per)) {
    data2 <- data2 %>%
      filter_at(vars(!!time.index), all_vars(. %in% time_per))
  }
  data2 %>%
    spread(.data$.group, !!enquo(y_var)) %>%
    mutate(diff = !!rlang::sym(gr_treat) - !!rlang::sym(gr_cntrl))%>%
    mutate_at(vars(!!enquo(time.index)), as.character) %>%
    util_add_total_row(fun=diff) %>%
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

  DD_manu(data=data_345, treat_gr = "0_0_1", control_gr="0_0_0", time_per = c(3,4))
}

