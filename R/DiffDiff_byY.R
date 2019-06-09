

#' Compute Diff diff by period
#'
#' @param y_var The name of the y variable
#' @param data data
#' @param time.index time.index
#' @param treat treatment variable
#' @param unit.index unit.index
#' @export
#' @examples
#' data <- sim_dat(N=100)
#' DD_out <- DD(data=data)
#' aggreg_DID(x=DD_out, by_DiD = FALSE)
#' aggreg_DID(x=DD_out, by_DiD = TRUE)


DD <- function(y_var="y", data, time.index = "Time", treat = "tr", unit.index="unit") {

  # time.index = quo("Time")
  # treat = quo("tr")
  # unit.index = quo("unit")

  lf_formula <-  as.formula(paste(y_var, ".treat|.unit+.time", sep="~"))
  data2 <- data %>%
    rename(.time=!!enquo(time.index),
           .treat=!!enquo(treat),
           .unit=!!enquo(unit.index))

  times <- sort(unique(pull(data2, .data$.time)))
  years_df <- tibble(time = tail(times, -1))

  ## Data treratment
  data_treat <- data2 %>%
    select(.time, .treat, .unit) %>%
    lag_group(group_var=.unit, time_var=.time, lag_var=.treat) %>%
    tidyr::unite(seq, c("lag", ".treat"), sep="_")

  ## add to big
  data3 <- data2 %>%
    left_join(data_treat, by = c(".unit", ".time"))

  ## DiD table
  DiD_tab <- tibble(DiD=1:4,
                    treat = rep(c("0_1", "1_0"), each=2),
                    control = rep(c("0_0", "1_1"), times=2),
                    all = paste(treat, .data$control, sep="|"))

  get_all <- function(df) {
    cnt <- count(df, seq) %>%
      tidyr::complete(seq = c("0_1", "1_0", "0_0", "1_1"), fill = list(n=0))

    DiD_tab_here <- DiD_tab %>%
      left_join(cnt %>%
                  rename(treat=seq, n_treat=n), by = c("treat")) %>%
      left_join(cnt %>%
                  rename(control=seq, n_control=n), by = c("control")) %>%
      mutate(n_min = pmin(.data$n_treat, .data$n_control),
             miss_data = .data$n_min<2)

    DiD_tab_here %>%
      mutate(reg_out = map2(all, .data$miss_data,  ~if(.y) NA else felm(lf_formula,
                                  data = df %>%
                                    dplyr::semi_join(filter(df, .time==max(.time) & stringr::str_detect(.data$seq, .x)), by = ".unit")) %>%
                              broom::tidy(conf.int=TRUE))) %>%
      unnest(.data$reg_out)
  }

  ## For each year
  res <- years_df %>%
    mutate(data = map(.data$time, ~filter(data3, .data$.time%in% c(., .-1))),
           reg_out = map(data, get_all)) %>%
    select(-.data$data) %>%
    unnest(.data$reg_out) %>%
    select(-.data$term, -.data$all)
  res


}


#' Aggreg results
#'
#' @param x output from DD
#' @param DiD_keep which to look at
#' @param by_DiD DiD specific?
#' @param \ldots grouping vars... time
#' @export
aggreg_DID <- function(x, DiD_keep=c(1,4), by_DiD=FALSE, ...) {

  if(by_DiD) x <- dplyr::grouped_df(x, "DiD")
  x %>%
    filter(.data$DiD %in% DiD_keep) %>%
    # group_by(!!!enquos(...)) %>%
    # distinct(time, treat, n_treat) %>%
    # mutate(perc = n_treat/sum(n_treat)) %>%
    summarise(estimate = weighted.mean(.data$estimate, w=.data$n_treat)) %>%
    ungroup()

}




if(FALSE) {
  res_out <- DD(data = dat_sim_1, time.index = "Time")
  res_out_asym <- DD(data = dat_sim_1, y_var = "y_asym",  time.index = "Time")

  res_out %>%
    filter(time==2) %>%
    select(time, DiD, treat, control, estimate)


  res_out %>%
    aggreg_DID()

  res_out %>%
    aggreg_DID(DiD_keep=1:4, DiD, treat, control)

  res_out_asym %>%
    aggreg_DID(DiD_keep=1:4, DiD, treat, control)

  ## compare
  mod.did <- wfe(y~ tr, data = dat_sim_1, treat = "tr",
                 unit.index = "unit", time.index = "Time", method = "unit",
                 qoi = "ate", estimator ="did", White=FALSE)

  mod.did2 <- wfe(y~ tr, data = dat_sim_1, treat = "tr",
                  unit.index = "unit", time.index = "Time", method = "unit",
                  qoi = "att", estimator ="did", White=FALSE)

  mod.did
  mod.did2

  res_out %>%
    aggreg_DID(DiD_keep=1:4, DiD, treat, control) %>%
    filter(DiD==1) %>%
    as.data.frame()


  ## plot
  res_out %>%
    ggplot(aes(x=time, y=estimate, colour = as.factor(DiD)))+
    geom_line() +
    geom_errorbar(aes(ymin=conf.low, ymax = conf.high))

}

