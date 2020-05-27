

#' Compute the diff-and-diff by period. \lifecycle{experimental}
#'
#' @template param_all
#' @param min_obs_required the minimum number of either treat or control needed to run a regresion
#' @param treatment Is treatment binary or categorical?
#' @export
#' @seealso \code{\link{DiD_aggreg}} for aggregating the output of \code{DD} over years.
#' @examples
#' data <- sim_dat(N=100)
#' DD_out <- DD(data=data)
#' DiD_aggreg(x=DD_out, by_DiD = FALSE)
#' DiD_aggreg(x=DD_out, by_DiD = TRUE)


DD <- function(y_var="y", data, time.index = "Time", treat = "tr", unit.index="unit",
               min_obs_required = 2, treatment=c("binary", "categorical")) {

  treatment <- match.arg(treatment)
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

  ## Data treatment
  data_treat <- data2 %>%
    select(.data$.time, .data$.treat, .data$.unit) %>%
    lag_group(group_var=".unit", time_var=".time", value_var=".treat") %>%
    # lag_group(group_var=.data$.unit, time_var=.data$.time, lag_var=.data$.treat) %>%
    tidyr::unite(seq, c(".treat_lag1", ".treat"), sep="_")

  ## add to big
  data3 <- data2 %>%
    left_join(data_treat, by = c(".unit", ".time"))

  ## DiD table
  if(treatment=="binary"){
    DiD_tab <- tibble(DiD=1:4,
                      treat = rep(c("0_1", "1_0"), each=2),
                      control = rep(c("0_0", "1_1"), times=2),
                      all = paste(treat, .data$control, sep="|"))
    seqs_all <- c("0_1", "1_0", "0_0", "1_1")
  } else {
    treat_vals <- sort(unique(data2$.treat))
    seqs_all <- outer(treat_vals, treat_vals, function(x, y) paste(x, y, sep="_"))
    seq_switch <- sort(seqs_all[row(seqs_all)!=col(seqs_all)])
    seq_control <- str_replace(seq_switch, "([0-9]+)_[0-9]+", "\\1_\\1")
    DiD_tab <- tibble(DiD=1:length(seq_control),
                      treat = seq_switch,
                      control = seq_control,
                      all = paste(treat, .data$control, sep="|"))
  }

  ## Main function
  DID_one_year <- function(df) {

    ## count units in each sequence data
    cnt <- df %>%
      filter(.data$.time==max(.data$.time)) %>%
      count(seq) %>%
      tidyr::complete(seq = seqs_all, fill = list(n=0))

    ## Add counts to DiD table
    DiD_tab_here <- DiD_tab %>%
      left_join(cnt %>%
                  rename(treat=seq, n_treat=n), by = c("treat")) %>%
      left_join(cnt %>%
                  rename(control=seq, n_control=n), by = c("control")) %>%
      mutate(n_min = pmin(.data$n_treat, .data$n_control),
             miss_data = .data$n_min<min_obs_required)

    ## subset and run for each period
    DiD_tab_here %>%
      mutate(data = map(all, ~df %>%
                          dplyr::semi_join(filter(df, .time==max(.time) & stringr::str_detect(.data$seq, .x)), by = ".unit")),
             D_var = map_dbl(data, ~var(.$.treat)),
             y_var = map_dbl(data, ~var(.[y_var])),
             reg_out = map2(data, .data$miss_data,  ~if(.y) tibble(estimate=NA) else felm(lf_formula, data = .x) %>%
                              broom::tidy(conf.int=TRUE)),
             n_vals=map_int(data, nrow)) %>%
      select(-.data$data) %>%
      unnest(.data$reg_out)
  }

  ## internal test
  # data3 <<- data3
  # years_df <<- years_df
  if(FALSE) {
    time_one <- years_df$time[2]
    dat_inner <- filter(data3, .data$.time%in% c(time_one, time_one-1))
    DID_one_year(df=dat_inner)
  }

  ## Run get_all for each year
  res <- years_df %>%
    mutate(data = map(.data$time, ~filter(data3, .data$.time%in% c(., .-1))),
           reg_out = map(data, DID_one_year)) %>%
    select(-.data$data) %>%
    unnest(.data$reg_out)

  if(all(res$miss_data)) {
    warning("No variation in treatment found!?")
  } else {
    res <-  res %>%
      select(-.data$term, -.data$all)
  }
  res

}

#' Count the treated/control
#'
#' @export
#' @rdname DiD_aggreg
DiD_count <- function(x) {

  x %>%
    select(.data$time, .data$DiD, .data$n_treat, .data$n_control) %>%
    gather("group", n, .data$n_treat, .data$n_control) %>%
    mutate(group= stringr::str_remove(.data$group, "n_")) %>%
    left_join(x %>% distinct(.data$DiD, .data$treat, .data$control), by = "DiD") %>%
    mutate(group = if_else(.data$group=="treat", .data$treat, .data$control)) %>%
    arrange(.data$time) %>%
    distinct(.data$time, .data$group, .data$n) %>%
    group_by(.data$time) %>%
    mutate(Tot = sum(.data$n)) %>%
    ungroup()
}

#' Aggreg results
#'
#' @param x output from DD
#' @param DiD_keep which to look at
#' @param by_DiD DiD specific?
#' @param \ldots grouping vars... time
#' @export
#' @examples
#' data <- sim_dat(N=100)
#' DD_out <- DD(data=data)
#' DiD_aggreg(x=DD_out, by_DiD = FALSE)
#' DiD_aggreg(x=DD_out, by_DiD = TRUE)
#' DiD_count(x=DD_out)

DiD_aggreg <- function(x, DiD_keep=c(1,4), by_DiD=FALSE, ...) {

  if(by_DiD) x <- dplyr::grouped_df(x,  c("DiD", "treat", "control"))
  x %>%
    filter(.data$DiD %in% DiD_keep) %>%
    # group_by(!!!enquos(...)) %>%
    # distinct(time, treat, n_treat) %>%
    # mutate(perc = n_treat/sum(n_treat)) %>%
    summarise(estimate = weighted.mean(.data$estimate, w=.data$n_treat)) %>%
    ungroup()

}




if(FALSE) {
  dat_sim_1 <- sim_dat(N=5000, Time = 5)
  coef(lfe::felm(y~tr|Time+unit, data = dat_sim_1))
  res_out <- DD(data = dat_sim_1, time.index = "Time")
  res_out

  ## weighting?
  nrow(dat_sim_1)
  sum(res_out$n_vals)
  with(res_out, weighted.mean(estimate, D_var*(n_vals-2)))

  ##
  res_out_asym <- DD(data = dat_sim_1, y_var = "y_asym",  time.index = "Time")

  res_out %>%
    filter(time==2) %>%
    select(time, DiD, treat, control, estimate)

  res_out %>%
    DiD_aggreg()

  res_out %>%
    DiD_aggreg(DiD_keep=1:4, by_DiD=TRUE)




  res_out_asym %>%
    DiD_aggreg(DiD_keep=1:4, DiD, treat, control)

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
    DiD_aggreg(DiD_keep=1:4, DiD, treat, control) %>%
    filter(DiD==1) %>%
    as.data.frame()


  ## plot
  res_out %>%
    ggplot(aes(x=time, y=estimate, colour = as.factor(DiD)))+
    geom_line() +
    geom_errorbar(aes(ymin=conf.low, ymax = conf.high))

  ## multiple periods
  dat_sim_2 <- sim_dat(N=5000, Time = 5)
  dat_sim_2$tr <- sample(1:4, size=nrow(dat_sim_2), replace = TRUE)
  DD(data = dat_sim_2, y_var = "y",  time.index = "Time", treatment = "categorical")
}

