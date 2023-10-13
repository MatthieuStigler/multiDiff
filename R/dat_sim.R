# N <- 100 # number of distinct units
# Time <- 15 # number of distinct time
#
# ## treatment effect
# beta <- 1
#
# ## generate treatment variable
# treat_M <- matrix(rbinom(N*Time, size = 1, 0.25), ncol = N)
# ## make sure at least one observation is treated for each unit
# while ((sum(apply(treat_M, 2, mean) == 0) > 0) | (sum(apply(treat_M, 2, mean) == 1) > 0) |
#        (sum(apply(treat_M, 1, mean) == 0) > 0) | (sum(apply(treat_M, 1, mean) == 1) > 0)) {
#   treat_M <- matrix(rbinom(N*Time, size = 1, 0.25), ncol = N)
# }
# treat <- c(treat_M)
#
# ## unit fixed effects
# alphai <- rnorm(N, mean = apply(treat, 2, mean))
#
#
# ## generate outcome variable
# y <- matrix(NA, ncol = N, nrow = Time)
# y2 <- alphai + treat  + rnorm(Time)
# for (i in 1:N) {
#   y[, i] <- alphai[i] + treat[, i]  + rnorm(Time)
# }
# all.equal(y2, y.vec)
# y.vec <- c(y)
#
# ## generate unit and time index
# unit.index <- rep(1:N, each = Time)
# time.index <- rep(1:Time, N)
#
# Data.str <- as.data.frame(cbind(y.vec, treat.vec, unit.index, x1.vec, x2.vec))
# colnames(Data.str) <- c("y", "tr", "strata.id", "x1", "x2")
#
# Data.obs <- as.data.frame(cbind(y.vec, treat.vec, unit.index, time.index, x1.vec, x2.vec)) %>%
#   as_tibble()
# colnames(Data.obs) <- c("y", "tr", "unit", "time", "x1", "x2")

################################
### NEW
################################

#' Simulate data
#'
#' Simulates data with common treatment date (\code{sim_dat_common}), staggered date (\code{sim_staggered}) or
#' reversible treatment, i.e. units might loose their treatment status (\code{sim_dat}).
#'
#' @param  N number of distinct units
#' @param Time number of distinct time
#' @param beta coef
#' @param beta_dyn optional coefficients for post-treatment periods in \code{sim_dat_common}. Its length should be equal to the number of post-treatment periods, or 1.
#' @param seed seed
#' @param gamma To generate the series \code{y_asym}, which has longer dynamics.
#' Gamma is the coefficients for observations that were not treated (0) the previous period,
#' while the standard beta will apply for values currently treated.
#' @param prob_treat Probability of treatment
#' @param as_mdd Should output object be formatted of class mdd? Default is `FALSE` for now.
#' @export
#' @examples
#' ## Standard 2 x 2: 2 groups, 2 time periods
#' dat_DiD_1 <- sim_dat_staggered(Time=2, timing_treatment=2)
#' DD_manu(data=dat_DiD_1)
#'
#' ## estimate with panel
#' library(lfe)
#' felm(y~tr|unit+Time, data=dat_DiD_1)
#'
#' ## Long 2 x 2: two  groups, 5 before, 5 after
#' dat_DiD_2 <- sim_dat_staggered(Time=10, timing_treatment=5, as_mdd = TRUE)
#' dat_DiD_2
#' mdd_DD_simple(dat_DiD_2)
#'
#' ## DiD with variation in treatment
#' dat_DiD_3 <- sim_dat_staggered(Time=10, timing_treatment=c(2, 10))
#'
#' ## DiD with dynamic effects post-treatment
#' dyn_eff <- seq(1.1, by = 0.1, length.out = 5)
#' dat_DiD_dyn <- sim_dat_common(N=10000, timing_treatment = 5, beta_dyn = dyn_eff, as_mdd = TRUE)
#' mdd_event_study(dat_DiD_dyn)
sim_dat <- function(N = 1000, Time = 15, beta =1, gamma = 0.7, seed=NULL, prob_treat = 0.25, as_mdd = FALSE) {

  if(!is.null(seed)) set.seed(seed)

  ## individual and firm
  ind_fe <- rep(rnorm(N), each=Time)
  time_fe <- rep(as.numeric(arima.sim(model=list(ar=0.8), n=Time)), N)

  ## simplest
  dat_sim_1 <- tibble(unit = rep(1:N, each=Time),
                      Time = rep(1:Time, times=N),
                      unit_fe = ind_fe,
                      time_fe = time_fe,
                      tr = rbinom(N*Time, size = 1, prob_treat),
                      err = rnorm(N*Time)) %>%
    group_by(.data$unit) %>%
    mutate(lag_1 =dplyr::lag(.data$tr),
           lag_2 =dplyr::lag(.data$tr, n = 2),
           lead_1= dplyr::lead(.data$tr, n = 1),
           lead_2= dplyr::lead(.data$tr, n = 2)) %>%
    ungroup() %>%
    mutate(lag_one_noNA = dplyr::if_else(is.na(.data$lag_1), 0L, .data$lag_1)) %>%
    mutate(y = beta* .data$tr + .data$unit_fe + .data$time_fe + .data$err,
           y_asym = beta* .data$tr + gamma * .data$lag_one_noNA*(1-.data$tr) + .data$unit_fe + .data$time_fe +.data$err )

  if(as_mdd){
    dat_sim_1 <- dat_sim_1 |>
      mdd_data_format()
  }
  dat_sim_1

}

#' @rdname sim_dat
#' @export
sim_dat_common <- function(N = 1000, Time=10, timing_treatment= 2:Time, beta =1, seed=NULL, perc_treat = 0.25,
                           beta_dyn = NULL,
                           as_mdd = FALSE) {

  if(!is.null(seed)) set.seed(seed)

  ## parameters
  N_treat <- round(perc_treat*N)
  N_ctrl <- N-N_treat
  tr_units <- rep(c("treated", "control"), c(N_treat, N_ctrl))
  T_time <- Time

  ## individual and time FEs, erros
  ind_fe <- rep(rnorm(N), each=Time)
  time_fe <- rep(as.numeric(arima.sim(model=list(ar=0.8), n=T_time)), N)
  error <- rnorm(N*T_time)



  ## Compute data
  dat_sim_1 <- tibble(unit = rep(1:N, each=T_time),
                      treat_group = rep(tr_units, each=T_time),
                      Time = rep(1:T_time, times=N),
                      unit_fe = ind_fe,
                      time_fe = time_fe,
                      tr = dplyr::if_else(.data$treat_group=="treated" & Time %in% timing_treatment,1,0)) %>%
    mutate(y = beta* .data$tr + .data$unit_fe + .data$time_fe + error) %>%
    select("unit", "Time", "treat_group", "tr", "y")

  ## eventually add dyn effects
  if(!is.null(beta_dyn)){
    timing_treatment_start <- min(timing_treatment)
    T_post <- T_time-timing_treatment_start
    if(!length(beta_dyn) %in% c(1, T_post)) stop(paste("`beta_dyn` should be of length 1 or", T_post, "(number of post treatment periods)"))
    if(length(beta_dyn)==1) beta_dyn <- rep(beta_dyn, T_post)

    dat_sim_1 <- dat_sim_1 %>%
      mutate(time_post_treated = if_else(.data$treat_group=="treated" & Time >=timing_treatment_start, Time-timing_treatment_start, 0),
             y= .data$y+ c(0, beta_dyn)[.data$time_post_treated+1]) ## all time_post_treated =0 become 1, selecting hence 0
  }

  ## eventually convert to mdd
  if(as_mdd){
    dat_sim_1 <- dat_sim_1 |>
      mdd_data_format()
  }
  dat_sim_1

}

#'@param perc_never,perc_always,perc_treat \emph{Staggered:} Percentage of (during-sample) treated, never and always
#'@param timing_treatment \emph{Staggered:} In what periods can treatment start?
#'@param trend_diff differential trend
#'@rdname sim_dat
#'@export
sim_dat_staggered <- function(N = 1000, Time = 15, beta =1, gamma = 0, seed=NULL,
                              perc_never = 0.2, perc_treat = 0.8, perc_always = 1-perc_treat-perc_never,
                              timing_treatment = 2:Time, trend_diff=0,
                              as_mdd = FALSE) {

  if(!is.null(seed)) set.seed(seed)

  ## individual and firm
  N_treat <- round(perc_treat*N)
  N_always <- round(perc_always*N)
  N_never <- N-N_treat-N_always
  type <- rep(c("treat", "always", "never"), c(N_treat, N_always, N_never))

  ##
  # ind_fe <- rep(rnorm(N), each=Time)
  ind_fe <- rep(c(rnorm(N_treat, mean=trend_diff), rnorm(N_always, mean=trend_diff), rnorm(N_never, mean=0)), each=Time)
  time_fe <- rep(as.numeric(arima.sim(model=list(ar=0.8), n=Time)), N)
  if(length(timing_treatment)==1) {
    starts <- rep(timing_treatment, N_treat)
  } else {
    starts <- sample(timing_treatment, N_treat, replace=TRUE)
  }

  ## timings
  fill_here <- function(t, Total) {
    x <- rep(0, Total)
    if(is.finite(t)& !is.na(t)&t<=Total) {
      x[seq_len(Total)>=t] <- 1
    }
    tibble(Time = seq_len(Total),
           tr=x, length_treat= cumsum(x))
  }
  timings <- tibble(unit = 1:N,
                    type = type,
                    timing_treat = c(starts, rep(1, N_always), rep(Inf, N_never)))

  timings_all <- timings %>%
    mutate(treat = map(.data$timing_treat, ~fill_here(., Time))) %>%
    unnest("treat")

  ## Data
  dat_sim_1 <- tibble(unit = rep(1:N, each=Time),
         Time = rep(1:Time, times=N),
         unit_fe = ind_fe,
         time_fe = time_fe,
         error = rnorm(N*Time)) %>%
    dplyr::full_join(timings_all, by = c("unit", "Time")) %>%
    relocate("unit", "type", "timing_treat", "Time") %>%
    mutate(y = beta* .data$tr + gamma* .data$length_treat + .data$unit_fe + .data$time_fe + .data$error)

  if(as_mdd){
    dat_sim_1 <- dat_sim_1 |>
      mdd_data_format()
  }
  dat_sim_1

}


if(FALSE){
  library(tidyverse)
  dat_sim_1 %>%
    count(unit, tr) %>%
    filter(tr==1) %>%
    count(n) %>%
    mat_add_total_row()


  ## Standard 2 x2: 2 groups, 2 time periods
  sim_dat_staggered(Time=2, timing_treatment=2)%>%
    count(type, Time, tr)

  ## Long 2 x 2: two  groups, 5 before, 5 after
  sim_dat_staggered(Time=10, timing_treatment=5) %>%
    distinct(type, Time, tr) %>%
    spread(Time, tr)


}


