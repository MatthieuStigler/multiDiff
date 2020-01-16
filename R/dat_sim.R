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
#'## NEW
################################

#' SImulate
#' @param  N number of distinct units
#' @param Time number of distinct time
#' @param beta coef
#' @param seed seed
#' @param gamma To generate the series \code{y_asym}, which has longer dynamics.
#' Gamma is the coefficients for observations that were not treated (0) the previous period,
#' while the standard beta will apply for values currently treated.
#' @param prob_treat Probability of treatment
#' @export
#' @examples
#' sim_dat()
sim_dat <- function(N = 1000, Time = 15, beta =1, gamma = 0.7, seed=NULL, prob_treat = 0.25) {

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


}




if(FALSE){
  dat_sim_1 %>%
    count(unit, tr) %>%
    filter(tr==1) %>%
    count(n) %>%
    mat_add_total_row()

}


