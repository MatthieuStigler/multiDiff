#' Run synthetic diff-diff by Arkhangelsky et al (2019)
#'
#' @description Simple wrapper for the package synthdid
#' @details Note that the package should be downloaded from github, see \url{https://synth-inference.github.io/synthdid/}
#' @template param_mdd_dat
#' @param add_weights return the weights?
#' @param ... Further argumetns pased to synthdid::synthdid_estimate
#' @seealso \link{mdd_gsynth} for the Generalized Synthetic Control Method by Xu (2017)
#' @examples
#' if(require(synthdid)){
#'   data('california_prop99')
#'   mdd_california_prop99 <- mdd_data_format(california_prop99,
#'                                            y_var = "PacksPerCapita",time.index = "Year",
#'                                            treat = "treated", unit.index = "State")
#'   mdd_synthdid(mdd_dat=mdd_california_prop99)
#' }
#' @export
mdd_synthdid <- function(mdd_dat, add_weights=FALSE, ...){

  if(!requireNamespace("synthdid", quietly = TRUE)) stop("Please install `synthdid`")

  ## prep data
  mdd_dat_slot <- intrnl_mdd_get_mdd_slot(mdd_dat)
  mdd_vars <- mdd_dat_slot$var_names
  tr_quo <- rlang::sym(mdd_vars$treat)
  time_quo <- rlang::sym(mdd_vars$time.index)
  unit_quo <- rlang::sym(mdd_vars$unit.index)
  setup <- synthdid::panel.matrices(as.data.frame(mdd_dat) %>%
                                      mutate({{tr_quo}} := as.integer({{tr_quo}})),
                                    unit=mdd_vars$unit.index,
                                    time = mdd_vars$time.index,
                                    outcome = mdd_vars$y_var,
                                    treatment = mdd_vars$treat)

  ## estimate
  res <- synthdid::synthdid_estimate(setup$Y, setup$N0, setup$T0, ...)

  ## re-estimate?
  if(add_weights){
    W <- attributes(res)$weights

    W_time <- W$lambda
    W_time_full <- c(W_time, rep(1, length(mdd_dat_slot$treated_periods)))
    W_time_full_df <- tibble({{time_quo}} := mdd_dat_slot$periods, weight_time=W_time_full)

    W_units <- W$omega
    W_units_full <- c(W_units, rep(1, mdd_dat_slot$n_units-length(W_units)))
    W_units_full_df <- tibble({{unit_quo}} := rownames(setup$Y), weight_unit=W_units_full)

    ## add weights to data
    mdd_dat_full <- mdd_dat %>%
      left_join(W_time_full_df, by = mdd_vars$time.index) %>%
      left_join(W_units_full_df, by = mdd_vars$unit.index) %>%
      mutate(weights= .data$weight_unit * .data$weight_time)
    attr(mdd_dat_full, "mdd_dat_slot") <- mdd_dat_slot ## attributes are lost by mutate!!


    ## re-estimate
    # res <- mdd_DD_simple(mdd_dat_full, weights = mdd_dat_full$weights)
    attr(res, "mdd_data") <- mdd_dat_full
  }

  attr(res, "mdd_dat_slot") <- mdd_dat_slot
  # class(res) <- c(class(res), "mdd_synthdid") ## see issue https://github.com/synth-inference/synthdid/issues/100
  res
}

#' @export
coef.synthdid_estimate <- function(object, ...) as.double(object)

## tidy method, see https://www.tidymodels.org/learn/develop/broom/

#' @importFrom generics tidy
#' @export
generics::tidy


#' Tidy a 'synthdid_estimate' object
#' @param x output from \code{\link{mdd_synthdid}} or synthdid::synthdid_estimate
#' @param conf.int,conf.level,... as standard
#' @param method method used in synthdid::vcov.synthdid_estimate
#' @export
tidy.synthdid_estimate <- function(x, conf.int=FALSE, conf.level=0.95, method='jackknife', ...){

  term <- attr(x, "mdd_dat_slot")$var_names$treat
  if(is.null(term)) term <- NA_character_
  coef <- as.double(x)
  # se = sqrt(synthdid::vcov.synthdid_estimate(x, method=method))
  se = sqrt(stats::vcov(x, method=method))
  res <- data.frame(term = term,
             estimate = coef,
             std.error =se,
             statistic =coef/se,
             p.value =2 * stats::pnorm(abs(coef/se), lower.tail = FALSE))
  if(conf.int) {
    left_Q <- (1-conf.level)/2
    quants <- stats::qnorm(c(left_Q, 1-left_Q))
    CI_LH <- rep(coef,2) + quants * se[[1]]
    res <- cbind(res,
                 data.frame(conf.low = CI_LH[1],
                            conf.high =CI_LH[2]))
  }
  res
}


if(FALSE){
  if(require(synthdid)){
    # data('california_prop99')
    n_distinct(california_prop99$State) # 39 states
    n_distinct(filter(california_prop99, treated==1)$State) # 1 treated state
    n_distinct(california_prop99$Year) # 31 years
    n_distinct(filter(california_prop99, treated==1)$Year) # 12 treated years, 19 untreated
    mdd_california_prop99 <- mdd_data_format(california_prop99, y_var = "PacksPerCapita",
                                             time.index = "Year", treat = "treated", unit.index = "State")
    mdd_california_prop99
    res <- mdd_synthdid(mdd_dat=mdd_california_prop99)
    res
    coef(res)
    tidy(res)

    ## General
    dat_sim_N1 <- mdd_data_format(sim_dat_common(N = 1000,  timing_treatment = 5:10, perc_treat=0.001))
    dat_sim <- mdd_data_format(sim_dat_common(N = 1000,  timing_treatment = 5:10, perc_treat=0.1))
    dat_sim
    # coef(mdd_DD_simple(dat_sim))
    res_gen <- mdd_synthdid(dat_sim)
    coef(res_gen)
    vcov(res_gen, method = "jackknife")
    tidy(res_gen)
    tidy(res_gen, conf.int = TRUE)
  }
}
