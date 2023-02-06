#' Run synthetic diff-diff
#'
#' @template param_mdd_dat
#' @param feols_output re-estuiamte model with feols?
#'
#' @examples
#' if(require(synthdid)){
#'   data('california_prop99')
#'   mdd_california_prop99 <- mdd_data_format(california_prop99,
#'                                            y_var = "PacksPerCapita",time.index = "Year",
#'                                            treat = "treated", unit.index = "State")
#'   mdd_synthdid(mdd_dat=mdd_california_prop99)
#' }
#' @export
mdd_synthdid <- function(mdd_dat, feols_output=FALSE){

  if(!requireNamespace("synthdid", quietly = TRUE)) stop("Please install `synthdid`")

  ## prep data
  mdd_dat_slot <- intrnl_mdd_get_mdd_slot(mdd_dat)
  # mdd_dat_slot <- multiDiff:::intrnl_mdd_get_mdd_slot(mdd_dat)
  mdd_vars <- mdd_dat_slot$var_names
  tr_quo <- rlang::sym(mdd_vars$treat)
  time_quo <- rlang::sym(mdd_vars$time.index)
  unit_quo <- rlang::sym(mdd_vars$unit.index)
  setup <- synthdid::panel.matrices(as.data.frame(mdd_dat) %>%
                                      mutate({{tr_quo}} := {{tr_quo}}),
                                    unit=mdd_vars$unit.index,
                                    time = mdd_vars$time.index,
                                    outcome = mdd_vars$y_var,
                                    treatment = mdd_vars$treat)

  ## estimate
  res <- synthdid::synthdid_estimate(setup$Y, setup$N0, setup$T0)

  ## re-estimate?
  if(feols_output){
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
    res <- mdd_DD_simple(mdd_dat_full, weights = mdd_dat_full$weights)
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
    all.equal(res[1],
              coef(mdd_synthdid(mdd_dat=mdd_california_prop99, feols_output=TRUE))[[1]])
  }
}
