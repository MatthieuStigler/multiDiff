#' Run placebo test for DiD
#'
#' Subset the data to the pre-period, and assign a treatment value of 1 to units
#' eventually treated
#'
#' @template param_mdd_dat
#' @param T_treated Number of pre-period to consider as treated
#' @param cluster,weights see \code{\link{mdd_DD_simple}}
#' @examples
#' DID_dat_raw <- sim_dat_common(timing_treatment = 5:10)
#' DID_dat <- mdd_data_format(DID_dat_raw)
#' mdd_test_placebo(mdd_dat = DID_dat, T_treated = 1)
#' mdd_test_placebo(mdd_dat = DID_dat, T_treated = 2)
#' @export
mdd_test_placebo <- function(mdd_dat, T_treated = NULL, weights = NULL, cluster=NULL){

  ## Check data formatting
  if(!inherits(mdd_dat, "mdd_dat")) stop("Data should be formatted with 'mdd_data_format' first ")
  mdd_dat_slot <- intrnl_mdd_get_mdd_slot(mdd_dat)
  mdd_vars <- mdd_dat_slot$var_names

  ## Check design
  DID_type <- mdd_dat_slot$DID_type
  if(!DID_type %in% c("classical")) stop("Placebo only implemented for `classical` design")
  pre_periods <- mdd_dat_slot$periods[!mdd_dat_slot$periods %in% mdd_dat_slot$treated_periods]
  # T_tot <- length(mdd_dat_slot$periods)
  T_pre <- length(pre_periods)
  # T_post <- length(mdd_dat_slot$treated_periods)
  if(is.null(T_treated)) {
    T_treated <- min(length(mdd_dat_slot$treated_periods), T_pre-1)
  } else {
    if(T_treated>T_pre-1) stop(paste("Arg. 'T_treated' should not be bigger than", T_pre-1))
    if(T_treated<1) stop("Arg. 'T_treated' should be >0")
  }
    # treated.fraction <- length(mdd_dat_slot$treated_periods)/length(mdd_dat_slot$periods)
  # T_pre_placebo <- floor(T_pre * (1 - treated.fraction))
  # T_post_placebo <- T_pre-T_pre_placebo
  post_period_placeb <- tail(pre_periods, T_treated)

  ## subset data, give 1 to treated
  mdd_dat_placeb <- mdd_dat %>%
    intrnl_add_treat_status_mdd() %>%
    filter(!!rlang::sym(mdd_vars$time.index) %in% pre_periods) %>%
    mutate(!!rlang::sym(mdd_vars$treat) := if_else(!!rlang::sym(mdd_vars$time.index) %in% post_period_placeb & .data$treat_categ =="Treat", 1,0)) %>%
    select(-all_of("treat_categ")) %>%
    intrnl_mdd_data_format_reassign(mdd_dat)

  ## re-estimate
  mdd_dat_placeb %>%
    mdd_DD_simple(cluster=cluster, weights = weights)
}

if(FALSE){

  mdd_test_placebo(mdd_dat = DID_dat)
  mdd_test_placebo(mdd_dat = DID_dat, T_treated = 0)
  mdd_test_placebo(mdd_dat = DID_dat, T_treated = 1)
  mdd_test_placebo(mdd_dat = DID_dat, T_treated = 2)
  mdd_test_placebo(mdd_dat = DID_dat, T_treated = 3)
  mdd_test_placebo(mdd_dat = DID_dat, T_treated = 4)

}
