#' Estimate simple did
#'
#' Standard two-way FE estimation of a DiD
#' @template param_mdd_dat
#' @template param_weights
#' @template param_cluster
#' @examples
#' ## simulate and format data
#' DID_dat_raw <- sim_dat_common(timing_treatment = 5:10)
#' DID_dat <- mdd_data_format(DID_dat_raw)
#'
#' ## Estimate DiD
#' mdd_DD_simple(DID_dat)
#' @seealso \code{\link{mdd_event_study}} for the event study.
#' @export
mdd_DD_simple <-  function(mdd_dat, weights = NULL, cluster = NULL){

  if(is.character(cluster)) cluster <- as.formula(paste0("~", cluster))

  ## mdd formatting
  if(!inherits(mdd_dat, "mdd_dat")) stop("Data should be formatted with 'mdd_data_format' first ")
  mdd_vars <- intrnl_mdd_get_mdd_slot(mdd_dat)$var_names

  formu <- paste0(mdd_vars$y_var, " ~ ",
                  mdd_vars$treat, " | ",
                  mdd_vars$unit.index, " + ",
                  mdd_vars$time.index)

  res <- fixest::feols(as.formula(formu), data =mdd_dat, weights = weights, cluster = cluster)

  ## format result
  class(res) <- c("mdd_DiD", class(res))
  res
}

if(FALSE){

  DID_dat <- multiDiff::sim_dat_staggered(N=100, perc_always = 0, Time=10,
                                          timing_treatment = 6, perc_treat=0.5)
  DD(data=DID_dat)
}

