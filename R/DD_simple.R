#' Estimate simple did
#'
#' Standard two-way FE estimation of a DiD
#' @template param_mdd_dat
#' @template param_weights
#' @template param_cluster
#' @examples
#' ## simulate and format data
#' DID_dat <- sim_dat_common(timing_treatment = 5:10, as_mdd=TRUE)
#'
#' ## Estimate DiD
#' mdd_DD_simple(DID_dat)
#'
#' ## estimate with weights
#' # add random unit-year specific weights:
#' DID_dat$wght <- runif(nrow(DID_dat))
#' mdd_DD_simple(DID_dat, weights=~wght)
#' @seealso \code{\link{mdd_event_study}} for the event study.
#' @export
mdd_DD_simple <-  function(mdd_dat, weights = NULL, cluster = NULL){

  if(is.character(cluster)) cluster <- as.formula(paste0("~", cluster))

  ## mdd formatting
  if(!inherits(mdd_dat, "mdd_dat")) stop("Data should be formatted with 'mdd_data_format' first ")
  mdd_dat_slot <- intrnl_mdd_get_mdd_slot(mdd_dat)
  mdd_vars <- mdd_dat_slot$var_names

  formu <- paste0(mdd_vars$y_var, " ~ ",
                  mdd_vars$treat, " | ",
                  mdd_vars$unit.index, " + ",
                  mdd_vars$time.index)

  res <- fixest::feols(as.formula(formu), data =mdd_dat, weights = weights, cluster = cluster)

  ## format result
  class(res) <- c("mdd_DiD", class(res))
  attr(res, "mdd_dat_slot") <- mdd_dat_slot
  res
}

#'@export
tidy.mdd_DiD <- function(x, conf.int = FALSE, conf.level = 0.95, ...){
  tidyhere <- utils::getFromNamespace("tidy.fixest", "broom")
  tidyhere(x=x, conf.int=conf.int, conf.level=conf.level, ...)
}

if(FALSE){

  DID_dat <- multiDiff::sim_dat_staggered(N=100, perc_always = 0, Time=10,
                                          timing_treatment = 6, perc_treat=0.5)
  DD(data=DID_dat)
}

