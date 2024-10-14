

#' @export
#' @rdname mdd_estim_CH
mdd_estim_CH_2020 <- function(mdd_dat, ...){
  requireNamespace("DIDmultiplegt")
  vars <- intrnl_mdd_get_mdd_slot(mdd_dat)$var_names


  DIDmultiplegt::did_multiplegt_old(as.data.frame(mdd_dat),
                                    Y=vars$y_var, G=vars$unit.index, T=vars$time.index,
                                    D=vars$treat,
                                    ...)

}

#' de Chaisemartin and d'Haufeuille estimators
#'
#' @description Simple wrapper for  \link[DIDmultiplegt]{did_multiplegt}, see help file there
#' @template param_mdd_dat
#' @param mode type of estimator, see \link[DIDmultiplegt]{did_multiplegt}.
#' @param ... Further arguments passed to \link[DIDmultiplegt]{did_multiplegt}
#' @examples
#' data(GentzkowData)
#' GentzkowData_md <- mdd_data_format(GentzkowData,  y_var="prestout",
#'                                    time.index = "year", treat = "numdailies", unit.index="cnty90" )
#' CH_2024_out <- mdd_estim_CH(mdd_dat = GentzkowData_md)
#' CH_2024_out
#' tidy(CH_2024_out)
#' @seealso \code{\link{mDid_weights_CH}} for a local implementation of the CH weights.
#' @export
mdd_estim_CH <- function(mdd_dat, mode = c("dyn", "had", "old"), ...){
  requireNamespace("DIDmultiplegt")
  mode <- match.arg(mode)
  vars <- intrnl_mdd_get_mdd_slot(mdd_dat)$var_names

  if(mode =="old"){

    res <- DIDmultiplegt::did_multiplegt_old(as.data.frame(mdd_dat),
                                             Y=vars$y_var, G=vars$unit.index, T=vars$time.index,
                                             D=vars$treat, ...)
  } else {
    res <- DIDmultiplegt::did_multiplegt(df=as.data.frame(mdd_dat),
                                         outcome=vars$y_var, group=vars$unit.index, time=vars$time.index,
                                         treatment=vars$treat,
                                         mode=mode,
                                         ...)
  }

  return(res)
}

#' @export
tidy.did_multiplegt_dyn <- function(x, conf.int=TRUE, ...){

  # term <- attr(x, "mdd_dat_slot")$var_names$treat
  # if(is.null(term)) term <- NA_character_
  eff <- x$results$Effects
  coef <- eff[1,"Estimate"]
  se <- eff[1,"SE"]
  res <- data.frame(term = "Estimation of treatment effects: Event-study effects",
                    estimate = coef,
                    std.error =se,
                    statistic =coef/se,
                    p.value =2 * stats::pnorm(abs(coef/se), lower.tail = FALSE))
  if(conf.int) {
    res <- cbind(res,
                 data.frame(conf.low = eff[1,"LB CI"],
                            conf.high =eff[1,"UB CI"]))
  }
  res
}


if(FALSE){
  # data(GentzkowData)
  GentzkowData_md <- mdd_data_format(GentzkowData,
                                     y_var="prestout",
                                     time.index = "year",
                                     treat = "numdailies",
                                     unit.index="cnty90" )
  # CH_2020_out <- mdd_estim_CH_2020(mdd_dat = GentzkowData_md, brep=10, covariance=FALSE)
  CH_2020_out <- mdd_estim_CH_2020(mdd_dat = GentzkowData_md)
  CH_2020_out

  CH_2024_out <- mdd_estim_CH(mdd_dat = GentzkowData_md)
  CH_2024_out
  tidy(CH_2024_out)

  DIDmultiplegtDYN:::print.did_multiplegt_dyn

}
