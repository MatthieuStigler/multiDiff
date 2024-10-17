

#' @export
#' @rdname mdd_estim_CH
mdd_estim_CH_2020 <- function(mdd_dat, ...){
  requireNamespace("DIDmultiplegt")
  vars <- intrnl_mdd_get_mdd_slot(mdd_dat)$var_names


  out <- DIDmultiplegt::did_multiplegt_old(as.data.frame(mdd_dat),
                                           Y=vars$y_var, G=vars$unit.index, T=vars$time.index,
                                           D=vars$treat,
                                           ...)
  class(out) <- c("did_multiplegt_old", class(out))
  out
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
    class(res) <- c("did_multiplegt_old", class(res))
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

#'@export
coef.did_multiplegt_old <- function(object, ...){
  out <- object$effect
  names(out) <- "ATT"
  out
}

#'@export
vcov.did_multiplegt_old <- function(object, ...){
  has_se <- "se_effect" %in% names(object)
  if(!has_se) return(NA)
  M <- matrix(object$se_effect^2)
  rownames(M) <- colnames(M) <- "ATT"
  M
}

#'@export
tidy.did_multiplegt_old <- function(x, conf.int=TRUE, conf.level = 0.95, ...){
  has_se <- "se_effect" %in% names(x)
  coef <- x$effect

  if(!has_se){
    df <- data.frame(term = "ATT",
                     estimate=coef)
  } else {
    se <- x$se_effect
    df <- data.frame(term = "ATT",
                     estimate=coef,
                     std.error=se,
                     statistic = coef/se,
                     p.value=2 * stats::pnorm(abs(coef/se), lower.tail = FALSE))
    if(conf.int){
    CI <- confint(x, level = conf.level)
    df <- cbind(df,
                conf.low = CI[1,1],
                conf.high = CI[1,2])
    }
  }
  df
}

if(FALSE){
  library(multiDiff)
  # data(GentzkowData)
  GentzkowData_md <- mdd_data_format(GentzkowData,
                                     y_var="prestout",
                                     time.index = "year",
                                     treat = "numdailies",
                                     unit.index="cnty90" )
  # CH_2020_out <- mdd_estim_CH_2020(mdd_dat = GentzkowData_md, brep=10, covariance=FALSE)
  CH_2020_out <- mdd_estim_CH_2020(mdd_dat = GentzkowData_md)
  CH_2020_out <- mdd_estim_CH_2020(mdd_dat = GentzkowData_md, brep=2)
  tidy(CH_2020_out)
  tidy(CH_2020_out, conf.int = FALSE)
  coef(object=CH_2020_out)
  vcov(CH_2020_out)
  confint(CH_2020_out)

  CH_2024_out <- mdd_estim_CH(mdd_dat = GentzkowData_md)
  CH_2024_out
  tidy(CH_2024_out)

  DIDmultiplegtDYN:::print.did_multiplegt_dyn

}
