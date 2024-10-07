################################
#'## Did here
################################

extract_mdd_DiD <- function(model, add_para_test = FALSE, include.n_treated=TRUE,
                            include.Ttreated=FALSE,
                            include.rsquared = FALSE, include.adjrs = FALSE, include.deviance=FALSE,
                            include.groups = FALSE,
                            ...) {

  ## call fixest method
  extr_out <- texreg:::extract.fixest(model,
                                      include.rsquared =include.rsquared,
                                      include.adjrs = include.adjrs,
                                      include.deviance=include.deviance,
                                      include.groups=include.groups,
                                      ...)

  ## Change name
  extr_out@coef.names <- "Treatment"

  if(include.n_treated){
    mdd_slot <- intrnl_mdd_get_mdd_slot(model)
    if(mdd_slot$n_seq!=2) stop("Arg. `include.n_treated` only works for standard DiD")

    extr_out@gof.names <- c(extr_out@gof.names, "Num. control", "Num. treated")
    extr_out@gof <- c(extr_out@gof, mdd_slot$n_units-mdd_slot$n_treated, mdd_slot$n_treated)
    extr_out@gof.decimal <- c(extr_out@gof.decimal, FALSE, FALSE)
  }

  if(include.Ttreated){
    mdd_slot <- intrnl_mdd_get_mdd_slot(model)

    extr_out@gof.names <- c(extr_out@gof.names, "Num. periods")
    extr_out@gof <- c(extr_out@gof, length(mdd_slot$periods))
    extr_out@gof.decimal <- c(extr_out@gof.decimal, FALSE)
  }

  if(add_para_test) {
    if(!all(c("para_test_joint_val", "para_test_joint_pval") %in% names(model))) stop("Output should have added slot 'para_test_joint_(p)val'")

    extr_out@gof.names <- c(extr_out@gof.names, "Parallel test: Wald stat", "Parallel test: p-val")
    extr_out@gof <- c(extr_out@gof, model$para_test_joint_val, model$para_test_joint_pval)
    extr_out@gof.decimal <- c(extr_out@gof.decimal, TRUE, TRUE)

  }
  extr_out
}


################################
#'## gsynth
################################

extract.gsynth <- function(model, type = c("average", "time"),
                           include.n_treated=TRUE, include.nobs = TRUE,
                           include.hyper =TRUE, ...) {

  type <- match.arg(type)
  has_se <- !is.null(model$est.att)

  ## simple way
  out <- tidy(model, type = type)
  co <- out$estimate
  co_names <- if(type=="average") "Treatment" else out$term
  if(has_se) {
    se <- out$std.error
    pval <- out$p.value
  } else {
    se <- numeric(0)
    pval <- numeric(0)
    if(length(co)==1) warning("POssible issue with https://github.com/leifeld/texreg/issues/209")
  }

  ## GOF
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()

  if(include.nobs){
    gof <- c(gof, sum(model$obs.missing!=0))
    gof.names <- c(gof.names, "Num. obs.")
    gof.decimal <- c(gof.decimal, FALSE)
  }

  ### gof
  if(include.hyper){

    hyper <-  model$r.cv
    if(is.null(hyper)) hyper <- model$lambda.cv
    gof <- c(gof, hyper)
    gof.names <- c(gof.names, "Hyperparameter")
    gof.decimal <- c(gof.decimal, FALSE)
  }

  ## N treated
  if(include.n_treated){
    gof <- c(gof, model$Nco, model$Ntr)
    gof.names <- c(gof.names, "Num. control", "Num. treated")
    gof.decimal <- c(gof.decimal, FALSE,FALSE)
  }
  # if(include.force){
  #   force <- c("none", "unit", "time", "two-way")[model$force+1]
  #   gof <- c(gof, force)
  #   gof.names <- c(gof.names, "Force:")
  #   gof.decimal <- c(gof.decimal, NA)
  # }


  tr <- texreg::createTexreg(coef.names = co_names,
                             coef = co,
                             se = se,
                             pvalues = pval,
                             gof = gof,
                             gof.names = gof.names,
                             gof.decimal =  gof.decimal)
  return(tr)
}


################################
#'## synthdid
################################

extract.synthdid <- function(model, ...) {

  ## this has more slots, but much slower
  # s <- summary(model)
  # names <- rownames(s$coef)

  ## simple way
  out <- tidy(model)
  co <- out$estimate
  se <- out$std.error
  pval <- out$p.value

  tr <- createTexreg(
    coef.names = "Treatment",
    coef = co,
    se = se,
    pvalues = pval,
  )
  return(tr)
}

#' Export texreg functions
#'
#' @examples
#' mdd_texreg_export()
#' if(require(gsynth)){
#'   data(gsynth)
#'   mdd_simdata_gs <- mdd_data_format(simdata,
#'                                y_var = "Y",time.index = "time",
#'                             treat = "D", unit.index = "id")
#'   res <- mdd_gsynth(mdd_dat=mdd_simdata_gs, echo=FALSE, se=TRUE)
#'   res_MC <- mdd_gsynth(mdd_dat=mdd_simdata_gs, echo=FALSE, se=FALSE, estimator="mc")
#' }
#' if(require(texreg)) {
#'   setMethod("extract", signature = className("gsynth", "multiDiff"), definition = multiDiff:::extract.gsynth)
#'   screenreg(res)
#'   screenreg(l=res_MC)
#' }
#'
#'@noRd
mdd_texreg_export <- function() {
  # if(require(texreg)) {
    # setMethod("extract", signature = className("gsynth", "multiDiff"), definition = multiDiff:::extract.gsynth)
    cat('setMethod("extract", signature = className("gsynth", "multiDiff"), definition = multiDiff:::extract.gsynth)\n')
  cat('setMethod("extract", signature = className("synthdid_estimate", "multiDiff"), definition = multiDiff:::extract.synthdid)\n')
  # }
}
## mdd_texreg_export()


################################
#'## test
################################

if(FALSE){
  library(multiDiff)
  library(synthdid)
  library(gsynth)
  library(texreg)

  mdd_data <- sim_dat_common(as_mdd = TRUE, Time = 20, timing_treatment = 15:20)

  mdd_DD <- mdd_DD_simple(mdd_data)
  mdd_synthDD <- mdd_synthdid(mdd_data)
  mdd_gs <- mdd_gsynth(mdd_data, echo=FALSE, se=TRUE, r=0:3)
  mdd_gs_mc <- mdd_gsynth(mdd_data, echo=FALSE, estimator="mc", se=FALSE)

  all <- list(mdd_DD=mdd_DD, mdd_synthDD=mdd_synthDD, mdd_gs=mdd_gs, mdd_gs_mc=mdd_gs_mc)
  # extr_dd_here <- extract_mdd_DiD
  # extr_sdd_here <- extract.synthdid
  # extr_gdd_here <- extract.gsynth

  extr_dd_here <- multiDiff:::extract_mdd_DiD
  extr_sdd_here <- multiDiff:::extract.synthdid
  extr_gdd_here <- multiDiff:::extract.gsynth
  setMethod("extract", signature = className("mdd_DiD", "multiDiff"), definition = extr_dd_here) # see https://github.com/leifeld/texreg/issues/200
  setMethod("extract", signature = className("synthdid_estimate", "multiDiff"), definition = extr_sdd_here)
  setMethod("extract", signature = className("gsynth", "multiDiff"), definition = extr_gdd_here)
  # environment(extract_mdd_DiD) <- environment(mdd_data_format)
  screenreg(all)

}
