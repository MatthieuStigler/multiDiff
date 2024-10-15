################################
### Did here
################################

extract_mdd_DiD <- function(model, add_para_test = FALSE, include.n_treated=TRUE,
                            include.Ttreated=FALSE,
                            include.rsquared = FALSE, include.adjrs = FALSE, include.deviance=FALSE,
                            include.groups = FALSE,
                            ...) {

  ## call fixest method
  texreg_extract.fixest <- get("extract.fixest", envir = asNamespace("texreg"))
  extr_out <- texreg_extract.fixest(model,
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
### gsynth
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


extract.fect <- function(model, type = c("average-obs", "average-unit", "time"),
                           include.n_treated=TRUE, include.nobs = TRUE,
                           include.hyper =TRUE, ...) {

  extract.gsynth(model=model, type=type,
                 include.n_treated=include.n_treated, include.nobs=include.nobs,
                 include.hyper=include.hyper, ...)
}

################################
### synthdid
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

  tr <- texreg::createTexreg(coef.names = "Treatment",
                             coef = co,
                             se = se,
                             pvalues = pval)
  return(tr)
}


mdd_texreg_export <- function() {
  # if(require(texreg)) {
  # setMethod("extract", signature = className("gsynth", "multiDiff"), definition = multiDiff:::extract.gsynth)
  cat('setMethod("extract", signature = className("gsynth", "multiDiff"), definition = multiDiff:::extract.gsynth)\n')
  cat('setMethod("extract", signature = className("synthdid_estimate", "multiDiff"), definition = multiDiff:::extract.synthdid)\n')
  # }
}

if(FALSE){
 mdd_texreg_export()
  setMethod("extract", signature = className("mdd_DiD", "multiDiff"), definition = multiDiff:::extract_mdd_DiD) # see https://github.com/leifeld/texreg/issues/200
  setMethod("extract", signature = className("synthdid_estimate", "multiDiff"), definition = multiDiff:::extract.synthdid)
  setMethod("extract", signature = className("gsynth", "multiDiff"), definition = multiDiff:::extract.gsynth)

}

