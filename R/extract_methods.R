extract.gsynth <- function(model, type = c("average", "time"),
                           include.n_treated=TRUE, ...) {

  type <- match.arg(type)
  has_se <- !is.null(model$est.att)

  ## simple way
  out <- tidy(model, type = type)
  co <- out$estimate
  if(has_se) {
    se <- out$std.error
    pval <- out$p.value
  } else {
    se <- numeric(0)
    pval <- numeric(0)
    if(length(co)==1) warning("POssible issue with https://github.com/leifeld/texreg/issues/209")
  }

  ### gof
  hyper <-  model$r.cv
  if(is.null(hyper)) hyper <- model$lambda.cv
  gof <- c(hyper)
  gof.names <- c("N factors:")
  gof.decimal <- FALSE

  if(include.n_treated){
    gof <- c(gof, model$Nco, model$Ntr)
    gof.names <- c(gof.names, "Num. control:", "Num. treated:")
    gof.decimal <- c(gof.decimal, FALSE,FALSE)
  }
  # if(include.force){
  #   force <- c("none", "unit", "time", "two-way")[model$force+1]
  #   gof <- c(gof, force)
  #   gof.names <- c(gof.names, "Force:")
  #   gof.decimal <- c(gof.decimal, NA)
  # }


  tr <- texreg::createTexreg(coef.names = out$term,
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
    coef.names = "treat_status",
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
