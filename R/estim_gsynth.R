#' Run Generalized Synthetic Control Method (Xu 2017)
#'
#' @description Simple wrapper for  \code{\link[gsynth]{gsynth}} or \code{\link[fect]{fect}}, see help files there.
#' @details Note that packages gsynth/fect are not imported, only suggested.
#' @template param_mdd_dat
#' @param echo Whether to print the messages of gsynth or not. Default to TRUE
#' @param parallel Argument passed to \link[gsynth]{gsynth}, default is FALSE.
#' @param ... Further arguments passed to \link[gsynth]{gsynth}
#' @seealso \code{\link{mdd_synthdid}} for the synthetic diff-diff by Arkhangelsky et al (2019).
#' @examples
#' if(require(gsynth)){
#'  data(gsynth)
#'  mdd_simdata_gs <- mdd_data_format(simdata,
#'                                    y_var = "Y",time.index = "time",
#'                                    treat = "D", unit.index = "id")
#'  res <- mdd_gsynth(mdd_dat=mdd_simdata_gs)
#' }
#' @export
mdd_gsynth <- function(mdd_dat, echo=FALSE, parallel=FALSE, ...){

  if(!rlang::is_installed("gsynth")) stop("Please install package 'gsynth' from CRAN")

  ## prep data
  mdd_dat_slot <- intrnl_mdd_get_mdd_slot(mdd_dat)
  mdd_vars <- mdd_dat_slot$var_names
  formu <- as.formula(paste(mdd_vars$y_var, "~", mdd_vars$treat))

  ## run
  if(!echo) sink(tempfile())  # Redirect output to a temporary file
  res <- gsynth::gsynth(formu, data = as.data.frame(mdd_dat),
                        index = c(mdd_vars$unit.index, mdd_vars$time.index),
                        parallel=parallel,
                        ...)
  if(!echo) sink()  # Restore output to the console


  ##
  attr(res, "mdd_dat_slot") <- mdd_dat_slot
  class(res) <- c(class(res), "mdd_gsynth")
  res
}

#'@export
#'@rdname mdd_gsynth
mdd_estim_fect <- function(mdd_dat, echo=FALSE, parallel=FALSE, ...){

  if(!rlang::is_installed("fect")) stop("Please install package 'gsynth' from CRAN")

  ## prep data
  mdd_dat_slot <- intrnl_mdd_get_mdd_slot(mdd_dat)
  mdd_vars <- mdd_dat_slot$var_names
  formu <- as.formula(paste(mdd_vars$y_var, "~", mdd_vars$treat))

  ## run
  run_with_message <- function(x, echo=TRUE) {
    if(!echo) return(suppressMessages(x))
    x
  }
  if(!echo) sink(tempfile())  # Redirect output to a temporary file
  res <- run_with_message(fect::fect(formu, data = as.data.frame(mdd_dat),
                                     index = c(mdd_vars$unit.index, mdd_vars$time.index),
                                     parallel=parallel,
                                     ...), echo=echo)
  if(!echo) sink()  # Restore output to the console


  ##
  attr(res, "mdd_dat_slot") <- mdd_dat_slot
  class(res) <- c(class(res), "mdd_fect")
  res
}

#' @export
coef.gsynth <- function(object, type = c("average", "time"), ...){
  co <- switch(match.arg(type),
               "time"=object$att,
               "average"= object$att.avg)
  co
}

#' @export
coef.fect <- function(object, type = c("average", "time"), ...){
  coef.gsynth(object, type=type, ...)
}

#' @export
tidy.gsynth <- function(x, type = c("average", "time"), ...){

  type <- match.arg(type)
  if(type=="time"){
    if(is.null(x$est.att)){
      co <- data.frame(estimate=x$att,
                       term=names(x$att))
    } else {
      co <- x$est.att %>%
        as.data.frame(estimate=) %>%
        mutate(term=rownames(.)) |>
        dplyr::rename(estimate="ATT")
    }
  } else if(type=="average"){
    if(is.null(x$est.avg)){
      co <- data.frame(estimate=x$att.avg,
                       term="Average ATT")
    } else {
      # small bug: described as est.att.avg but actually est.avg !?
      co <- x$est.avg %>%
        as.data.frame() %>%
        mutate(term="Average ATT") |>
        dplyr::rename(estimate="Estimate")
    }
  }

  out <-  co |>
    tibble::as_tibble() |>
    dplyr::relocate("term",
                    tidyselect::any_of("n.Treated"))
  if(!is.null(x$est.avg)){
    out <- out %>%
      dplyr::relocate("p.value", .after ="S.E.") |>
      dplyr::rename(std.error = "S.E.",
                    conf.low = "CI.lower",
                    conf.high = "CI.upper")
  }

  out
}

#' @export
tidy.fect <- function(x, type = c("average-obs", "average-unit", "time"), ...){

  type <- match.arg(type, choices = c("average-obs", "average-unit", "average", "time"))
  ## allow
  if(type=="average") type <- "average-obs"

  if(type=="time"){
    if(is.null(x$est.att)){
      co <- data.frame(estimate=x$att,
                       term= x$time)
    } else {
      co <- x$est.att %>%
        as.data.frame(estimate=) %>%
        mutate(term=rownames(.)) |>
        dplyr::rename(estimate="ATT")
    }
  } else if(type%in% c("average-obs", "average-unit")){
    names_att_avg <- switch(type,
                           "average-obs"="Average ATT (by obs)",
                           "average-unit"="Average ATT (by unit)")
    if(is.null(x$est.avg)){
      coefs_here <- ifelse(type=="average-obs", x$att.avg,x$att.avg.unit)
      co <- data.frame(estimate=coefs_here,
                       term= names_att_avg)
    } else {
      # small bug: described as est.att.avg but actually est.avg !?
      co_M <- if(type=="average-obs")  x$est.avg else x$est.avg.unit
      co <- as.data.frame(co_M) %>%
        mutate(term=names_att_avg) |>
        dplyr::rename_with(~str_replace(., "ATT\\..+", "estimate"))
    }
  }

  out <-  co |>
    tibble::as_tibble() |>
    dplyr::relocate("term",
                    tidyselect::any_of("n.Treated"))
  if(!is.null(x$est.avg)){
    out <- out %>%
      dplyr::relocate("p.value", .after ="S.E.") |>
      dplyr::rename(std.error = "S.E.",
                    conf.low = "CI.lower",
                    conf.high = "CI.upper")
  }

  out
}



if(FALSE){
  library(multiDiff)
  # require(gsynth)
  library(fect)
  # data(fect)
  mdd_simdata_gs <- mdd_data_format(simdata1,
                                    y_var = "Y",time.index = "time",
                                    treat = "D", unit.index = "id")
  res_gs <- mdd_gsynth(mdd_dat=mdd_simdata_gs,
                    force = "two-way")
  res_gs_se <- mdd_gsynth(mdd_dat=mdd_simdata_gs,
                    force = "two-way",
                    CV = FALSE, r = 2, se = TRUE,
                    inference = "parametric", nboots = 10)

  res_fect <- mdd_estim_fect(mdd_dat=mdd_simdata_gs, method = "gsynth", r=2)
  res_fect_se <- mdd_estim_fect(mdd_dat=mdd_simdata_gs, se=TRUE, nboots = 10, method = "gsynth", r=2)

  all_mdls <- tibble::lst(res_gs, res_gs_se, res_fect, res_fect_se)
  purrr::map_int(all_mdls, ~.$r.cv)

  purrr::map_dbl(all_mdls, coef, type ="average")
  purrr::map(all_mdls, coef, type ="time")

  purrr::map(all_mdls, tidy, type ="average")
  purrr::map(all_mdls, tidy, type ="time")
}
