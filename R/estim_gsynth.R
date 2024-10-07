#' Run Generalized Synthetic Control Method (Xu 2017)
#'
#' @description Simple wrapper for  \link[gsynth]{gsynth}, see help file there
#' @details Note that package gsynth is not imported, only suggested.
#' @template param_mdd_dat
#' @param echo Whether to print the messages of gsynth or not. Default to TRUE
#' @param parallel Argument passed to \link[gsynth]{gsynth}, default is FALSE.
#' @param ... Further arguments passed to \link[gsynth]{gsynth}
#' @seealso \link{mdd_synthdid} for the synthetic diff-diff by Arkhangelsky et al (2019).
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

#' @export
tidy.gsynth <- function(x, type = c("time", "average"), ...){

  type <- match.arg(type)
  if(type=="time"){
    if(is.null(x$est.att)){
      co <- data.frame(estimate=x$att,
                       term=names(x$att))
    } else {
      co <- x$est.att %>%
        as.data.frame(estimate=) %>%
        mutate(term=rownames(.)) |>
        dplyr::rename(estimate=.data$ATT)
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
        dplyr::rename(estimate=.data$Estimate)
    }
  }

  out <-  co |>
    tibble::as_tibble() |>
    dplyr::relocate(.data$term,
                    tidyselect::any_of("n.Treated"))
  if(!is.null(x$est.avg)){
    out <- out %>%
      dplyr::relocate(.data$p.value, .after =.data$S.E.) |>
      dplyr::rename(std.error = .data$S.E.,
                    conf.low = .data$CI.lower,
                    conf.high = .data$CI.upper)
  }

  out
}


if(FALSE){
  require(multiDiff)
  require(gsynth)
  # data(gsynth)
  mdd_simdata_gs <- mdd_data_format(simdata,
                                 y_var = "Y",time.index = "time",
                                 treat = "D", unit.index = "id")
  res <- mdd_gsynth(mdd_dat=mdd_simdata_gs,
                    force = "two-way",
                    CV = FALSE, r = 2, se = TRUE,
                    inference = "parametric", nboots = 1000,
                    parallel = FALSE)
  res
  tidy(x=res, type ="time")
  tidy(x=res, type ="average")
}
