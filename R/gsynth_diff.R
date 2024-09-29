#' Run generalized synthetic diff-diff from XXX
#'
#' @template param_mdd_dat
#' @param ... Further argumetns passed to
#'
#' @examples
#' if(require(gsynth)){
#'  data(gsynth)
#'  mdd_simdata_gs <- mdd_data_format(simdata,
#'                                    y_var = "Y",time.index = "time",
#'                                    treat = "D", unit.index = "id")
#'  res <- mdd_gsynth(mdd_dat=mdd_simdata_gs,
#' }
#' @export
mdd_gsynth <- function(mdd_dat, ...){

  ## prep data
  mdd_dat_slot <- multiDiff:::intrnl_mdd_get_mdd_slot(mdd_dat)
  mdd_vars <- mdd_dat_slot$var_names
  formu <- as.formula(paste(mdd_vars$y_var, "~", mdd_vars$treat))

  ## run
  res <- gsynth(formu, data = as.data.frame(mdd_dat),
                index = c(mdd_vars$unit.index, mdd_vars$time.index),
                ...)


  ##
  attr(res, "mdd_dat_slot") <- mdd_dat_slot
  res
}

#' @export
tidy.gsynth <- function(x, type = c("time", "average"), ...){

  type <- match.arg(type)
  if(type=="time"){
    co <- x$est.att %>%
      as.data.frame() %>%
      mutate(term=rownames(.)) |>
      dplyr::rename(estimate=ATT)
  } else if(type=="average"){
    co <- x$est.avg %>%
      as.data.frame() %>%
      mutate(term="Average ATT") |>
      dplyr::rename(estimate=Estimate)
  }

  out <-  co |>
    tibble::as_tibble() |>
    dplyr::relocate(term,
                    tidyselect::any_of("n.Treated")) |>
    dplyr::relocate(p.value, .after =S.E.) |>
    dplyr::rename(std.error = S.E.,
                  conf.low=CI.lower,
                  conf.high =CI.upper)

  out
}


if(FALSE){
  require(multiDiff)
  require(gsynth)
  data(gsynth)
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
