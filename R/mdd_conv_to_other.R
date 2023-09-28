#' Convert did to mdd
#'
#' @examples
#' library(did)
#' dat_sim <- build_sim_dataset(reset.sim(time.periods=4))
#' mdd_conv_did_to_mdd(dat_sim)
#' @noRd
mdd_conv_did_to_mdd <- function(data, yname = "Y", tname = "period",
                                idname = "id",
                                gname  = "G"){

  # tname -> time.index
  # idname -> unit.index
  # y_name -> y_var
  # gname transform for treat
  data |>
    mutate(tr = if_else(!!sym(tname)>=!!sym(gname) & !!sym(gname) >0,1,0)) |>
    mdd_data_format(y_var = yname, time.index = tname,
                    treat = "tr", unit.index = idname)
}

#' Convert mdd to did
#'
#'@examples
#' DID_dat_raw <- sim_dat_common(as_mdd=TRUE, timing_treatment = 7:10)
#' dat_for_did <- multiDiff:::mdd_conv_mdd_to_did(DID_dat_raw)
#' library(did)
#' did_out <- att_gt(yname = "y",
#'                   tname = "Time",
#'                   idname = "unit",
#'                   gname = "treat_timing",
#'                   data = dat_for_did,
#'                   bstrap = FALSE, cband = FALSE, est_method = "reg")
#' did_out
#' @noRd
mdd_conv_mdd_to_did <- function(mdd_data){
  mdd_data |>
    intrnl_add_treat_time_mdd() |>
    as.data.frame()
}
