utl_char_to_num <- function(x) match(x, unique(x))

if(FALSE){
  utl_char_to_num(rep(LETTERS[1:5], each=3))
}


#' Apply CS estimate
#'
#' @template param_mdd_dat
#' @param ... further objects passed to \code{\link[did]{att_gt}}
#' @examples
#' # example code
#' dat <- sim_dat_staggered(as_mdd = TRUE, Time=6)
#' mdd_CS(dat)
#' @export
mdd_CS <- function(mdd_dat, ...){

  requireNamespace("did")
  vars <- intrnl_mdd_get_mdd_slot(mdd_dat)$var_names

  ## conv to did format
  dat_did <- mdd_conv_mdd_to_did(mdd_dat)
    # mutate(treat_timing2= if_else(.data$treat_timing>0, min(.[[vars$time.index]])+.data$treat_timing, 0))

  ## make unit.index (idname) numeric
  if(!is.numeric(dat_did[[vars$unit.index]])){
    nam <- vars$unit.index
    dat_did <- dat_did %>%
      mutate({{nam}} := utl_char_to_num(vars$unit.index))
  }

  ## now run
  did::att_gt(yname = vars$y_var, tname = vars$time.index, idname = vars$unit.index,
              gname = "treat_timing",
              data = dat_did,
              ...)
}

if(FALSE){
  dat <- sim_dat_staggered(as_mdd = TRUE, Time=6)
  dat
  environment(mdd_CS) <- environment(mdd_data_format)
  mdd_CS(dat)

}
