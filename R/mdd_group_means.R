#' Compute group means by time period
#'
#' Compute the means of each group for wach time periods
#'
#' @template param_mdd_dat
#' @param conf.int Weather to compute confidence intervals for the means
#' @examples
#'  library(multiDiff)
#'  dat_DiD_raw <- sim_dat_common()
#'
#'  ## format data
#'  dat_DiD <- mdd_data_format(data=dat_DiD_raw)
#'
#'  mdd_group_means(dat_DiD)
#'  mdd_group_means(dat_DiD, conf.int = TRUE)

#' @export
mdd_group_means <- function(mdd_dat, conf.int=FALSE) {

  if(!inherits(mdd_dat, "mdd_dat")) stop("Data should be formatted with 'mdd_data_format' first ")
  mdd_dat_slot <- intrnl_mdd_get_mdd_slot(mdd_dat)
  mdd_vars <- mdd_dat_slot$var_names

  # add groups
  mdd_dat_add <- mdd_dat %>%
    add_group(time.index = mdd_vars$time.index, treat = mdd_vars$treat,
              unit.index = mdd_vars$unit.index, group_rename_maybe=TRUE)

  ## Compute means
  if(!conf.int) {
    res_means <- mdd_dat_add %>%
      group_by(dplyr::across(c(".group", mdd_vars$time.index))) %>%
      summarise(dplyr::across(mdd_vars$y_var, mean),
                .groups = "drop") %>%
      arrange(.data$.group, all_of(mdd_vars$time.index))

  } else {
    formu <- as.formula(paste(mdd_vars$y_var, "~1"))
    res_means <- mdd_dat_add %>%
      group_by(dplyr::across(c(".group", mdd_vars$time.index))) %>%
      nest() %>%
      mutate(data = map(.data$data, ~lm(formu, data =.) %>%
                          broom::tidy(conf.int=TRUE))) %>%
      unnest(.data$data) %>%
      ungroup() %>%
      relocate(all_of(c(".group", mdd_vars$time.index))) %>%
      select(-"term") %>%
      dplyr::rename_with(~str_replace(., "^estimate$", mdd_vars$y_var)) %>%
      arrange(.data$.group, all_of(mdd_vars$time.index))
  }

  ## return result
  res_means
}


if(FALSE){
  library(multiDiff)
  dat_DiD_raw <- sim_dat_common()
  dat_DiD <- mdd_data_format(data=dat_DiD_raw)

  ## to mdd
  environment(mdd_group_means) <- environment(mdd_event_study)
  mdd_group_means(dat_DiD)
  mdd_group_means(dat_DiD, conf.int = TRUE)

}
