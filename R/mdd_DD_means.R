#' Estimate DiD 2x2 means
#' @template param_mdd_dat
#' @param add_tests compute also tests?
#' @examples
#' dat_sim <- sim_dat_common(timing_treatment = 6:10)
#' dat_sim_mdd <- mdd_data_format(dat_sim)
#' out <- mdd_DD_means22(mdd_dat=dat_sim_mdd)
#' @export
mdd_DD_means22 <- function(mdd_dat, add_tests = TRUE){

  ## mdd_dat formatting
  if(!inherits(mdd_dat, "mdd_dat")) stop("Data should be formatted with 'mdd_data_format' first ")
  mdd_dat_slot <- intrnl_mdd_get_mdd_slot(mdd_dat)
  mdd_vars <- mdd_dat_slot$var_names

  ## only works for classical for now
  if(mdd_dat_slot$DID_type!="classical") stop("mdd_DD_means only implemented for `classical` DiD design")

  ## add data
  pre_periods <- intrnl_mdd_get_pre_periods(mdd_dat)

  mdd_dat_extended <- mdd_dat %>%
    intrnl_add_treat_status_mdd() %>%
    mutate(treat_period = if_else(!!sym(mdd_vars$time.index) %in% pre_periods, "pre", "post"),
           pre_Untreat = as.numeric(.data$treat_period=="pre" & .data$treat_categ !="Treat"),
           pre_Treat = as.numeric(.data$treat_period=="pre" & .data$treat_categ =="Treat"),
           post_Untreat=as.numeric(.data$treat_period=="post" & .data$treat_categ !="Treat"),
           post_Treat=as.numeric(.data$treat_period=="post" & .data$treat_categ =="Treat"))

  ## regression now
  formu_here <- paste0(mdd_vars$y_var, " ~ -1 + pre_Untreat + pre_Treat + post_Untreat + post_Treat")
  reg_2_2 <- stats::lm(formu_here, data=mdd_dat_extended)


  ## add tests
  if(add_tests){
    diff_pre <-     car::linearHypothesis(reg_2_2, hypothesis.matrix = c(-1,1, 0, 0)) %>% broom::tidy()
    diff_post <-    car::linearHypothesis(reg_2_2, hypothesis.matrix = c(0, 0, -1,1)) %>% broom::tidy()
    diff_treat <-   car::linearHypothesis(reg_2_2, hypothesis.matrix = c(-1, 0, 1,0)) %>% broom::tidy()
    diff_untreat <- car::linearHypothesis(reg_2_2, hypothesis.matrix = c(0, -1, 0,1)) %>% broom::tidy()
    diff_diff <-    car::linearHypothesis(reg_2_2, hypothesis.matrix = c(1, -1, -1,1)) %>% broom::tidy()

    tests <- rbind(diff_pre,
                   diff_post,
                   diff_treat,
                   diff_untreat,
                   diff_diff)

  }

  ## add to res
  mdd_dat_slot$DiD_manu_tests <- tests

  ##
  class(reg_2_2) <- c("mdd_DD_means22", "mdd_DiD", class(reg_2_2))
  attr(reg_2_2, "mdd_dat_slot") <- mdd_dat_slot
  reg_2_2
}



#' @param x output of mdd_DD_means
#' @param ... unused
#' @export
#' @rdname mdd_DD_means
print.mdd_DD_means22 <- function(x, ...){
  stats::printCoefmat(coef(summary(x)))
  tests_out <- intrnl_mdd_get_mdd_slot(x)$DiD_manu_tests %>%
    select(-all_of(c("rss", "df", "sumsq")))
  print(tests_out)
}


if(FALSE){

  ## example
  dat_sim <- sim_dat_common(timing_treatment = 6:10)
  dat_sim_mdd <- mdd_data_format(dat_sim)


  out <- mdd_DD_means22(mdd_dat=dat_sim_mdd)
  out

  ## doat other
  dat_sim_alter <- sim_dat_common(timing_treatment = 6:10) |>
    dplyr::rename(id=unit, year=Time, treat_var=tr, out=y)|>
    dplyr::select(-treat_group) |>
    mdd_data_format(y_var = "out", time.index = "year", treat = "treat_var", unit.index = "id")
  mdd_DD_means22(dat_sim_alter)
}

