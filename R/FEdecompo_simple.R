#' Decomposition of fixed-effects weights
#'
#' @template param_all
#' @param by over which dimension to do it?
#' @param fixed_effects The fixed effects to include, either time, unit ot both.
#' This will use the values indicated in \code{time.index} and \code{unit.index}
#' @examples
#' library(lfe)
#'  dat_sim_1 <- sim_dat(N=100, Time = 5)
#' coef_FE_time <- coef(felm(y~tr|Time, data = dat_sim_1))
#' coef_FE_unit <- coef(felm(y~tr|unit, data = dat_sim_1))
#'
#' coefs_by_Y <- FE_decompo(data=dat_sim_1,
#'                          treat = "tr",
#'                          by = "Time",
#'                          fixed_effects = "time")
#' coefs_by_U <- FE_decompo(data=dat_sim_1,
#'                          treat = "tr",
#'                          by = "unit",
#'                          fixed_effects = "unit")
#' with(coefs_by_Y, weighted.mean(treat_coef, treat_weight))
#' coef_FE_time

#' with(coefs_by_U, weighted.mean(treat_coef, treat_weight))
#' coef_FE_unit
#'
#'@export
FE_decompo <- function(data, y_var="y", time.index = "Time", treat = "tr", unit.index="unit",
                           fixed_effects =c("time", "unit", "both"),
                           by = unit.index) {
  fixed_effects <-  match.arg(fixed_effects)
  fixed_effects_index <- switch(fixed_effects, time=time.index, unit = unit.index, both = c(unit.index, time.index))
  treat_quo <- rlang::ensym(treat)

  formu <- as.formula(paste(y_var, "~", treat, "-1"))

  ## demeanlist wants a list of factor
  fixed_effects_list <- data %>%
    select_at(fixed_effects_index) %>%
    mutate_all(as.factor) %>%
    as.list()

  ## Demean data Y and D
  dat_demeaned <- lfe::demeanlist(data %>%
                                    select_at(c(y_var, treat)),
                                  fixed_effects_list) %>%
    as_tibble() %>%
    dplyr::bind_cols(select_at(data, c(fixed_effects_index, by)))

  ## add dim in case
  if(!all(by %in% fixed_effects_index)) warning("Check")

  ## Estimate specific betas
  dat_coefs <- dat_demeaned %>%
    group_by_at(by) %>%
    group_modify(~bind_cols(broom::tidy(lm(formu, data=.x), quick=TRUE),
                            summarise(.x,
                                      treat_var = var({{treat_quo}}),
                                      n_vals = n()))) %>%
    ungroup() %>%
    select(-.data$term) %>%
    rename(treat_coef = .data$estimate)

  ## check only 1 vals?
  if(any(dat_coefs$n_vals==1)) warning("Cells (", sum(dat_coefs$n_vals==1), ") with only one observation")

  ## add weights
  dat_coefs_w <- dat_coefs %>%
    mutate(treat_weight=.data$treat_var*(.data$n_vals-1),
           treat_weight= .data$treat_weight/sum(.data$treat_weight)) %>%
    select(tidyselect::one_of(by), .data$n_vals, tidyselect::everything())

  dat_coefs_w

}

FE_decompo_old <- function(data, y_var="y", time.index = "Time", treat = "tr", unit.index="unit",
                           by = c("time", "unit", "both")) {
  by <-  match.arg(by)
  by_index <- switch(by, time=time.index, unit = unit.index, both = c(unit.index, time.index))
  treat_quo <- rlang::ensym(treat)

  formu <- as.formula(paste(y_var, "~", treat))

  data %>%
    dplyr::group_by_at(by_index) %>%
    dplyr::group_modify(~bind_cols(summarise(.x, n_vals = n()),
                                   broom::tidy(lm(formu, data=.x), quick=TRUE) %>%
                                     filter(str_detect(term, treat))%>%
                                     spread(term, .data$estimate) %>%
                                     rename(treat_coef = {{treat_quo}}),
                                   summarise(.x, treat_var = var({{treat_quo}})))) %>%
    ungroup() %>%
    mutate(treat_weight=.data$treat_var*(.data$n_vals-1),
           treat_weight= .data$treat_weight/sum(.data$treat_weight))

}


if(FALSE){
  intrnl_ave <-  function(df) with(df, weighted.mean(treat_coef, treat_weight))
  intrnl_check <-  function(df, reg) {
    me <- with(df, weighted.mean(treat_coef, treat_weight))
    all.equal(me, coef(reg), check.attributes=FALSE)
  }

  library(multiDiff)
  library(tidyverse)
  #data(GentzkowData)
  GentzkowData

  ## The FE from R is slightly different,
  ## they use some strange formulation of fixed effects in their STATA areg code
  library(lfe)
  GentzkowData2 <- GentzkowData %>%
    mutate(treat = as.integer(numdailies>0),
           county = str_pad(as.character(cnty90), width = 5, pad = "0"),
           state = str_sub(county, end = 2)) %>%
    add_count(state, year, name = "n_state_year")
  reg_FE_lfe <- felm(prestout ~treat|cnty90 + year, data = GentzkowData2)
  reg_FE1_time <- felm(prestout ~treat|year, data = GentzkowData2)
  reg_FE1_unit <- felm(prestout ~treat|cnty90, data = GentzkowData2)
  reg_FE2 <- felm(prestout ~treat|cnty90+year, data = GentzkowData2)
  reg_FE2_n2 <- felm(prestout ~treat|cnty90+year,
                     data = filter(GentzkowData2, n_state_year>1))
  reg_FE2_state_year <- felm(prestout ~treat|state+year,
                             data = filter(GentzkowData2, n_state_year>1))
  reg_FE_lfe

  ## by Time
  coefs_by_Y <- FE_decompo(data=GentzkowData2,
                           y_var="prestout",
                           time.index = "year",
                           treat = "treat",
                           unit.index="cnty90",
                           by = "time")
  coefs_by_Y_new <- FE_decompo_new(data=GentzkowData2,
                           y_var="prestout",
                           time.index = "year",
                           treat = "treat",
                           unit.index="cnty90",
                           by = "year",
                           fixed_effects = "time")
  coefs_by_Y
  coefs_by_Y_new

  intrnl_check(coefs_by_Y, reg_FE1_time)
  intrnl_check(coefs_by_Y_new, reg_FE1_time)

  ## By unit
  coefs_by_unit <- FE_decompo_new(data=GentzkowData2,
                                   y_var="prestout",
                                   time.index = "year",
                                   treat = "treat",
                                   unit.index="cnty90",
                                   by = "cnty90",
                                  fixed_effects = "unit")
  intrnl_check(coefs_by_unit, reg_FE1_unit)

  ## by time
  coefs_by_Y_FE2 <- GentzkowData2 %>%
    FE_decompo_new(y_var="prestout",
               time.index = "year",
               treat = "treat",
               unit.index="cnty90",
               fixed_effects = "both",
               by = "year")

  coefs_by_N_FE2 <- GentzkowData2 %>%
    FE_decompo_new(y_var="prestout",
                   time.index = "year",
                   treat = "treat",
                   unit.index="cnty90",
                   fixed_effects = "both",
                   by = "cnty90")

  intrnl_check(coefs_by_Y_FE2, reg_FE2)
  intrnl_check(coefs_by_N_FE2, reg_FE2)

  ## higher FEs? Yes for 1!
  coefs_by_YS_FE2 <- GentzkowData2 %>%
    # filter(n_state_year>1) %>%
    FE_decompo_new(y_var="prestout",
                   time.index = "year",
                   treat = "treat",
                   unit.index="cnty90",
                   fixed_effects = "both",
                   by = c("state")) #

  coefs_by_YS_FE2 %>%
    full_join(coefs_by_N_FE2 %>%
                left_join(GentzkowData2 %>%
                            distinct(state, cnty90)) %>%
                group_by(state) %>%
                summarise(treat_coef = weighted.mean(treat_coef, treat_weight),
                          n_vals_2 = sum(n_vals)),
              by = "state")


  intrnl_check(coefs_by_YS_FE2, reg_FE2)
  # intrnl_check(coefs_by_YS_FE2, reg_FE2_n2)


  reg_FE2_state_year
  reg_FE2_state_year_w <- with(coefs_by_YC, weighted.mean(treat_coef, treat_weight))
  all.equal(reg_FE2_state_year_w, coef(reg_FE2_state_year),check.attributes=FALSE)

  coefs_by_C <- FE_decompo(data=GentzkowData2,
                           y_var="prestout",
                           time.index = "year",
                           treat = "treat",
                           unit.index="cnty90",
                           by = "unit")


  sum(coefs_by_Y$n_vals)
  sum(coefs_by_C$n_vals)



  ## unit
  reg_FE1_unit_w <- with(coefs_by_C, weighted.mean(treat_coef, treat_weight))
  reg_FE1_unit
  all.equal(reg_FE1_unit_w, coef(reg_FE1_unit),check.attributes=FALSE)

  ## plot corrs
  coefs_by_C %>%
    ggplot(aes(x=treat_coef, y=treat_weight))+
    geom_point()

  ## both?

}
