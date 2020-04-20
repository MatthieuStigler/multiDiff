#' Decomposition of fixed-effects weights
#'
#' @template param_all
#' @param by over which dimension to do it?
#' @examples
#' library(lfe)
#'  dat_sim_1 <- sim_dat(N=100, Time = 5)
#' coef_FE_time <- coef(felm(y~tr|Time, data = dat_sim_1))
#' coef_FE_unit <- coef(felm(y~tr|unit, data = dat_sim_1))
#'
#' coefs_by_Y <- FE_decompo(data=dat_sim_1,
#' treat = "tr",
#'                       by = "time")
#' coefs_by_U <- FE_decompo(data=dat_sim_1,
#'               treat = "tr",
#'                       by = "unit")
#' with(coefs_by_Y, weighted.mean(treat_coef, treat_weight))
#' coef_FE_time

#' with(coefs_by_U, weighted.mean(treat_coef, treat_weight))
#' coef_FE_unit
#'
#'@export
FE_decompo <- function(data, y_var="y", time.index = "Time", treat = "tr", unit.index="unit",
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
  reg_FE1_state_year <- felm(prestout ~treat|state+year,
                             data = filter(GentzkowData2, n_state_year>4))
  reg_FE_lfe

  ## by Time
  coefs_by_Y <- FE_decompo(data=GentzkowData2,
                           y_var="prestout",
                           time.index = "year",
                           treat = "treat",
                           unit.index="cnty90",
                           by = "time")

  coefs_by_Y

  reg_FE1_time_w <- with(coefs_by_Y, weighted.mean(treat_coef, treat_weight))
  all.equal(reg_FE1_time_w, coef(reg_FE1_time),check.attributes=FALSE)
  reg_FE1_time

  ## by time and unit (higher unit)
  coefs_by_YC <- GentzkowData2 %>%
    filter(n_state_year>4) %>%
    # arrange(n_state_year)
    FE_decompo(y_var="prestout",
               time.index = "year",
               treat = "treat",
               unit.index="state",
               by = "both")
  sum(coefs_by_YC$n_vals)

  reg_FE1_state_year
  reg_FE1_state_year_w <- with(coefs_by_YC, weighted.mean(treat_coef, treat_weight))
  all.equal(reg_FE1_state_year_w, coef(reg_FE1_state_year),check.attributes=FALSE)

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
