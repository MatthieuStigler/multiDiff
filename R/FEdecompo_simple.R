


# DD <- function(y_var="y", data, time.index = "Time", treat = "tr", unit.index="unit",
#                min_obs_required = 2) {
FE_decompo <- function(data, y_var="y", time.index = "Time", treat = "tr", unit.index="unit",
                       by = c("time", "unit", "both")) {
  by <-  match.arg(by)
  by_index <- switch(by, time=time.index, unit = unit.index, both = c(unit.index, time.index))
  treat_weight <- paste0(treat, "_weight")
  treat_weight_quo <- rlang::ensym(treat_weight)
  # by_index_quo <- rlang::ensym(by_index)

  formu <- as.formula(paste(y_var, "~", treat))

  data %>%
    dplyr::group_by_at(by_index) %>%
    dplyr::group_modify(~bind_cols(summarise(.x, n_vals = n()),
                            broom::tidy(lm(formu, data=.x), quick=TRUE) %>%
                              filter(str_detect(term, treat))%>%
                              mutate(term = paste0(term, "_coef")) %>%
                              spread(term, estimate),
                            summarise_at(.x, tidyselect::vars_select(names(.), starts_with(treat)), list(var=var)))) %>%
    ungroup() %>%
    mutate(treat_weight:=.data$treat_var*(.data$n_vals-1),
           treat_weight:= {{treat_weight_quo}}/sum({{treat_weight_quo}}))

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
           state = str_sub(county, end = 2))
  reg_FE_lfe <- felm(prestout ~treat|cnty90 + year, data = GentzkowData2)
  reg_FE1_time <- felm(prestout ~treat|year, data = GentzkowData2)
  reg_FE1_unit <- felm(prestout ~treat|cnty90, data = GentzkowData2)
  reg_FE_lfe

  ##
  coefs_by_Y <- FE_decompo(data=GentzkowData2,
                           y_var="prestout",
                           time.index = "year",
                           treat = "treat",
                           unit.index="cnty90",
                           by = "time")

  coefs_by_Y

  ## time
  coefs_by_Y
  reg_FE1_time_w <- with(coefs_by_Y, weighted.mean(treat_coef, treat_weight))
  all.equal(reg_FE1_time_w, coef(reg_FE1_time),check.attributes=FALSE)
  reg_FE1_time

  coefs_by_YC <- FE_decompo(data=GentzkowData2,
                            y_var="prestout",
                            time.index = "year",
                            treat = "treat",
                            unit.index="state",
                            by = "both")
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
  with(coefs_by_YC, weighted.mean(coef_treat, treat_var))
  reg_FE_lfe
}
