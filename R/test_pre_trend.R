#' Conduct parallel pre-trends test
#'
#' @template param_mdd_dat
#' @param cluster argument passed to `feols(..., cluster=)`
#'
#' @examples
#'
#' data <- sim_dat_common(N=100, Time=10,
#'                        timing_treatment = 6, perc_treat=0.5)
#'
#' mdd_data <- mdd_data_format(data)
#' mdd_test_pre_trend_means(mdd_dat=mdd_data)
#'
#'@export
mdd_test_pre_trend_means <- function(mdd_dat,
                                     cluster=NULL){

  ## mdd formatting
  if(!inherits(mdd_dat, "mdd_dat")) stop("Data should be formatted with 'mdd_data_format' first ")
  mdd_dat_slot <- attributes(mdd_dat)$mdd_dat_slot
  mdd_vars <- mdd_dat_slot$var_names

  if(mdd_dat_slot$n_seq>2) stop("Not implemented for more than 2 groups")
  if(mdd_dat_slot$is_reversible) warning("reversible treatment? Taking till first treatment")


  ## prep data
  mdd_dat_add <- mdd_dat %>%
    add_group(time.index = mdd_vars$time.index, treat = mdd_vars$treat,
              unit.index = mdd_vars$unit.index, group_rename_maybe=TRUE) %>%
    rename(treat =".group",
           period = !!sym(mdd_vars$time.index),
           y= !!sym(mdd_vars$y_var)) %>%
    mutate(treat =as.factor(.data$treat),
           period = as.factor(.data$period))

  ## Estimate ES regression
  reg_out <- fixest::feols(y~ -1+period:treat,
                           cluster = cluster,
                           data=mdd_dat_add)

  nam_coef <- names(coef(reg_out))
  K <- length(nam_coef)

  ## get pre-preiods
  pre_periods <- mdd_dat_slot$periods[mdd_dat_slot$periods < min(mdd_dat_slot$treated_periods)]

  ## H1: individual pairs
  H_pairs <- sapply(pre_periods, \(i) paste(nam_coef[c(i,i+(K/2))], collapse =" - "))

  ## internal checkI got it right
  nam_check_1 <- purrr::map(stringr::str_split(H_pairs, " = "), \(str) lapply(stringr::str_split(str, ":"), \(i) i[[1]]) %>% unlist())
  if(!all(map_int(nam_check_1, dplyr::n_distinct)==1)) {
    stop("Problem with creating hypo H1, vector is: ", H_pairs[1])
  }

  ## H2: join tests
  H2_char <- paste(H_pairs[-1],"=",  H_pairs[1])

  test_joint <- car::linearHypothesis(reg_out, hypothesis.matrix = H2_char)
  test_indiv <- purrr::map_dfr(H2_char, \(i) car::linearHypothesis(reg_out, hypothesis.matrix = i) %>%
                          broom::tidy())

  ## assemble
  rbind(test_joint %>% broom::tidy() %>%
          distinct(.data$statistic, .data$p.value) %>%
          mutate(term = paste(H2_char, collapse = "AND")),
        test_indiv %>%
          distinct(.data$term, .data$statistic, .data$p.value)) %>%
    mutate(test = c("test_joint", rep("test_indiv", nrow(test_indiv)))) %>%
    dplyr::relocate("test")
}

if(FALSE){
  library(multiDiff)

  ##
  df <- sim_dat_common(N=100, Time=10,
                       timing_treatment = 6, perc_treat=0.5)
  df_mdd <- mdd_data_format(df)
  plot(df_mdd)


  ##
  mdd_test_pre_trend_means(mdd_dat = df_mdd)


  library(fixest)
  library(tidyverse)

  data <- multiDiff::sim_dat_staggered(N=100, perc_always = 0, Time=10,
                            timing_treatment = 6, perc_treat=0.5)
  data %>%
    count(Time, tr) %>%
    spread(tr, n)

  DD <- feols(y~tr|unit+Time, cluster = "unit", data=data)
  DD
  data_modif <- data %>%
    group_by(unit) %>%
    mutate(type = if_else(any(tr==1), "Treat", "Control"),
           time_fac = as.factor(Time)) %>%
    select(y, type, tr, Time, unit,time_fac) %>%
    ungroup()

  DD <- feols(y~tr|unit+Time, cluster = "unit", data=data)

  ##
  means_manu <- data_modif %>%
    group_by(Time, type) %>%
    summarise(y = mean(y)) %>%
    arrange(type)
  means_manu%>%
    spread(type, y) %>%
    mutate(diff =Control-Treat)
  cf <- feols(y~-1+time_fac:type, cluster = "unit", data=data_modif) %>%
    coef()
  all.equal(cf, means_manu$y, check.attributes=FALSE)
  -0.0840--0.292
  #
  lm(y~-1+time_fac*type, data=data_modif)

  ## Full means
  lm(y~-1+time_fac:type, data=data_modif)

  ## aov?
  ao <- aov(y~time_fac+type, data=data_modif)
  summary(aov(y~time_fac:type, data=data_modif))
  TukeyHSD(ao, which = "time_fac")

  ### Full
  treat.group.index = quo(type)
  time.index <- quo(Time)
  mdd_test_pre_trend_means(df=data_modif, cluster=NULL)
}
