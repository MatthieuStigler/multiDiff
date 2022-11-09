#' Conduct parallel pre-trends test
#'
#' @param data data-frame, containing only pre-intervention values
#' @template param_y_var
#' @template param_treat
#' @template param_time.index
#' @template param_unit.index
#' @param cluster argument passed to `feols(..., cluster=)`
#'
#' @examples
#'
#' data <- sim_dat_common(N=100, Time=10,
#'                        timing_treatment = 6, perc_treat=0.5)
#'  library(tidyverse, warn.conflicts = FALSE)
#'  data_modif <- data %>%
#'    group_by(unit) %>%
#'    mutate(type = if_else(any(tr==1), "Treat", "Control")) %>%
#'    ungroup() %>%
#'    select(y, type, Time, unit)
#'
#' #
#' mdd_test_pre_trend_means(data=data, unit.index = "unit",
#'                          time.index=Time, cluster = "unit")
#'
#'@export
mdd_test_pre_trend_means <- function(data,
                                     unit.index,
                                     time.index,
                                     y_var = "y",
                                     treat = "tr",
                                     cluster=NULL){

  ## prep data
  dat_prep <- data %>%
    add_group(time.index = {{time.index}},
              treat = {{treat}}, unit.index = {{unit.index}},
              raw=FALSE) %>%
    rename(treat =".group",
           period = {{time.index}},
           y= {{y_var}}) %>%
    mutate(treat =as.factor(.data$treat),
           period = as.factor(.data$period))

  ##
  reg_out <- fixest::feols(y~ -1+period:treat,
                           cluster = cluster,
                           data=dat_prep)

  nam <- names(coef(reg_out))
  K <- length(nam)

  ## H1: individual tests
  H1_char <- sapply(1:(K/2), \(i) paste(nam[c(i,i+K/2)], collapse =" = "))
  ## check
  nam_check_1 <- purrr::map(stringr::str_split(H1_char, " = "), \(str) lapply(stringr::str_split(str, ":"), \(i) i[[1]]) %>% unlist())
  if(!all(map_int(nam_check_1, dplyr::n_distinct)==1)) {
    stop("Problem with creating hypo H1, vector is: ", H1_char[1])
  }

  ## H2: join tests
  H_pairs <- sapply(1:(K/2), \(i) paste(nam[c(i,i+K/2)], collapse =" - "))
  H2_char <- paste(H_pairs[-1],"=",  H_pairs[1])

  test_joint <- car::linearHypothesis(reg_out, hypothesis.matrix = H2_char)
  test_indiv <- purrr::map_dfr(H1_char, \(i) car::linearHypothesis(reg_out, hypothesis.matrix = i) %>%
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

  ##
  df <- sim_dat_common(N=100, Time=10,
                         timing_treatment = 6, perc_treat=0.5)
  df


  ##
  time.index = quo(Time)
  y_var = quo(y)
  mdd_test_pre_trend_means(data=df, treat = tr, time.index = Time,
                           y_var = y)
  ## make sure vars exist!?
  mdd_test_pre_trend_means(data=df, treat = tr, time.index = Timesakjdhfjshfskjshfkjdshfjhszf,
                           y_var = y)

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
