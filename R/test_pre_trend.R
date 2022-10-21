#' Conduct parallel pre-trends test
#'
#' @param data data-frame, containing only pre-intervention values
#' @param treat.group.index the variable in `data` containing the post-intervention treatment status
#' @template param_y_var
#' @template param_time.index
#' @param cluster argument passed to `feols(..., cluster=)`
#'
#' @examples
#'
#' data <- multiDiff::sim_dat_staggered(N=100, perc_always = 0, Time=10,
#'                                      timing_treatment = 6, perc_treat=0.5)
#'  library(tidyverse, warn.conflicts = FALSE)
#'  data_modif <- data %>%
#'    group_by(unit) %>%
#'    mutate(type = if_else(any(tr==1), "Treat", "Control")) %>%
#'    ungroup() %>%
#'    select(y, type, Time, unit)
#'
#' #
#' test_pre_trend(data_modif, treat.group.index=type,
#'                time.index=Time, cluster = "unit")
#'
#'@export
test_pre_trend <- function(data, treat.group.index, time.index, y_var = "y",
                           cluster=NULL){

  ## prep data
  dat_prep <- data %>%
    rename(treat_here ={{treat.group.index}},
           year_here = {{time.index}},
           y= {{y_var}}) %>%
    mutate(treat_here =as.factor(.data$treat_here),
           year_here = as.factor(.data$year_here))

  ##
  reg_out <- fixest::feols(y~ -1+year_here:treat_here,
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
  H2_char <- str_replace(H1_char, " = ", " - ") %>%
    paste(collapse = " + ") %>%
    paste("=0")

  test_joint_or <- car::linearHypothesis(reg_out, hypothesis.matrix = H1_char)
  test_joint_and <- car::linearHypothesis(reg_out, hypothesis.matrix = H2_char)
  test_indiv <- purrr::map_dfr(H1_char, \(i) car::linearHypothesis(reg_out, hypothesis.matrix = i) %>%
                          broom::tidy())

  ## assemble
  rbind(test_joint_or %>% broom::tidy() %>%
          distinct(.data$statistic, .data$p.value) %>%
          mutate(term = paste(H1_char, collapse = " OR ")),
        test_joint_and %>% broom::tidy() %>%
          distinct(.data$statistic, .data$p.value) %>%
          mutate(term = H2_char),
        test_indiv %>%
          distinct(.data$term, .data$statistic, .data$p.value)) %>%
    mutate(test = c("test_joint_or", "test_joint_and", rep("test_indiv", nrow(test_indiv)))) %>%
    dplyr::relocate(.data$test)
}

if(FALSE){

  ##
  Year <- rep(1:10, times=400)
  Group <- rep(c("Control", "Treat"), each = 2000)

  set.seed(123)
  y_same <- as.numeric(as.factor(Year))+
    rnorm(length(Group))
  y_diff <- y_same   +
    ifelse(Year==5 & Group =="Treat",rnorm(mean=0.5, length(Group)),0)

  df <- data.frame(Year, Group, y_same, y_diff)

  ##
  test_pre_trend(data=df, treat.group.index = Group, time.index = Year,
                 y_var = y_same)
  test_pre_trend(data=df, treat.group.index = Group, time.index = Year,
                 y_var = y_diff)

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
  test_pre_trend(df=data_modif, cluster=NULL)
}
