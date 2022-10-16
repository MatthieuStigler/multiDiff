test_pre_trend <- function(df, treat_var, year_var, cluster=NULL){

  ## prep data
  dat_prep <- df %>%
    rename(treat_here ={{treat_var}},
           year_here = {{year_var}}) %>%
    mutate(treat_here =as.factor(treat_here),
           year_here = as.factor(year_here))

  ##
  reg_out <- feols(y~ -1+treat_here:year_here,
                   cluster = cluster,
                   data=dat_prep)

  nam <- names(coef(reg_out))
  nam_split <- str_split(nam, ":")
  if(!all(map_int(nam_split, length)==2)) stop("Problem with names... contains a `:`?")
  K <- length(nam)
  co_tab <- tibble(co=nam, first = map_chr(nam_split, 1), sec=map_chr(nam_split, 2))
  co_tab %>%
    pread()
  treat_status <- str_extract(nam, "treat_here.+(?=:)")

  H_which <- which(str_detect(nam, "TRUE"))
  treat_status
  H1_char <- paste(paste(nam[H_which], collapse = " + "), "=0")
  H2_char <- paste(nam[H_which], "=0")

  test_sum <- linearHypothesis(reg_out, hypothesis.matrix = H1_char)
  test_indiv <- linearHypothesis(reg_out, hypothesis.matrix = H2_char)

  ## assemble
  rbind(test_sum %>% tidy() %>%
          distinct(statistic, p.value),
        test_indiv %>% tidy() %>%
          distinct(statistic, p.value)) %>%
    mutate(test = c("sum", "indiv")) %>%
    relocate(test)
}

if(FALSE){
  library(fixest)
  library(tidyverse)
  data <- sim_dat_staggered(N=100, perc_always = 0, Time=10,
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


  ### Full
  treat_var = quo(type)
  year_var <- quo(Time)
  test_pre_trend(df=data_modif, )
}
